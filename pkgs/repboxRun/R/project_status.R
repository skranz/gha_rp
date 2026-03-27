example = function() {
  project_dir = "/home/rstudio/repbox/projects_gha_new/aejapp_1_2_4"
  if (FALSE)
    rstudioapi::filesPaneNavigate(project_dir)

  status = repbox_project_status(project_dir)
}

# A central function to return the general status of a project,
# i.e. which steps have been run (successful or not) and when
# and which files are there
repbox_project_status = function(project_dir, add_fp=FALSE, add_repdb = TRUE) {
  restore.point("repbox_project_status")
  # project status will be a data.frame
  # the complete status shall consist of different rbs
  rb_li = vector("list", 100)
  rb_ind = 0
  project_dir = normalizePath(project_dir)
  fp = function(file) {
    file = normalizePath(file,mustWork = FALSE)
    ifelse(startsWith(file, project_dir), file, paste0(project_dir, "/", file))
  }
  has_file = function(...) {
    file.exists(fp(...))
  }
  add_rb = function(rbid,has=if (!is.na(val)) TRUE else NA, timestamp = NA_POSIXct_,  val=NA_real_, mb=NA_real_, help="", warn="", ...) {
    rb = data.frame(rbid,has, timestamp,val,mb, help, ...)
    rb_ind <<- rb_ind +1
    rb_li[[rb_ind]] <<- rb
    rb
  }


  add_file_rb = function(rbid, file,mb = file.size(file) / 1e6, ...) {
    file = fp(file)

    add_rb(rbid, has = file.exists(file), timestamp=file.mtime(file),mb = mb, ...)
  }
  add_dir_rb = function(rbid, dir, first=FALSE,...) {
    restore.point("add_dir_rb")
    if (length(dir)==0) {
      add_rb(rbid, has = FALSE, ...)
      return()
    }
    if (first) dir = dir[1]
    if (!startsWith(dir,"/") & ! startsWith(dir,"~"))
      dir = file.path(project_dir, dir[1])
    files = list.files(dir, recursive = TRUE)
    if (length(files)==0) {
      add_rb(rbid, has = FALSE, ...)
      return()
    }
    timestamp = max(file.mtime(dir))
    add_rb(rbid, has=TRUE, timestamp = timestamp, ...)
  }


  # meta information
  add_file_rb("meta_art","meta/art_meta.Rds")
  add_file_rb("meta_sup","meta/sup_meta.Rds")

  # infos on supplement files
  fi_status = project_org_files_status(project_dir)
  add_rb("org_dir_complete", has = fi_status$org_dir_complete, mb = fi_status$org_dir_mb, val=fi_status$org_dir_num_files, help="Does /org contain all files of the reproduction package?")
  add_rb("sup_file_info", has = fi_status$has_file_info, mb = fi_status$fi_mb, val=fi_status$fi_num_files, help="Originally stored info about the files in the reproduction package. mb is total size of unzipped supplement.")



  # repboxDoc
  library(repboxDoc)
  doc_dirs = repboxDoc::repbox_doc_dirs(project_dir)
  doc_types = repboxDoc::rdoc_type(doc_dirs)
  doc_forms = repboxDoc::rdoc_form(doc_dirs)

  has_doc_app = length(app_prefix)>0


  add_rb("doc_art",has=any(doc_types == "art"))
  add_rb("doc_app",has=any(doc_types != "art"))
  add_dir_rb("doc_art_pdf",doc_dirs[doc_types == "art" & doc_forms=="pdf"])
  add_dir_rb("doc_art_mocr",doc_dirs[doc_types == "art" & doc_forms=="mocr"])
  add_dir_rb("doc_art_html",doc_dirs[doc_types == "art" & doc_forms=="html"])
  add_dir_rb("doc_app_mocr",doc_dirs[doc_types != "art" & doc_forms=="mocr"])


  # general repbox info
  add_file_rb("stata_results","repbox/stata/repbox_results.Rds", help="Does file repbox/stata/repbox_results.Rds exist?")
  add_dir_rb("stata_cmd_log","repbox/stata/cmd", help="Does any file in repbox/stata/cmd exist?")

  # metareg base info
  if (has_file("metareg/base")) {
    add_dir_rb("mr_base_dir","metareg/base")

    r_runtime = stata_runtime = NA_real_
    if (has_file("metareg/base/runtimes.Rds")) {
      rt = readRDS(fp("metareg/base/runtimes.Rds"))
      r_runtime = rt$total$r_runtime
      stata_runtime = rt$total$stata_runtime
    }
    add_rb("mr_base_r_runtime", val=r_runtime)
    add_rb("mr_base_stata_runtime", val=stata_runtime)

    num_infeasible_steps = length(list.files(fp("metareg/base/step_results"), glob2rx("infeasible*.Rds")))
    num_reg_results = length(list.files(fp("metareg/base/step_results"), glob2rx("infeasible*.Rds")))

    add_rb("mr_base_num_infeasible_steps", val=num_infeasible_steps)
    add_rb("mr_base_num_reg_results", val=num_reg_results)
  } else {
    add_rb("mr_base_dir", has=FALSE)
  }

  # metareg DAP info
  # if metareg/base exits, also a DAP should exist
  if (has_file("metareg/dap/stata/dap.Rds")) {
    add_file_rb("dap","metareg/dap/stata/dap.Rds")
    cache_files = list.files(fp("metareg/dap/stata/cache"), glob2rx("*.dta"))
    add_rb("dap_cache", has=length(cache_files)>0, mb=sum(file.size(cache_file) / 1e6), val=length(cache_files), help="MB and number of cached data sets in DAP for replications of regressions in Stata code." )

    cache_files = list.files(fp("metareg/dap/stata/extra_cache"), glob2rx("*.dta"))
    add_rb("dap_extra_cache", has=length(cache_files)>0, mb=sum(file.size(cache_files) / 1e6), val=length(cache_files), help="Extra cache files are generated if translated R code was not able to reproduce correct data sets for regressions. Ideally, we don't need them." )
  } else {
    add_rb("dap", has=FALSE)
  }


  # readme
  add_rb("readme_dir",has=has_file("readme"))
  add_file_rb("readme_ranks","readme/readme_ranks.Rds")
  add_dir_rb("readme_txt","readme/txt")

  # gha
  if (has_file("gha/gha_status.txt")) {
    add_rb("gha", has = TRUE, timestamp = file.mtime(fp("gha/gha_status.txt")), status=merge.lines(readLines(fp("gha/gha_status.txt"))),help="Was run on Github Actions")
  } else {
    add_rb("gha", has = FALSE, help="Was not run on Github Actions")
  }

  # reports
  add_file_rb("report_do_tab","reports/do_tab.html")

  # fp
  if (add_fp) {
    types = "art"
    if (has_doc_app) type = c(types, "app")


    for (type in types) {
      library(FuzzyProduction)
      app_types = setdiff(unique(doc_types),"art")

      type = "art"
      doc_type = type
      if (type=="app") doc_type = app_types
      fp_dir =paste0(fp("fp/prod_"),doc_type)
      ver_dirs = FuzzyProduction::fp_all_ok_ver_dirs(fp_dir)
      fp_df = FuzzyProduction::fp_ver_dir_to_ids(ver_dirs)

      fp_df$timestamp = file.mtime(file.path(fp_df$ver_dir, "prod_df.Rds"))
      p_df = fp_df %>%
        group_by(prod_id) %>%
        summarize(
          timestamp = max(timestamp, na.rm=TRUE),
          val = n()
        )
      if (NROW(p_df)>0) {
        add_rb(paste0("fp_prod_", p_df$prod_id),has=TRUE, timestamp = p_df$timestamp, val=p_df$val, help="val: no. versions")
      }
      p_df = fp_df %>%
        group_by(prod_id, proc_id) %>%
        summarize(
          timestamp = max(timestamp, na.rm=TRUE),
          val = n()
        )
      if (NROW(p_df)>0) {
        add_rb(paste0("fp_proc_",p_df$prod_id,"-", p_df$proc_id),has=TRUE, timestamp = p_df$timestamp, val=p_df$val, help="val: no. versions")
      }
    }
  }


  if (add_repdb) {
    repdb_files = list.files(fp("repdb"),glob2rx("*.Rds"), full.names = TRUE)
    timestamp = file.mtime(repdb_files)
    parcel = basename(repdb_files) %>% tools::file_path_sans_ext()
    add_rb(paste0("repdb_", parcel), has=TRUE, timestamp=timestamp, mb = file.size(repdb_files) / 1e6)
  }


  status = bind_rows(rb_li)
}

project_org_files_status = function(project_dir) {
  restore.point("project_org_files_status")

  fi_meta = read_rds_or_null(file.path(project_dir,"meta/sup_files.Rds"))
  has_meta_fi = !is.null(fi_meta)

  has_repbox_fi = file.exists(file.path(project_dir,"repbox/org_files.Rds"))
  if (is.null(fi_meta)) {
    fi_meta = read_rds_or_null(file.path(project_dir,"repbox/org_files.Rds"))
  }
  if (!has_col(fi_meta, "mb") & has_col(fi_meta,"size")) {
    fi_meta$mb = fi_meta$size / 1e6
  }

  org_dir = file.path(project_dir,"org")
  files = list.files(org_dir,recursive = TRUE,include.dirs = FALSE)
  fi = as.data.frame(file.info(files))
  rownames(fi) = NULL
  fi$file = files
  fi$base = basename(files)
  fi$ext = tools::file_ext(files)

  na_if_null = function(x) {
    if (is.null(x)) return(NA)
  }

  res = data.frame(has_file_info = !is.null(fi_meta), fi_mb = sum(fi_meta$mb), fi_num_files = NROW(fi_meta), has_org_dir = dir.exists(org_dir), org_dir_mb = sum(fi_meta$size) /1e6, org_dir_num_files = NROW(fi), has_meta_fi = has_meta_fi, has_repbox_fi = has_repbox_fi)


  if (is.null(fi_meta)) {
    res$fi_mb =  res$fi_num_files = NA
  }

  res$org_dir_complete = case_when(
    !has_org_dir ~ FALSE,
    is.na(res$fi_num_files) | isTRUE(res$fi_num_files==0) ~ NA,
    isTRUE(res$fi_num_files == res$org_dir_num_files) ~ TRUE,
    TRUE ~ FALSE
  )
  res = res[unique(c("org_dir_complete", names(res)))]
  res
}
