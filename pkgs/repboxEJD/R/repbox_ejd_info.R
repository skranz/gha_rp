example = function() {
	library(EconJournalScrap)
  project_dir = "/home/rstudio/repbox/gha_ejd/projects_ejd_gha2/aejapp_16_2_11"
	artid = basename(project_dir)
	arts = ejs_load_agg("art")
	art = arts[arts$artid==artid,]

  add_new_ejd_project_infos(parent_dir = c("~/repbox/gha_ejd/projects_ejd_gha2","~/repbox/projects_gha","~/repbox/projects_gha_new","~/repbox/projects_gha_old2", "~/repbox/gha_ejd/projects_ejd_gha"), overwrite = TRUE)
}


# Generate project info in old EJD format.
# At some point in the future, we should generate a new EJD info format
make_ejd_project_info = function(project_dir, art) {
  restore.point("make_ejd_project_info")

  #rstudioapi::filesPaneNavigate(project_dir)
  artid = basename(project_dir)
  parcels = repdb_load_parcels(project_dir, c("stata_do_run_info","art"))

  art$authors = parcels$art$art$authors

  do_info = parcels$stata_do_run_info$stata_do_run_info
  if (NROW(do_info)==0) return(NULL)

  do_info = repdb_null_to_empty(do_info,"stata_do_run_info")

  do_info$artid = rep(artid, NROW(do_info))

  run_info = do_info %>%
    group_by(artid) %>%
    summarize(
      num_do = n(),
      across(is_included:has_parse_err, ~sum(.,na.rm=TRUE))
    ) %>%
    ungroup() %>%
    mutate(
      project_dir = project_dir
    )

  #cat(paste0(colnames(run_info), collapse=", "))
  #num_do, is_included, does_include, runtime, analyzed, timeout, was_run, num_runs, num_runs_err, num_load_data, num_load_data_err, has_parse_err
#
#
#   run_info = run_info %>% transmute(
#     artid = artid,
#     project_dir = project_dir,
#     project = artid,
#     num.tables = NA,
#     do.files = num_do,
#     data.existing = NA,
#     data.missing = NA,
#     data.intermediate = NA,
#     data.existing.mb = NA,
#     share.runs.no.error = ifelse(is.true(num_runs>0), (num_runs-num_runs_err)/num_runs,0),
#     size.mb = NA,
#     stata.time = NA,
#     stata.res.mb = NA,
#     r.time = NA,
#     r.res.mb = NA,
#     runtime = runtime,
#     analyzed = analyzed,
#     timeout = is.true(timeout>0),
#     parse.err = is.true(has_parse_err>0),
#     runs = num_runs,
#     runs.with.data = NA,
#     runs.no.data = NA,
#     runs.err = num_runs_err,
#     runs.err.with.data = NA,
#     rruns = NA,
#     rruns.with.data = NA,
#     rruns.no.data = NA,
#     rruns.err = NA,
#     rruns.err.with.data = NA,
#     matched = NA,
#     norun.reg.lines = NA,
#     complete.data = NA,
#     project.dir = project_dir,
#     tabs = NA,
#     share.full.match.tabs = NA,
#     not.full.match.tabs = NA,
#     tab.match.shares = NA,
#     reg.tabs = NA,
#     alltype.tabs = NA,
#     unknown.tabs = NA
#   )
  #info = cbind(art, run_info)
  cat("\nAdded Stata run info to ", run_info$artid, "\n")

  cols = c("mb_zip","mb_unzip","mb")
  run_info[cols] = art[cols]

  #setdiff(names(info_df), names(info))
  run_info
}


# By default adds the project infos
add_new_ejd_project_infos = function(info.file = "~/ejd_files/agg/repbox_ejd_infos.Rds", project_dirs = NULL, parent_dir = NULL, overwrite=FALSE) {
  restore.point("add_new_ejd_project_infos")
  if (is.null(project_dirs) & !is.null(parent_dir)) {
    li = lapply(parent_dir,list.dirs, recursive=FALSE)
    project_dirs = unlist(li)
    names(project_dirs) = NULL
  }
  points = -seq_along(project_dirs)/(length(project_dirs)+1)

  exist_files = c("reports/report_ejd/index.html", "repdb/stata_do_run_info.Rds")
  for (i in seq_along(exist_files)) {
    files = file.path(project_dirs, exist_files[i])
    points = points + 10*i*file.exists(files)
  }
  new_proj = data.frame(artid = basename(project_dirs), project_dir = project_dirs, points=points) %>%
    arrange(desc(points)) %>%
    filter(!duplicated(artid)) %>%
    filter(points > 0)

  project_dirs = new_proj$project_dir

  library(EconJournalScrap)
  old_info_df = readRDS(info.file) %>%
    filter(!is.na(artid)) %>%
    filter(!duplicated(artid))


  if (!overwrite) {
    project_dirs = project_dirs[!basename(project_dirs) %in% old_info_df$artid]
  } else {
    old_info_df = old_info_df[!old_info_df$artid %in% basename(project_dirs),]
  }

  i = 1
  arts = EconJournalScrap::ejs_load_agg("art")
  inner.fun = function(i) {
    project_dir = project_dirs[i]
    cat("\n",i,project_dir)
    artid = basename(project_dir)
    art = arts[arts$artid==artid,]
    if (NROW(art)==0) return(NULL)
    res = try(make_ejd_project_info(project_dir, art))
    #res = make_ejd_project_info(project_dir, art)
    if (is(res,"try-error")) return(NULL)
    if (isTRUE(is.na(res$artid))) {
      restore.point("NA artid")
      stop()
    }
    return(res)
  }

  inds = seq_along(project_dirs)
  #inds = 1:10
  new_info_li = lapply(inds, inner.fun)
  new_info_df = bind_rows(new_info_li)

  which(is.na(new_info_df$artid))
  restore.point("add_new_ejd_project_infos_post_run")

  info_df = bind_rows(new_info_df, old_info_df)
  which(is.na(info_df$artid))
  saveRDS(info_df, info.file)
  invisible(info_df)
}
