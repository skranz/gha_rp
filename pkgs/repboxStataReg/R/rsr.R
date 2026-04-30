example = function() {
  library(repboxStataReg)
  # Should point to this project dir
  overwrite=TRUE
  project_dir = "~/repbox/projects/aejapp_11_2_10"
  parcels = list()

  if (FALSE)
    rstudioapi::filesPaneNavigate(project_dir)

  rsr_make_reg_info(project_dir, overwrite=TRUE)
}

# Stores cmdpart parcels and internal reg info in repbox/stata dir
# we cannot yet store any regcoef etc parcels
# since that requires metaregBase semantic parsing with data set info
rsr_make_reg_info = function(project_dir, overwrite=FALSE, parcels=list(), repbox_results=repbox_load_internal_repbox_results(project_dir)) {
  restore.point("rsr_make_reg_info")
  parcels = repdb_load_parcels(project_dir, "stata_run_cmd")
  run_df = repboxDRF::repbox_get_run_df(project_dir = project_dir, parcels=parcels)
  parcels = rsr_make_cmdpart_parcels(project_dir = project_dir, overwrite=overwrite, run_df=run_df)
  rsr_make_stata_reg_run_info_parcel(project_dir, parcels=parcels,repbox_results=repbox_results)
  regtab = rsr_extract_stata_reg_output(project_dir)

  parcels
}

rsr_make_cmdpart_parcels = function(project_dir,  overwrite=FALSE, parcels=list(), run_df = repboxDRF::repbox_get_run_df(project_dir = project_dir, parcels=parcels)) {
  restore.point("rsr_make_cmdpart_parcels")

  if (!overwrite) {
    if (repboxDB::repdb_has_parcel(project_dir, "reg_cmdpart")) {
      return(parcels)
    }
  }

  repdb_dir = file.path(project_dir, "repdb")

  rows = which(run_df$cmd_type %in% c("reg"))
  cp_df = cmdparts_of_stata_reg(run_df$cmdline[rows]) %>%
    arrange(str_row)
  cp_df$runid = run_df$runid[rows[cp_df$str_row]]
  parcels[["reg_cmdpart"]] = cp_df

  repboxDB::repdb_save_parcels(parcels[c("reg_cmdpart")],dir=repdb_dir)

  rows = which(run_df$cmd_type %in% c("quasi_reg"))
  if (length(rows)==0) return(parcels)

  cp_df = cmdparts_of_stata_reg(run_df$cmdline[rows]) %>%
    arrange(str_row)
  cp_df$runid = run_df$runid[rows[cp_df$str_row]]
  parcels[["quasi_reg_cmdpart"]] = cp_df
  repboxDB::repdb_save_parcels(parcels[c("quasi_reg_cmdpart")],dir=repdb_dir)

  parcels
}


# Only saves run_info after regression run
# Regression information will be stored after metareg run
rsr_make_stata_reg_run_info_parcel = function(project_dir, parcels=list(), repbox_results=repbox_load_internal_repbox_results(project_dir)) {
  restore.point("repbox_save_stata_reg_run_parcels")

  parcels = repdb_load_parcels(project_dir, c("stata_run_info","stata_file"), parcels=parcels)
  artid = basename(project_dir)

  res = repbox_results

  # No Stata run results stored
  if (is.null(res)) {
    return(parcels)
  }

  dotab = res$dotab
  dotab$file_path = str.right.of(dotab$file, paste0("/",artid,"/mod/"))

  if (!has.col(res$tab,"in_loop")) {
    cat("\nNote there was no 'in_loop' col in repbox_results$tab\n")
    res$tab$in_loop = NA_integer_
  }

  run_df = res$run.df %>%
    mutate(
      artid = artid,
      found_path = file_path_relative_to_supp(foundfile, paste0("/", artid, "/mod/"),wdir = wdir, supp.dir = paste0(project_dir, "/", artid, "/mod/"))
    ) %>%
    rename(

      start_time = stime,
      end_time = etime,
      errcode = runerrcode,
      errmsg = runerrmsg,
      out_img_file = out.img.file
    ) %>%
    left_join(select(dotab, donum, file_path), by="donum") %>%
    left_join(select(dotab, rootdonum = donum, root_file_path = file_path), by="rootdonum")

  run_df$runid = seq_len(NROW(run_df))
  script_df = parcels$stata_file
  run_df = left_join(run_df, select(script_df, file_path, script_num), by="file_path")

  # Don't save detailed run results after reg run

  #repdb_check_data(run_df,"stata_run_cmd")
  #repdb_check_data(run_df,"stata_run_log")

  #parcels$stata_run_cmd = run_df
  #parcels$stata_run_log = run_df

  run_info = run_df %>%
    summarize(
      artid = first(artid),
      runtype = "reg",
      runs = n(),
      ok_runs = sum(is.true(has.data & !runerr)),
      no_dat_runs = sum(!is.true(has.data)),
      err_runs = sum(runerr & is.true(has.data)),
      reg_runs = sum(is.regcmd),
      no_dat_reg_runs = sum(is.regcmd & !is.true(has.data)),
      err_reg_runs = sum(is.regcmd & runerr & is.true(has.data)),
      ok_reg_runs = reg_runs-err_reg_runs-no_dat_reg_runs,
      start_time = first(start_time[!is.na(start_time)]),
      end_time = last(end_time[!is.na(end_time)]),
      run_sec = time.diff(start_time, end_time)
    )






  org_run_info = parcels$stata_run_info

  if (NROW(org_run_info)==0) {
    if (run_info$err_runs >0) {
      repbox_problem(msg = paste0("The Stata run had ", run_info$err_runs, " commands that threw an error"), type = "stata_err",fail_action = "msg")
    }
    if (run_info$no_dat_runs >0) {
      repbox_problem(msg = paste0(run_info$no_dat_runs, " of the ", run_info$runs, " run commands had a missing data set."), type = "stata_no_dat",fail_action = "msg")
    }

  } else {
    if (run_info$err_runs > org_run_info$err_runs) {
      repbox_problem(msg = paste0("The Stata run which stored regression information had more errors than the original run."), type = "stata_reg_err",fail_action = "msg")
    } else if (run_info$runs != org_run_info$runs) {
      repbox_problem(msg = paste0("The Stata run which stored regression information executed a different number of commands than the original run."), type = "stata_reg_runs_diff",fail_action = "msg")
    } else if (run_info$err_runs < org_run_info$err_runs) {
      repbox_problem(msg = paste0("The Stata run  which stored regression information had fewer errors than the original run."), type = "stata_reg_err",fail_action = "msg")
    }
  }
  parcels$stata_reg_run_info = run_info

  repdb_save_parcels(parcels[c("stata_reg_run_info")], file.path(project_dir, "repdb") )
  invisible(parcels)
}
