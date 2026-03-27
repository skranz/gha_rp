# Functions for 2nd reproduction run in Stata that extracts systematic
# regression info and stores intermediate data sets for metareg replication

repbox_reg_repro_opts = function(store_data_caches = TRUE) {
  as.list(environment())
}

rb_has_mr_base = function(rb, project_dir = rb$project_dir) {
  return(FALSE)
  file.exists(file.path(project_dir, "repdb/stata_reg_run_info.Rds"))
}


rb_run_mr_base = function(rb, overwrite = FALSE) {
  restore.point("rb_run_stata_reg_repro")
  library(metaregBase)
  rb_log_step_start(rb, "stata_reg_repro")
  project_dir = rb$project_dir

  if (!rb_has_stata(rb)) {
    cat("\nThe reproduction package has no Stata scripts.\n")
    return(rb)
  }

  if (!rb_has_stata_reg_repro(rb)) {
    cat("\nNo results for the required 2nd Stata reproduction run (reg_repro) found.\n")
    return(rb)
  }

  if (!overwrite) {
    if (rb_has_mr_base(rb)) {
      cat("\nBase Metareg (mr_base) results already exist. Thus skipped.\n")
      return(rb)
    }
  }

  cat("\nBase Metareg\n")
  rb_log_step_start(rb, "mr_base")
  opts = rb$opts
  res = mr_base_run_study(project_dir, stop.on.error = opts$stop.on.error,create.repdb = TRUE,stata_version = opts$stata_version)
  rb_log_step_end(rb, "mr_base")



}
