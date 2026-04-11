# Functions for 2nd reproduction run in Stata that extracts systematic
# regression info and stores intermediate data sets for metareg replication

repbox_reg_repro_opts = function(store_data_caches = TRUE) {
  as.list(environment())
}

rb_has_mr_base = function(rb, project_dir = rb$project_dir) {
  file.exists(file.path(project_dir, "repdb", "reg_rb.Rds"))
}


rb_run_mr_base = function(rb, overwrite = FALSE) {
  restore.point("rb_run_mr_base")
  library(metaregBase)

  rb_log_step_start(rb, "mr_base")
  project_dir = rb$project_dir

  if (!rb_has_stata(rb)) {
    cat("\nThe reproduction package has no Stata scripts.\n")
    return(rb)
  }

  if (!rb_has_stata_postprocess(rb)) {
    cat("\nNo (postprocessed regression) results for Stata reproduction run found.\n")
    return(rb)
  }

  if (!overwrite) {
    if (rb_has_mr_base(rb)) {
      cat("\nBase Metareg (mr_base) results already exist. Thus skipped.\n")
      return(rb)
    }
  }

  cat("\nBase Metareg\n")
  opts = rb$opts
  drf = rb[["drf"]]
  if (is.null(drf)) drf = repboxDRF::drf_load(project_dir)

  res = metaregBase::mrb_run_all(project_dir = project_dir, drf=drf)

  rb_log_step_end(rb, "mr_base")
  rb
}
