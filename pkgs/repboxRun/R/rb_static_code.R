rb_update_static_code_analysis = function(rb, overwrite=FALSE, lang=c("stata","r"), opts=rb$opts) {
  restore.point("rb_update_static_code_analysis")

  project_dir = rb$project_dir
  parcels = rb$parcels

  if ("stata" %in% lang & (overwrite | !rb_has_static_code_analysis(rb,"stata"))) {
    rb_log_step_start(rb, "static_code_stata")
    cat("\n  Stata static code analysis...\n\n")
    parcels = repbox_stata_static_parcel(project_dir, parcels=parcels, opts=opts)
    parcels = repboxCodeText::code_project_find_refs(project_dir, parcels=parcels)

    rb_log_step_end(rb, "static_code_stata")
  }
  if ("r" %in% lang & (overwrite | !rb_has_static_code_analysis(rb,"r"))) {
    rb_log_step_start(rb, "static_code_r")
    cat("\n  R static code analysis...\n\n")
    parcels = repboxR::repbox_project_static_analyse_r(project_dir,parcels=parcels, opts=opts$r_opts)
    rb_log_step_end(rb, "static_code_r")
  }


  rb$parcels = parcels
  rb
}

rb_has_static_code_analysis = function(rb, lang="stata", project_dir=rb$project_dir) {

  if (lang=="stata") {
    return(file.exists(file.path(project_dir, "repdb/stata_cmd.Rds")))
  } else if (lang=="r") {
    return(file.exists(file.path(project_dir, "repdb/r_cmd.Rds")))
  }
  stop(paste0("unknown language lang=", lang))
}
