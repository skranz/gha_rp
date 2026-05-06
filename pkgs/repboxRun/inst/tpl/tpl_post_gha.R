# Initializes the repbox project for a given article / reproduction package
# Essentially only copies all files, does not yet run any reproduction

repboxRun::repbox_load_libs()

project_dir = rb_get_project_dir("{{default_project_dir}}")

if (FALSE)
  rstudioapi::filesPaneNavigate(project_dir)

overwrite = {{overwrite}}

run_all_post_gha = function() {
  if (!rb_has_ok_gha_run(project_dir))
    stop("\nNo successful gha_run omit further analysis.")

  # Install SUP ZIP
  cat("\nCreate org folder if missing.\n")
  rb_extract_zip_to_org(project_dir=project_dir, overwrite=overwrite)



  rb = rb_new(project_dir, just_steps=NULL, ignore_steps=NULL)

  # Static code analysis
  cat("\nrb_update_static_code_analysis.\n")
  rb = rb_update_static_code_analysis(rb, overwrite=overwrite)


  # Postprocess regression results

  cat("\nrb_postprocess_stata_reproduction.\n")
  rb = rb_postprocess_stata_reproduction(rb=rb,overwrite = overwrite)


  # Metareg base study
  cat("\nrb_run_mr_base\n")
  rb = rb_run_mr_base(rb, overwrite=overwrite)

  # Write reports
  rb_report_md(project_dir=project_dir)
  library(repboxReportDo)
  repboxReportDo::rrd_html_do(project_dir, parcels=rb$parcels)
  #repboxReportDo::rrd_plain_do(project_dir, parcels=mrb$parcels)

}

run_all_post_gha()
