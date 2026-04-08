# Initializes the repbox project for a given article / reproduction package
# Essentially only copies all files, does not yet run any reproduction

library(repboxRun)
project_dir = rb_get_project_dir("{{default_project_dir}}")

if (FALSE)
  rstudioapi::filesPaneNavigate(project_dir)


run_all_post_gha = function() {
  if (!file.exists("gha_log/gha_ok.log"))
    stop("\nNo successful gha_run omit further analysis.")

  # Install SUP ZIP
  rb_extract_zip_to_org(project_dir=project_dir, overwrite=FALSE)


  rb = rb_new(project_dir, just_steps=NULL, ignore_steps=NULL)

  # Postprocess
  rb_postprocess_stata_reproduction(rb=rb)

  # Static code analysis
  rb = rb_update_static_code_analysis(rb, overwrite=FALSE)

  # Metareg base study
  rb = rb_run_mr_base(rb, overwrite=FALSE)


}

