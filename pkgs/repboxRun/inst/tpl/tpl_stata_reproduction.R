# Perform reproduction run of Stata code
#
# Create logs for each command
# and if store_reg_info=TRUE also stores
# special regression information

run_stata_reproduction = function(rb=NULL) {
  library(repboxRun)
  # Should point to this project dir
  project_dir = rb_get_project_dir("{{default_project_dir}}")

  if (FALSE)
    rstudioapi::filesPaneNavigate(project_dir)

  rb = rb_new(project_dir)

  rb = rb_run_stata_reproduction(rb, overwrite=!FALSE, store_reg_info=TRUE)

  rb
}
