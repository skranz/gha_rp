# Perform reproduction run of Stata code
#
# Create logs for each command
# and if store_reg_info=TRUE also stores
# special regression information

repboxRun::repbox_load_libs()

project_dir = rb_get_project_dir("{{default_project_dir}}")

if (FALSE)
  rstudioapi::filesPaneNavigate(project_dir)
rb_run_gha_stata_reproduction(project_dir, overwrite=overwrite)


