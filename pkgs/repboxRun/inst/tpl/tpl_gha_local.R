# If GHA is not available and you want to run the part done by run_gha.R locally
# on your server

repboxRun::repbox_load_libs()

project_dir = rb_get_project_dir("{{default_project_dir}}")
overwrite = {{overwrite}}

if (FALSE)
  rstudioapi::filesPaneNavigate(project_dir)

rb = rb_new(project_dir)
drf_clear_r_err_runids(project_dir)
rb = rb_run_stata_reproduction_raw(rb, overwrite=overwrite)
