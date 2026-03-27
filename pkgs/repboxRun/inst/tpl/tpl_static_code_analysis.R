# Performs static analysis of Stata and R code
library(repboxRun)

# Should point to this project dir
project_dir = rb_get_project_dir("{{default_project_dir}}")

if (FALSE)
  rstudioapi::filesPaneNavigate(project_dir)

rb = rb_new(project_dir)

rb = rb_update_static_code_analysis(rb, overwrite=!FALSE)
