repboxRun::repbox_load_libs()

# Should point to this project dir
project_dir = rb_get_project_dir("{{project_dir}}")
run_test_file =
report_file = file.path(project_dir, "test_report/test_report.Rmd")
overwrite = {{overwrite}}

if (FALSE) {
  options(warn=1)
  opts=mrb_test_opts(data_head_rows = 30, show_org_data=FALSE, show_pre_reg_data = FALSE, data_width=100, max_cases = 1, data_add_org_row=TRUE)
  metaregBase::mrb_run_as_test(project_dir = project_dir,file.path(project_dir, "run/run_mrb_test.R"), opts=opts)
  rstudioapi::navigateToFile(file.path(project_dir, "test_report/test_report.Rmd"))
  rstudioapi::filesPaneNavigate(project_dir)

  # Extra debugging info for a given runid (if report stopped due to error)
  if (FALSE)
    mrb_runid_test_files(project_dir, runid=127)
}

rb = rb_new(project_dir,forced_fail_action = "msg")
if (FALSE) { # don't rerun by default previous data repbox steps
  rb = rb_update_file_info_parcel(rb, overwrite=overwrite,assume_org_complete = TRUE)
  rb = rb_update_script_parcels(rb, overwrite=overwrite)
  rb = rb_update_static_code_analysis(rb, overwrite = overwrite)

  rb = rb_create_mod_dir(rb)
  rb = rb_run_stata_reproduction(rb, overwrite=overwrite)
}


drf = repboxDRF::drf_load(project_dir)
mrb = mrb_init(project_dir)
if (!FALSE) {
  mrb = mrb_full_stata_script(mrb)
  mrb_clear_stata_reg_out(project_dir)
  mrb = mrb_run_stata_script(mrb)
  mrb = mrb_agg_stata(mrb)
}
mrb = mrb_run_r_base(mrb)
mrb = mrb_run_r_reg(mrb)

