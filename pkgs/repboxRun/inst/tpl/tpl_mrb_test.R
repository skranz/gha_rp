repboxRun::repbox_load_libs()

# Should point to this project dir
project_dir = rb_get_project_dir("{{project_dir}}")
overwrite = {{overwrite}}

if (FALSE) {
  options(warn=1)
  opts=mrb_test_opts(data_head_rows = 10, show_org_data=FALSE, show_pre_reg_data = FALSE, data_width=100, max_cases = 10, data_add_org_row=TRUE)
  metaregBase::mrb_run_as_test(project_dir = project_dir,file.path(project_dir, "run/run_mrb_test.R"), opts=opts)
  rstudioapi::navigateToFile(file.path(project_dir, "test_report/test_report.Rmd"))
  rstudioapi::filesPaneNavigate(project_dir)


  # Extra debugging info for a given runid (if report stopped due to error)
  # mrb_create_cache_at_runid(project_dir=project_dir, cache_runid=128)
  # drf = drf_create(project_dir)
  mrb_pid_test_files(project_dir, pid=128)
  mrb_test_data_path(project_dir, pid=128)

}

repboxRun::rb_remove_project_dirs(project_dir, clear_all=TRUE)
rb = rb_new(project_dir)
if (TRUE) {
  rb = rb_update_file_info_parcel(rb, overwrite=TRUE,assume_org_complete = TRUE)
  rb = rb_update_script_parcels(rb, overwrite=TRUE)
  rb = rb_update_static_code_analysis(rb, overwrite = TRUE)

  rb = rb_create_mod_dir(rb)
  rb = rb_run_stata_reproduction_raw(rb, overwrite= TRUE)
  rb = rb_postprocess_stata_reproduction(rb, overwrite= TRUE)
}


drf = repboxDRF::drf_load(project_dir)
drf_clear_mcache()
mrb = mrb_init(project_dir,with_try = TRUE)
if (TRUE) {
  options(warn=2)
  mrb = mrb_full_stata_script(mrb)
  mrb = mrb_run_stata_script(mrb)
  mrb$drf = repboxDRF:::drf_apply_caches(mrb$drf)
  mrb = mrb_agg_stata(mrb)

}
mrb = mrb_make_so_parcels(mrb)
mrb = mrb_run_r_base(mrb)
mrb = mrb_run_r_reg(mrb)
mrb = mrb_make_regcheck_parcel(mrb)
if (FALSE)
  mrb = mrb_repair_failed_runs(mrb = mrb)
library(repboxReportDo)
repboxReportDo::rrd_html_do(project_dir)

