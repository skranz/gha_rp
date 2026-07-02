repboxRun::repbox_load_libs()

# Should point to this project dir
project_dir = rb_get_project_dir("{{project_dir}}")
mrb = mrb_repair_paths_with_r_fail_cmds_via_cache(project_dir, max_cache=5)
#mrb = mrb_repair_via_ignore(project_dir = project_dir)
repboxReportDo::rrd_html_do(project_dir)

if (FALSE) {
  rstudioapi::filesPaneNavigate(file.path(project_dir,"run"))
  browseURL(file.path(project_dir,"reports/do_report.html" ))
  mrb_runid_test_files(project_dir, runid=127)
}
