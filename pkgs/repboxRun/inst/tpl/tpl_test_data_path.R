repboxRun::repbox_load_libs()

# Should point to this project dir
project_dir = rb_get_project_dir("{{default_project_dir}}")
pid = 43

mrb_pid_test_files(project_dir, pid=pid)
mrb_test_data_path(project_dir, pid =pid,max_dta_files = Inf)



if (FALSE) {
  mrb = mrb_init(project_dir,with_try = FALSE)
  mrb = mrb_run_r_base(mrb,just_pids=pid,continue_on_error = FALSE)
  mrb = mrb_run_r_reg(mrb, just_pids=pid,continue_on_error = FALSE)
}

rstudioapi::filesPaneNavigate(file.path(project_dir,"run"))

rstudioapi::filesPaneNavigate(project_dir)
