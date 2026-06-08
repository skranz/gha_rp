repboxRun::repbox_load_libs()

# Should point to this project dir
project_dir = rb_get_project_dir("{{default_project_dir}}")
pid = 43

mrb_pid_test_files(project_dir, pid=pid)
mrb_test_data_path(project_dir, pid =pid,max_dta_files = Inf)


rstudioapi::filesPaneNavigate(file.path(project_dir,"run"))

rstudioapi::filesPaneNavigate(project_dir)
