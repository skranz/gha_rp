repboxRun::repbox_load_libs()

# Should point to this project dir
project_dir = rb_get_project_dir("/home/rstudio/repbox/projects/aer_95_4_2")
mrb_test_data_path(project_dir, pid = 56,max_dta_files = Inf)
mrb_pid_test_files(project_dir, pid=56)

rstudioapi::filesPaneNavigate(file.path(project_dir,"run"))

rstudioapi::filesPaneNavigate(project_dir)
