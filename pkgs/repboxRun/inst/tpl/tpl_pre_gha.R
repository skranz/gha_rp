# Initializes the repbox project for a given article / reproduction package
# Essentially only copies all files, does not yet run any reproduction



repboxRun::repbox_load_libs()

# Should point to this project dir
project_dir = rb_get_project_dir("{{default_project_dir}}")

if (!dir.exists(project_dir)) dir.create(project_dir)

if (FALSE)
  rstudioapi::filesPaneNavigate(project_dir)

overwrite = {{overwrite}}
rb = rb_new(project_dir)

# Update article and supplement meta info
rb = rb_update_meta(rb, overwrite=overwrite)

# Update file_info
rb = rb_update_file_info_parcel(rb, overwrite=overwrite)

# Update script parcels
rb = rb_update_script_parcels(rb, overwrite=overwrite)

# Update docs
rb = rb_update_docs(rb, overwrite=overwrite)
