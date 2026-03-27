# Initializes the repbox project for a given article / reproduction package
# Essentially only copies all files, does not yet run any reproduction

library(repboxRun)

# Should point to this project dir
project_dir = rb_get_project_dir("{{default_project_dir}}")

if (FALSE)
  rstudioapi::filesPaneNavigate(project_dir)

rb = rb_make_rb(project_dir, just_steps=NULL, ignore_steps=NULL)

# Update article and supplement meta info
rb = rb_update_meta(rb, overwrite=FALSE)

# Update file_info
rb = rb_update_file_info_parcel(rb, overwrite=FALSE)

# Update script parcels
rb = rb_update_script_parcels(rb, overwrite=FALSE)

# Update docs
rb = rb_update_docs(rb, overwrite=FALSE)
