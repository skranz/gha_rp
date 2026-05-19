rb_read_intermediate_data = function(
  project_dir,
  file = file.path(project_dir, "repbox", "stata", "intermediate_data.Rds")
) {
  if (!file.exists(file)) {
    return(NULL)
  }
  readRDS(file)
}
