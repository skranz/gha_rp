rb_remove_dir = function(rb,sub_dir = NULL, project_dir=rb$project_dir ) {
  if (is.null(project_dir)) stop()
  if (is.null(sub_dir)) stop()

  rem_dir = file.path(project_dir, sub_dir)
  if (!dir.exists(rem_dir)) return()
  remove.dir(rem_dir,must.contain = project_dir)
}
