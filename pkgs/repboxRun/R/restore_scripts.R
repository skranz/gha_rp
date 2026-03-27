# Restores scripts stored in repdb in case the org folder is deleted
# Allows to re-run static code analysis in case parsing has been changeds

example = function() {
  project_dir = "~/repbox/projects_gha/aejapp_1_1_3"
  repbox_restore_scripts(project_dir)
}

repbox_restore_scripts = function(project_dir, overwrite=FALSE) {
  parcels = repdb_load_parcels(project_dir, "stata_source")
  script_df = parcels[["stata_source"]]
  if (is.null(script_df) | NROW(script_df)==0) return(NULL)
  org_dir = file.path(project_dir, "org")
  i = 1
  for (i in seq_len(NROW(script_df))) {
    file = file.path(org_dir, script_df$file_path[i])
    dir = dirname(file)
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    if (!overwrite) {
      if (file.exists(file)) next
    }
    write_utf8(script_df$text[[i]], file)
  }
}
