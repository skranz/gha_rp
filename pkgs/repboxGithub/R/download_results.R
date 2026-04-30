gha_repbox_download_results = function(repo, project_dir, password=NULL) {
  restore.point("gha_repbox_download_results")
  if (!dir.exists(project_dir))
    stop(paste0("project_dir ", project_dir, " does not exist." ))

  temp.dir = file.path(project_dir, "temp")
  if (!dir.exists(temp.dir)) dir.create(temp.dir)
  zip.file = file.path(temp.dir, "gha_results.zip")
  gh_download_artifact(repo, destfile = zip.file)
  unzip(zip.file, exdir = temp.dir)
  z7.file = file.path(temp.dir, "results.7z")

  extract.7z(project_dir, z7.file, password = password)
  file.remove(zip.file)
  file.remove(z7.file)
}
