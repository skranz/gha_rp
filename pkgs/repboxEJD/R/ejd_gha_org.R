

has.col = function(x, col) {
  col %in% names(x)
}

update_gha_repbox_art = function(art, repodir,db) {
  restore.point("update_gha_repbox_art")
  if (!isTRUE(art$repo=="oi")) {
    cat("\nWe can currently only use Github Actions for OpenICPSR articles.")
    return()
  }
  ejd_art = art_to_ejd_art(art,db)
  project.dir = paste0(repodir,"/project")
  if (!dir.exists(file.path(project.dir,"meta"))) {
    dir.create(file.path(project.dir,"meta"))
  }
  saveRDS(ejd_art, file.path(project.dir, "meta","ejd_art.Rds"))

  oi_id = str.between(art$data_url,"openicpsr/project/","/")
  yaml::write_yaml(list(repo_type="oi",repo_id = oi_id), file.path(repodir, "repbox_config.yml"))

}

write_ejd_gha_status = function(project.dir, status, runid, log.txt, prefer.inner.status = TRUE) {
  runid = as.character(runid)

  if (prefer.inner.status) {
    inner.status.file = file.path(project.dir, "gha","gha_inner_status.txt")
    if (file.exists(inner.status.file)) {
      status = readLines(inner.status.file)
    }
  }

  # Write current state
  gha.dir = file.path(project.dir, "gha")
  if (!dir.exists(gha.dir)) dir.create(gha.dir, recursive = TRUE)
  writeLines(status, file.path(gha.dir,"gha_status.txt"))
  writeLines(runid, file.path(gha.dir,"gha_runid.txt"))
  writeLines(log.txt, file.path(gha.dir,"gha_log.txt"))

  # Write permanent state
  #gha.dir = file.path(project.dir, "gha", format(Sys.time(),"%Y_%m_%d-%H%M"))
  #if (!dir.exists(gha.dir)) dir.create(gha.dir, recursive = TRUE)
  #writeLines(status, file.path(gha.dir,"gha_status.txt"))
  #writeLines(runid, file.path(gha.dir,"gha_runid.txt"))
  #writeLines(log.txt, file.path(gha.dir,"gha_log.txt"))
}
