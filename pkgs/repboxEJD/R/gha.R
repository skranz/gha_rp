# At some point we might move the code to another repo

example = function() {



  project_dir = "~/repbox/projects_full/myproject"
  repo = "skranz/gha_repbox_ex4"
  repodir = normalizePath("~/repbox/gha/gha_repbox_ex4")
  temp_dir = normalizePath("~/repbox/temp")
  pat = readLines("~/repbox/pat.txt",warn = FALSE)
  gh_set_pat(pat)

  gh_cli_list_runids(repodir)
  gh_remove_previous_runs(repodir)

  password_7z = password = readLines("~/repbox/repbox_encryption.txt",warn = FALSE)
  overwrite = TRUE

  #GithubActions::gh_update(repodir)

  #encrypt_files_in_gha_folder(repodir, password_7z)
  #gha_repbox_run_project(project_dir, repodir, repo, pat, password_7z, temp_dir)

  artid = "aejapp_10_2_8"
  repodir = normalizePath("~/repbox/gha/gha_repbox_ex4")
  project_dir = paste0("~/repbox/projects_full/", artid)
  repo = "skranz/gha_repbox_ex4"
  temp_dir = normalizePath("~/repbox/temp")
  pat = readLines("~/repbox/pat.txt",warn = FALSE)
  gh_set_pat(pat)
  password_7z = password = readLines("~/repbox/repbox_encryption.txt",warn = FALSE)
  overwrite = TRUE
  GithubActions::gh_remove_history(repodir)
  GithubActions::gh_remove_previous_runs(repodir)

  gha_ejd_repbox_init_repo(artid=artid, repodir=repodir)
  encrypt_files_in_gha_folder(repodir, password_7z)
  gha_ejd_move_sup_file(repodir,to="~/web/repbox_temp/temp_sup.7z")

  gha_repbox_run_project(project_dir, repodir, repo, pat, password_7z, temp_dir)

}

gha_ejd_move_sup_file = function(repodir, to,from = NULL) {
  if (is.null(from)) {
    from = list.files(file.path(repodir,"sup"),full.names = TRUE)[1]
  }
  cat("\nMove ", from, " to ", to,"\n")
  to = normalizePath(to, mustWork = FALSE)
  cmd = paste0("mv ", from," ", to)
  system(cmd)
  #file.rename(from, to)
}

gha_ejd_repbox_init_repo = function(artid=NULL, repodir, copy_sup=TRUE, art=NULL,ejd.db.dir = "~",...) {
  db = db = get.articles.db(ejd.db.dir)
  art = get_ejd_art(artid=artid, art=art,db=db, ...)
  res = get_ejd_project_art_and_sup_files(art=art, ...)
  restore.point("gha_ejd_repbox_init_repo")
  art_file = res$art_file
  sup_file = res$sup_file

  if (is.null(art_file)) {
    cat("\nNo article file found for ", art$artid,"\n")
    return(FALSE)
  }
  if (is.null(sup_file)) {
    cat("\nNo article file found for ", art$artid,"\n")
    return(FALSE)
  }


  art$pdf_file = res$pdf_file
  art$html_file = res$html_file
  art$has.pdf = !is.null(art$pdf_file)
  art$has.html = !is.null(art$html_file)

  remove.all.files.in.dir(file.path(repodir,"art"))
  remove.all.files.in.dir(file.path(repodir,"sup"))
  remove.all.files.in.dir(file.path(repodir,"meta"))

  file.copy(art_file, file.path(repodir,"art",basename(art_file)))
  if (copy_sup) {
    file.copy(sup_file, file.path(repodir,"sup",basename(sup_file)))
  }

  meta = ejd_art_to_meta(as_tibble(art),db)
  writeLines(artid,file.path(repodir, "artid.txt"))
  save_art_sup_meta(meta, repodir)
}


# We assume everything is already copied to gha_dir and settings are set
gha_repbox_run_project = function(project_dir, repodir, repo, pat,  password_7z=NULL, temp_dir = tempdir(), overwrite=TRUE) {
  restore.point("gha_repbox_run_project")
  artid = project = basename(project_dir)

  if (!overwrite) {
    gha.status.file = file.path(project_dir, "gha","gha_status.txt")
    if (file.exists(gha.status.file)) {
      cat("\nWe already have run gha for project ", project_dir,".\n")
      return("already_run")
    }
  }

  cat("\nPrepare Github action for ", project,"\n")

  library(repboxGithub)
  gh_set_pat(pat)

  if (!dir.exists(project_dir)) {
    dir.create(project_dir)
  }

  # In case some error happens: already generate result project dir
  # Fill status as error
  write_ejd_gha_status(project_dir, status="error_in_job_script",runid="NA",log.txt="")

  # Update github repo
  cat("\nUpdate Github repo\n")
  gh_update(repodir,msg = paste0(project))

  old_runid = gh_newest_runid(repo)

  # Run github action workflow
  cat("\nStart Github action workflow...\n")
  gh_run_workflow(repo)

  runid = gh_wait_until_new_run_starts(repo, old_runid)
  if (is.na(runid)) {
    write_ejd_gha_status(project_dir, status="not_started",runid=NA,log.txt="run not started")
    return("not_started")
  }

  cat("\n",as.character(Sys.time())," wait until Github workflow finishes...\n")
  run = gh_wait_until_run_completed(repo, runid)

  cat("\n",as.character(Sys.time()),"Run finished with status:",run$status,"\n")

  status = run$status
  if (is.null(status)) {
    status = "timeout"
  }
  ok = isTRUE(status == "completed")
  runid = run$id[[1]]
  # Get results from Github

  if (ok) {
    Sys.sleep(2)
    ok = gha_repbox_download_full_results(project_dir, repo=repo, temp_dir=temp_dir, runid=runid,password_7z = password_7z)
  }

  if(!ok) {
    if (!dir.exists(project_dir)) dir.create(project_dir)
    repboxUtils::repbox_problem("GHA run failed.", "gha_failed", fail_action = "msg", project_dir=project_dir)
    if (status=="completed") {
      status = "no_artifact"
    }
  }


  write_ejd_gha_status(project_dir, status=status,runid=runid,log.txt="")

  return(status)
}

gha_repbox_download_full_results = function(project_dir, repo, temp_dir, runid=NULL, password_7z=NULL, pat=gh_pat()) {
  restore.point("gha_repbox_download_full_results")
  artifacts = gh_list_artifacts(repo, runid=runid,pat = pat)
  if (NROW(artifacts)==0) {
    cat("\nCould not find any artifact.")
    return(FALSE)
  }
  artifact_id = artifacts$id[[1]]

  zip.file = file.path(temp_dir, "project.zip")
  gh_download_artifact(repo, destfile = zip.file, artifact_id=artifact_id, runid=runid, pat=pat,use_curl = TRUE)
  file.exists(zip.file)
  zip_content = unzip(zip.file,list = TRUE)$Name

  is_7z = NROW(zip_content==1) & endsWith(zip_content[1],".7z")
  if (!is_7z) {
    stop("The results ZIP does not contain a 7z file. This case is not yet implemented.")
  }

  unzip(zip.file, exdir = temp_dir)
  tmp_project_dir = file.path(temp_dir,"temp_project")
  if (dir.exists(tmp_project_dir)) unlink(tmp_project_dir, recursive = TRUE)
  dir.create(tmp_project_dir)
  extract.7z(tmp_project_dir, file.path(temp_dir,zip_content), password = password_7z)

  if (dir.exists(tmp_project_dir)) {
    copy.dir(tmp_project_dir, project_dir, overwrite=TRUE)
    return(TRUE)
  } else {
    return(FALSE)
  }
}



encrypt_files_in_gha_folder = function(repodir, password) {
  restore.point("encrypt_files_in_gha_folder")
  dir = file.path(repodir, "art")
  files = list.files(dir, full.names = TRUE)
  files = files[!endsWith(files,"7z")]
  if (length(files)>0) {
    file = first(files)
    to.7z(file, password=password)
    file.remove(files)
  }

  dir = file.path(repodir, "sup")
  files = list.files(dir,glob2rx("*.zip"),ignore.case = TRUE, full.names = TRUE)
  files = files[!endsWith(files,"7z")]
  if (length(files)>0) {
    file = first(files)
    to.7z(file, password=password)
    file.remove(files)
  }

}

