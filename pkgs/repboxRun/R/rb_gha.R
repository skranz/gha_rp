# Example end-to-end test script for the minimal Github Actions
# Stata reproduction in /home/rstudio/repbox/gha/gha_rp.
#
# Adapt all paths and repo names to your setup.
#
# This script also works if the project has no original supplement ZIP
# anymore, but still has a complete /org folder. In that case
# gha_rp_stage_sup_zip() creates a ZIP from /org automatically.

example = function() {
  suppressPackageStartupMessages(library(repboxRun))
  project_dir = "/home/rstudio/repbox/projects/gha_test"
  project_dir = "~/repbox/projects/aejapp_11_2_10"
  rb_run_gha_stata_reproduction(project_dir, overwrite=TRUE)
  browseURL("https://github.com/skranz/gha_rp/actions")
  rstudioapi::filesPaneNavigate(project_dir)

}

rb_run_gha_stata_reproduction = function(project_dir, postprocess=TRUE, overwrite=FALSE) {
  restore.point("rb_run_gha_stata_reproduction")
  #stop()
  if (!overwrite && rb_has_stata_raw_reproduction(project_dir=project_dir) & (!postprocess | rb_has_stata_postprocess(project_dir=project_dir))) {
    cat("\nRaw Stata reproduction already exists. Thus skipped.")
    return()
  }


  log_dir = file.path(project_dir, "gha_log")
  if (!dir.exists(log_dir)) dir.create(log_dir)
  log_files = list.files(log_dir, glob2rx("*.log"),full.names = TRUE)
  if (length(log_files)>0)
    file.remove(log_files)

  writeLines(as.character(Sys.time()), file.path(log_dir, "gha_start.log"))

  project_dir = normalizePath(project_dir)


  suppressPackageStartupMessages(library(repboxGithub))
  suppressPackageStartupMessages(library(repboxRun))

  # ------------------------------------------------------------
  # 1. Example paths and repo names
  # ------------------------------------------------------------

  # Local repbox project.
  # Example 1: project has an original supplement ZIP somewhere that
  # rb_get_sup_zip(project_dir) can find.
  #
  # Example 2: project only has /org and no ZIP. Then the helper
  # automatically creates meta/<artid>_org.zip from /org.

  artid = basename(project_dir)

  # Local checkout of the Github Actions repo.


  if (FALSE)
    rstudioapi::filesPaneNavigate(gha_repo_dir)

  gha_repo_dir = "/home/rstudio/repbox/gha/gha_rp"

  # Github repo slug used by git and gh.
  github_repo = "skranz/gha_rp"

  # Workflow file in the gha_rp repo.
  workflow_file = "run_remote_stata.yml"

  # Directory on your server where the supplement ZIP should be copied.
  server_dir = "/home/rstudio/web/repbox_temp"

  # Public base URL that maps to server_dir.
  url_base = "http://klein.mathematik.uni-ulm.de/repbox_temp"

  # ------------------------------------------------------------
  # 2. Stage the supplement ZIP and refresh gha_rp/run_config.R
  # ------------------------------------------------------------

  # If you want to force using a specific ZIP file, set sup_zip explicitly.
  # Otherwise keep sup_zip = NULL.
  sup_zip = NULL

  prep = gha_rp_prepare_repo(
    project_dir = project_dir,
    server_dir = server_dir,
    url_base = url_base,
    repo_dir = gha_repo_dir,
    sup_zip = sup_zip,
    overwrite = TRUE,
    timeout = 10 * 60,
    create_mod_dir = TRUE,
    capture_reg_info = TRUE,
    capture_scalar_info = TRUE,
    stop.on.error = TRUE,
    work_dir = "gha_work",
    output_dir = "gha_output/bundle",
    artifact_name = "gha-rp-results"
  )
  saveRDS(prep, file.path(log_dir, "gha_run_settings.Rds"))

  cat("\nPrepared Github Actions input.\n")
  print(prep$staged_sup)
  print(prep$run_config)

  # 2b copy local r packages


  repboxGithub::copy_r_package("/home/rstudio/repbox/repboxRun",dest_parent_dir = "~/repbox/gha/gha_rp/pkgs", overwrite = TRUE)
  repboxGithub::copy_r_package("/home/rstudio/repbox/repboxStata",dest_parent_dir = "~/repbox/gha/gha_rp/pkgs", overwrite=TRUE)
  #repboxGithub::copy_r_package("/home/rstudio/repbox/GithubActions",dest_parent_dir = "~/repbox/gha/gha_rp/pkgs")
  # ------------------------------------------------------------
  # 3. Commit and push the updated gha_rp/run_config.R
  # ------------------------------------------------------------

  library(GithubActions)

  GithubActions::gh_set_pat(readLines("/home/rstudio/repbox/pat.txt"))
  GithubActions::gh_auth_status()

  # The workflow reads run_config.R from the Github repo, so the changed
  # config must be committed and pushed before triggering the run.


  GithubActions::gh_update(gha_repo_dir,msg = paste0("Update to ", artid))

  # ------------------------------------------------------------
  # 4. Trigger the Github Actions workflow
  # ------------------------------------------------------------

  old_runid = runid = GithubActions::gh_newest_runid(github_repo)

  cat("\nStart GHA workflow\n")

  cat("\nSee\nhttps://github.com/skranz/gha_rp/actions\n")

  res = GithubActions::gh_run_workflow(github_repo)

  cat("\nWait until workflow actually starts\n")
  runid = GithubActions::gh_wait_until_new_run_starts(
    repo = github_repo,
    old_runid = old_runid,
    pause.sec = 3,
    timeout = 20
  )

  cat("\nWait until workflow run completes...\n")
  run_info = GithubActions::gh_wait_until_run_completed(
    repo = github_repo,
    runid = runid,
    pause.sec = 10,
    timeout = 6 * 60 * 60
  )

  cat("\nWait until workflow is completed...\n")

  cat("\nArtifact:\n")
  arti = GithubActions::gh_list_artifacts(
    repo = github_repo,
    runid = runid
  )
  gha_log = as.character(try(GithubActions::gh_run_log(repodir = gha_repo_dir, runid=runid)))
  gha_log = str.right.of(gha_log, "Z ")
  row = which(startsWith(gha_log, "GHA: Start Reproduction"))
  if (length(row)>0) {
    gha_log = gha_log[row[1]:NROW(gha_log)]
  }
  row = which(startsWith(gha_log, "GHA: End Reproduction"))
  if (length(row)>0) {
    gha_log = gha_log[1:row[1]]
  }

  log = paste0(as.character(Sys.time()),"\n", paste0(gha_log, collapse="\n"))
  writeLines(log, file.path(log_dir, "gha_log.log"))

  print(arti)
  if (NROW(arti)==0) {
    writeLines(as.character(Sys.time()), file.path(log_dir, "gha_no_artificat.log"))
    cat("\nNo artifact found / created. Likely an error in the Github action run. See \n", file.path(log_dir, "gha_no_artifact.log"))
    return()
  }


  cat("\nCopy and extract raw results to local project directory.\n")
  gha_rp_download_stata_raw_results(
    repo = github_repo,
    project_dir = project_dir,
    overwrite = TRUE,
    verify = TRUE,
    local_input_zip = NULL
  )

  cat("\nPerform local post-processing of Stata reproduction.\n")

  # ------------------------------------------------------------
  # 6. Local postprocess after the remote raw run
  # ------------------------------------------------------------


  writeLines(as.character(Sys.time()), file.path(log_dir, "gha_ok.log"))

  # Now do the easy-to-debug local steps.

  rb = repboxRun:::rb_new(
    project_dir = project_dir,
    copy_existing = FALSE,
    fail_action = "error"
  )

  # Make sure the file and script parcels exist locally.
  rb = repboxRun:::rb_update_file_info_parcel(
    rb = rb,
    overwrite = FALSE,
    assume_org_complete = TRUE
  )

  rb = repboxRun:::rb_update_script_parcels(
    rb = rb,
    overwrite = FALSE
  )

  # Turn the raw Stata outputs into local derivatives.
  rb = repboxRun:::rb_postprocess_stata_reproduction(
    rb = rb,
    overwrite = TRUE,
    build_run_info = TRUE,
    build_do_run_info = TRUE,
    build_reg_info = TRUE,
    build_drf = TRUE
  )
  cat("\nDone with stata reproduction for ", project_dir,"\n")

  cat(paste0("\nrstudioapi::filesPaneNavigate('",project_dir,"')\n"))
  writeLines(as.character(Sys.time()), file.path(log_dir, "gha_postprocess_ok.log"))

}


rb_make_stata_run_manifest = function(project_dir, input_zip = NULL, extra = list()) {
  restore.point("rb_make_stata_run_manifest")

  project_dir = normalizePath(project_dir, mustWork = FALSE)
  artid = basename(project_dir)

  repbox_results_file = file.path(project_dir, "repbox", "stata", "repbox_results.Rds")

  manifest = list(
    artid = artid,
    created_at = Sys.time(),
    project_dir = project_dir,
    input_zip = if (!is.null(input_zip)) normalizePath(input_zip, mustWork = FALSE) else NA_character_,
    input_zip_base = if (!is.null(input_zip)) basename(input_zip) else NA_character_,
    input_zip_md5 = if (!is.null(input_zip) && file.exists(input_zip)) {
      unname(tools::md5sum(input_zip))
    } else {
      NA_character_
    },
    repbox_results_exists = file.exists(repbox_results_file),
    repbox_results_mtime = if (file.exists(repbox_results_file)) file.mtime(repbox_results_file) else as.POSIXct(NA),
    github_repository = Sys.getenv("GITHUB_REPOSITORY", unset = NA_character_),
    github_run_id = Sys.getenv("GITHUB_RUN_ID", unset = NA_character_),
    github_sha = Sys.getenv("GITHUB_SHA", unset = NA_character_),
    github_ref = Sys.getenv("GITHUB_REF", unset = NA_character_),
    host = Sys.info()[["nodename"]]
  )

  if (length(extra) > 0) {
    manifest = utils::modifyList(manifest, extra)
  }
  manifest
}

#' Write the raw Stata run manifest
#'
#' @export
rb_write_stata_run_manifest = function(
  project_dir,
  input_zip = NULL,
  extra = list(),
  file = file.path(project_dir, "repbox", "stata", "stata_remote_manifest.Rds")
) {
  restore.point("rb_write_stata_run_manifest")

  manifest = rb_make_stata_run_manifest(
    project_dir = project_dir,
    input_zip = input_zip,
    extra = extra
  )

  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  saveRDS(manifest, file)
  invisible(file)
}

#' Read the raw Stata run manifest
#'
#' @export
rb_read_stata_run_manifest = function(
  project_dir = NULL,
  file = if (!is.null(project_dir)) {
    file.path(project_dir, "repbox", "stata", "stata_remote_manifest.Rds")
  } else {
    NULL
  }
) {
  restore.point("rb_read_stata_run_manifest")

  if (is.null(file) || !file.exists(file)) return(NULL)
  readRDS(file)
}

rb_copy_tree = function(from, to, overwrite = TRUE) {
  restore.point("rb_copy_tree")

  if (!file.exists(from) && !dir.exists(from)) return(FALSE)

  if (dir.exists(from)) {
    if ((file.exists(to) || dir.exists(to)) && overwrite) {
      unlink(to, recursive = TRUE, force = TRUE)
    }
    if ((file.exists(to) || dir.exists(to)) && !overwrite) {
      return(TRUE)
    }

    dir.create(to, recursive = TRUE, showWarnings = FALSE)
    rb_copy_dir_contents(
      from_dir = from,
      to_dir = to,
      overwrite = overwrite
    )
    return(TRUE)
  }

  if ((file.exists(to) || dir.exists(to)) && overwrite) {
    unlink(to, recursive = TRUE, force = TRUE)
  }
  if ((file.exists(to) || dir.exists(to)) && !overwrite) {
    return(TRUE)
  }

  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  ok = file.copy(
    from = from,
    to = to,
    recursive = FALSE,
    copy.mode = TRUE,
    copy.date = TRUE,
    overwrite = overwrite
  )

  if (!isTRUE(ok)) {
    stop(paste0("Could not copy ", from, " to ", to, "."))
  }
  TRUE
}

rb_copy_dir_contents = function(from_dir, to_dir, overwrite = TRUE) {
  restore.point("rb_copy_dir_contents")

  if (!dir.exists(from_dir)) return(FALSE)

  dir.create(to_dir, recursive = TRUE, showWarnings = FALSE)

  rel = list.files(
    from_dir,
    recursive = TRUE,
    full.names = FALSE,
    include.dirs = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )

  if (length(rel) == 0) {
    return(TRUE)
  }

  full = file.path(from_dir, rel)
  is_dir = dir.exists(full)

  dir_rel = rel[is_dir]
  if (length(dir_rel) > 0) {
    dir_targets = file.path(to_dir, dir_rel)
    invisible(vapply(
      X = unique(dir_targets),
      FUN = dir.create,
      recursive = TRUE,
      showWarnings = FALSE,
      FUN.VALUE = logical(1)
    ))
  }

  file_rel = rel[!is_dir]
  if (length(file_rel) == 0) {
    return(TRUE)
  }

  file_from = file.path(from_dir, file_rel)
  file_to = file.path(to_dir, file_rel)

  parent_dirs = unique(dirname(file_to))
  invisible(vapply(
    X = parent_dirs,
    FUN = dir.create,
    recursive = TRUE,
    showWarnings = FALSE,
    FUN.VALUE = logical(1)
  ))

  ok = file.copy(
    from = file_from,
    to = file_to,
    overwrite = overwrite,
    copy.mode = TRUE,
    copy.date = TRUE
  )

  if (!all(ok)) {
    stop(paste0("Could not copy all files from ", from_dir, " to ", to_dir, "."))
  }
  TRUE
}

#' Create a narrow Github Actions bundle for the raw Stata run
#'
#' The bundle contains only repbox/stata, steps, problems, and a copy
#' of the raw run manifest at the bundle root.
#'
#' @export
rb_make_gha_stata_bundle = function(
  project_dir,
  bundle_dir = file.path(project_dir, "gha_output", "bundle"),
  overwrite = TRUE,
  input_zip = NULL,
  manifest_extra = list()
) {
  restore.point("rb_make_gha_stata_bundle")

  project_dir = normalizePath(project_dir, mustWork = FALSE)
  if (!file.exists(file.path(project_dir, "repbox", "stata", "repbox_results.Rds"))) {
    stop("No raw Stata results found in repbox/stata.")
  }

  manifest_file = rb_write_stata_run_manifest(
    project_dir = project_dir,
    input_zip = input_zip,
    extra = manifest_extra
  )

  if (dir.exists(bundle_dir) && overwrite) {
    unlink(bundle_dir, recursive = TRUE, force = TRUE)
  }
  dir.create(bundle_dir, recursive = TRUE, showWarnings = FALSE)

  rb_copy_tree(
    from = file.path(project_dir, "repbox", "stata"),
    to = file.path(bundle_dir, "repbox", "stata"),
    overwrite = TRUE
  )

  if (dir.exists(file.path(project_dir, "steps"))) {
    rb_copy_tree(
      from = file.path(project_dir, "steps"),
      to = file.path(bundle_dir, "steps"),
      overwrite = TRUE
    )
  }

  if (dir.exists(file.path(project_dir, "problems"))) {
    rb_copy_tree(
      from = file.path(project_dir, "problems"),
      to = file.path(bundle_dir, "problems"),
      overwrite = TRUE
    )
  }

  file.copy(
    from = manifest_file,
    to = file.path(bundle_dir, "stata_remote_manifest.Rds"),
    overwrite = TRUE,
    copy.mode = TRUE,
    copy.date = TRUE
  )

  invisible(bundle_dir)
}

#' Import a raw Github Actions Stata bundle into a local project
#'
#' @export
rb_import_gha_stata_bundle = function(
  project_dir,
  bundle_dir,
  overwrite = TRUE,
  verify = TRUE,
  local_input_zip = NULL
) {
  restore.point("rb_import_gha_stata_bundle")

  project_dir = normalizePath(project_dir, mustWork = FALSE)
  bundle_dir = normalizePath(bundle_dir, mustWork = TRUE)

  manifest_file = file.path(bundle_dir, "stata_remote_manifest.Rds")
  if (!file.exists(manifest_file)) {
    manifest_file = file.path(bundle_dir, "repbox", "stata", "stata_remote_manifest.Rds")
  }

  manifest = NULL
  if (file.exists(manifest_file)) {
    manifest = readRDS(manifest_file)
  }

  if (verify && !is.null(manifest)) {
    if (!is.na(manifest$artid) && !identical(as.character(manifest$artid), basename(project_dir))) {
      stop("Bundle artid does not match the local project directory.")
    }

    if (!is.null(local_input_zip) && file.exists(local_input_zip) &&
        !is.null(manifest$input_zip_md5) && !is.na(manifest$input_zip_md5)) {
      local_md5 = unname(tools::md5sum(local_input_zip))
      if (!identical(as.character(local_md5), as.character(manifest$input_zip_md5))) {
        stop("Local supplement ZIP md5 does not match the imported bundle manifest.")
      }
    }
  }

  stata_from = file.path(bundle_dir, "repbox", "stata")
  if (!dir.exists(stata_from)) {
    stop("Bundle does not contain repbox/stata.")
  }

  rb_copy_tree(
    from = stata_from,
    to = file.path(project_dir, "repbox", "stata"),
    overwrite = overwrite
  )

  steps_from = file.path(bundle_dir, "steps")
  if (dir.exists(steps_from)) {
    rb_copy_dir_contents(
      from_dir = steps_from,
      to_dir = file.path(project_dir, "steps"),
      overwrite = overwrite
    )
  }

  problems_from = file.path(bundle_dir, "problems")
  if (dir.exists(problems_from)) {
    rb_copy_dir_contents(
      from_dir = problems_from,
      to_dir = file.path(project_dir, "problems"),
      overwrite = overwrite
    )
  }

  invisible(list(
    project_dir = project_dir,
    bundle_dir = bundle_dir,
    manifest = manifest
  ))
}
