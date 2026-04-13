Here are the complete modified files to integrate your new intermediate data logic properly into Github Actions, as well as Direct Replication Format generation for Stata and R.

To answer your questions:
1. **Artifact copying**: Yes, `rb_make_gha_stata_bundle()` copies the entire `repbox/stata` tree via `rb_copy_tree()`. This means `repbox/stata/intermediate_data/`, `intermediate_state/`, and `intermediate_data.Rds` are already automatically securely exported!
2. **Filtering `mod` on GHA**: I've added the parameter `filter_mod_by_intermediate = TRUE` to `rb_make_gha_stata_bundle` and updated its logic so it cross-references `generated_mod_files` against `intermediate_data.Rds` before bundling them.
3. **DRF Integration**: `drf_run_df_add_data_paths` in `repboxDRF` now interrogates `intermediate_data.Rds` to determine the most recent intermediate snapshot available *before* a specific `runid`. `drf_copy_org_data` now diverts these accurately to `drf/im_data` and references them properly during Stata/R DRF translation.

I have also fixed a subtle bug in `repboxStata/R/intermediate_data.R` (`repbox_intermediate_read_state_df`) where `copy_path` incorrectly generated flat relative paths like `c1_data/temp.dta` instead of preserving the real Stata structure `data/c1_temp.dta`.


### 2. Update `repboxRun/R/rb_gha.R`

!MODIFICATION rb_gha.R
scope = "file"
file = "/home/rstudio/repbox/repboxRun/R/rb_gha.R"
is_new_file = false
description = "Implement filter_mod_by_intermediate to only track explicitly saved intermediate states."
---
```R
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

rb_has_ok_gha_run = function(project_dir) {
  file.exists(file.path(project_dir, "gha_log/gha_ok.log"))
}

rb_run_gha_stata_reproduction = function(project_dir, postprocess=FALSE, overwrite=FALSE, timeout = 30*60) {
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
    timeout = timeout,
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
    timeout = timeout + 60
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

  cat("\nDone with GHA stata reproduction for ", project_dir,"\n")


  writeLines(as.character(Sys.time()), file.path(log_dir, "gha_ok.log"))

  if (!postprocess) return()

  # ------------------------------------------------------------
  # 6. Local postprocess after the remote raw run
  # ------------------------------------------------------------
  cat("\nPerform local post-processing of Stata reproduction.\n")


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
  cat("\nDone with stata postprocessing reproduction for ", project_dir,"\n")

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
#' The bundle contains repbox/stata, steps, problems, a copy
#' of the raw run manifest, generated or changed files from /mod
#' that are not already available in /org, and intermediate_data.
#'
#' @export
rb_make_gha_stata_bundle = function(
  project_dir,
  bundle_dir = file.path(project_dir, "gha_output", "bundle"),
  overwrite = TRUE,
  input_zip = NULL,
  manifest_extra = list(),
  include_generated_mod_files = TRUE,
  filter_mod_by_intermediate = TRUE,
  generated_mod_files = NULL,
  ignore_repbox_generated_files = TRUE,
  compare_mod_relative_path = TRUE,
  treat_same_name_same_size_as_existing = TRUE
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

  if (isTRUE(include_generated_mod_files)) {
    if (is.null(generated_mod_files)) {
      generated_mod_files = rb_find_generated_mod_files(
        project_dir = project_dir,
        ignore_repbox_files = ignore_repbox_generated_files,
        compare_relative_path = compare_mod_relative_path,
        treat_same_name_same_size_as_existing = treat_same_name_same_size_as_existing,
        only_generated = TRUE
      )
    }

    if (isTRUE(filter_mod_by_intermediate) && NROW(generated_mod_files) > 0) {
      im_file = file.path(project_dir, "repbox", "stata", "intermediate_data.Rds")
      if (file.exists(im_file)) {
        im_df = readRDS(im_file)
        if (NROW(im_df) > 0 && "file_path" %in% names(im_df)) {
          keep = generated_mod_files$rel_path %in% im_df$file_path
          generated_mod_files = generated_mod_files[keep, , drop = FALSE]
        } else {
          generated_mod_files = generated_mod_files[0, , drop = FALSE]
        }
      } else {
        generated_mod_files = generated_mod_files[0, , drop = FALSE]
      }
    }

    if (NROW(generated_mod_files) > 0) {
      rb_copy_generated_mod_files_to_dir(
        project_dir = project_dir,
        to_dir = file.path(bundle_dir, "mod"),
        file_df = generated_mod_files,
        overwrite = TRUE
      )
    }
  }

  # Deprecated root intermediate_data folder mapping (retained in case old setups exist)
  if (dir.exists(file.path(project_dir, "intermediate_data"))) {
    rb_copy_tree(
      from = file.path(project_dir, "intermediate_data"),
      to = file.path(bundle_dir, "intermediate_data"),
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
  local_input_zip = NULL,
  import_mod_files = TRUE,
  import_intermediate_data = TRUE,
  restore_intermediate_data_to_mod = TRUE
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

  imported_mod_files = character(0)
  if (isTRUE(import_mod_files)) {
    mod_from = file.path(bundle_dir, "mod")
    if (dir.exists(mod_from)) {
      rb_copy_dir_contents(
        from_dir = mod_from,
        to_dir = file.path(project_dir, "mod"),
        overwrite = overwrite
      )
      imported_mod_files = list.files(
        path = mod_from,
        recursive = TRUE,
        full.names = FALSE,
        all.files = TRUE,
        no.. = TRUE
      )
    }
  }

  imported_intermediate_files = character(0)
  if (isTRUE(import_intermediate_data)) {
    intermediate_from = file.path(bundle_dir, "intermediate_data")
    if (dir.exists(intermediate_from)) {
      rb_copy_tree(
        from = intermediate_from,
        to = file.path(project_dir, "intermediate_data"),
        overwrite = overwrite
      )
      imported_intermediate_files = list.files(
        path = intermediate_from,
        recursive = TRUE,
        full.names = FALSE,
        all.files = TRUE,
        no.. = TRUE
      )
    }
  }

  if (isTRUE(restore_intermediate_data_to_mod)) {
    # If the user explicitly asks, try to map the restored items back to mod
    # but normally the mod mapping is extracted organically.
    # rb_restore_latest_intermediate_data_to_mod(project_dir, overwrite = overwrite)
  }

  invisible(list(
    project_dir = project_dir,
    bundle_dir = bundle_dir,
    manifest = manifest,
    imported_mod_files = imported_mod_files,
    imported_intermediate_files = imported_intermediate_files
  ))
}

```
!END_MODIFICATION rb_gha.R

### 3. Update `repboxDRF/R/drf.R`

!MODIFICATION drf.R
scope = "file"
file = "/home/rstudio/repbox/repboxDRF/R/drf.R"
is_new_file = false
description = "Inject intermediate data path mappings eagerly into run_df initialization."
---
```R
example = function() {
  # Should point to this project dir
  project_dir = "~/repbox/projects/aejapp_11_2_10"

  if (FALSE)
    rstudioapi::filesPaneNavigate(project_dir)

  drf = drf_load(project_dir)

}


# To do: variant that is faster for selected paths
drf_load = function(project_dir, parcels=list()) {
  restore.point("drf_load")
  project_dir = normalizePath(project_dir)
  drf = list(project_dir = project_dir, drf_dir = file.path(project_dir, "drf"), parcels = parcels)
  drf$parcels = repboxDB::repdb_load_parcels(project_dir, c("stata_run_cmd", "r_trans"), parcels=parcels)
  drf$run_df = drf_make_run_df(drf=drf,add_rcode = TRUE)
  drf$path_df = drf_load_path_df(drf=drf)
  drf$path_df = drf_add_path_df_cols_for_cache(drf=drf)
  drf$runids = drf_runids(drf)
  drf$pids= drf_pids(drf)
  drf
}

drf_pids = function(drf, path_df=drf$path_df) {
  if (!is.null(drf[["runids"]])) drf$pids
  unique(drf$path_df$pid)
}

drf_runids = function(drf, path_df=drf$path_df) {
  if (!is.null(drf[["runids"]])) drf$runids
  unique(drf$path_df$runid)
}


drf_make_run_df = function(project_dir = drf$project_dir, parcels=drf$parcels, drf = list(parcels=list()), add_rcode = TRUE) {
  restore.point("drf_make_run_df")
  parcels = repboxDB::repdb_load_parcels(project_dir, "stata_run_cmd", parcels=parcels)
  run_df = parcels$stata_run_cmd

  start_time = run_df$start_time[1]
  run_df$ok = !is.true(run_df$errcode != 0) & !is.true(run_df$missing_data)
  run_df$start_sec = sec_since_start(run_df$start_time, start_time)
  run_df$end_sec = sec_since_start(run_df$end_time, start_time)
  run_df$dur_sec =run_df$end_sec - run_df$start_sec

  cmd_types = drf_stata_cmd_types_vec()
  run_df$cmd_type = cmd_types[run_df$cmd]

  rows = run_df$cmd_type == "reg"
  inds = which(has.substr(run_df$cmdline[rows]," _I"))
  run_df$is_I_reg = FALSE
  run_df$is_I_reg[rows[inds]] = TRUE

  rows = run_df$cmd_type %in% c("reg","mod")
  inds = which(stri_detect_regex(run_df$cmdline[rows],"(^|[ \\:])[ ]*xi[ \\:]"))
  run_df$is_xi = FALSE
  run_df$is_xi[inds] = TRUE
  
  # Resolve explicit intermediate tracking before generating scripts / caches
  run_df = drf_run_df_add_data_paths(run_df, drf_dir = file.path(project_dir, "drf"), drf = drf)

  if (add_rcode) {
    parcels = repboxDB::repdb_load_parcels(project_dir, "r_trans", parcels=parcels)
    rt_df = parcels$r_trans
    if (is.null(rt_df)) {
      stop("\nNo r_trans parcel exists generate it.")
    }
    if (!is.null(rt_df)) {
      run_df = remove_cols(run_df, "rcode")
      run_df = left_join(run_df, rt_df, by="runid") %>%
        mutate(rcode = na.val(rcode, ""))
    }
  }

  run_df
}

# Needs to be refined. We do not yet know how to handle regressions
# that modify the data set.
# We probably need a special data modification translation
drf_add_path_df_cols_for_cache = function(path_df = drf$path_df, run_df=drf$run_df, drf=NULL, overwrite=FALSE) {
  restore.point("drf_mark_path_df_is_load_mod")
  if (!overwrite) {
    if (has_col(path_df, "is_load_mod")) return(path_df)
  }
  cols = setdiff(names(path_df), c("cmd_type","found_path", "is_xi", "is_I_reg","is_load_mod"))
  path_df = path_df[,cols]
  path_df %>%
    left_join(run_df %>% select(runid, cmd_type,  found_path, is_xi, is_I_reg), by="runid") %>%
    mutate(is_load_mod = cmd_type %in% c("load","mod","merge"))

}

drf_require = function(cond, msg, drf) {
  if (!isTRUE(all(cond))) {
    stop(msg)
  }
}
```
!END_MODIFICATION drf.R

### 4. Update `repboxDRF/R/drf_stata_data.R`

!MODIFICATION drf_stata_data.R
scope = "file"
file = "/home/rstudio/repbox/repboxDRF/R/drf_stata_data.R"
is_new_file = false
description = "Introduce explicit handling of intermediate data paths and copying."
---
```R
# Functions related to data set
# already needed when generating Stata code
drf_run_df_add_data_paths = function(run_df = drf$run_df, drf_dir = drf$drf_dir, drf = NULL) {
  restore.point("drf_run_df_add_data_paths")
  project_dir = drf$project_dir

  im_file = file.path(project_dir, "repbox", "stata", "intermediate_data.Rds")
  if (file.exists(im_file)) {
    im_df = readRDS(im_file)
  } else {
    im_df = NULL
  }

  run_df$is_intermediate = FALSE
  run_df$im_source_path = NA_character_
  run_df$org_data_path = NA_character_

  for (i in seq_len(NROW(run_df))) {
    fp = run_df$found_path[i]
    if (!nzchar(fp) || is.na(fp)) next

    is_im = FALSE
    if (!is.null(im_df) && NROW(im_df) > 0 && run_df$cmd_type[i] %in% c("load", "merge")) {
      # Find latest save of this file before current runid
      past_saves = im_df[im_df$file_path == fp & im_df$runid < run_df$runid[i], ]
      if (NROW(past_saves) > 0) {
        latest_save = past_saves[which.max(past_saves$runid), ]
        is_im = TRUE

        if (isTRUE(latest_save$has_copy)) {
          source_physical = file.path(project_dir, "repbox", "stata", "intermediate_data", latest_save$copy_path)
          dest_rel = latest_save$copy_path
        } else {
          source_physical = file.path(project_dir, "mod", latest_save$file_path)
          dest_rel = latest_save$copy_path
        }

        run_df$is_intermediate[i] = TRUE
        run_df$im_source_path[i] = source_physical
        run_df$org_data_path[i] = file.path(drf_dir, "im_data", dest_rel)
      }
    }

    if (!is_im) {
      run_df$org_data_path[i] = file.path(drf_dir, "org_data", fp)
    }
  }

  run_df$has_org_data = FALSE
  for (i in seq_len(NROW(run_df))) {
     if (is.na(run_df$org_data_path[i])) next
     if (run_df$is_intermediate[i]) {
        run_df$has_org_data[i] = file.exists(run_df$im_source_path[i])
     } else {
        run_df$has_org_data[i] = file.exists(file.path(project_dir, "mod", run_df$found_path[i])) |
                                 file.exists(file.path(project_dir, "org", run_df$found_path[i]))
     }
  }

  cache_data_path = file.path(drf_dir, "cache_dta", paste0(run_df$runid,".dta"))
  has_cache_data = file.exists(cache_data_path)
  run_df$cache_data_path = ifelse(has_cache_data, cache_data_path, NA_character_)
  run_df$has_cache_data = has_cache_data
  run_df

}


drf_data_load_needs_clear = function(run_df=NULL, cmd=run_cmd$cmd, cmd_type=run_df$cmd_type) {
  cmd_type %in% c("load")
}

# This function just replaces the data path
drf_replace_run_df_code_data_path = function(run_df =drf$run_df, drf_dir = drf$drf_dir, drf = NULL, cmd_types = c("load","merge"), rows=NULL, prefer_cache = FALSE) {
  restore.point("drf_replace_run_df_org_data_path")

  if (!has_col(run_df, "org_data_path")) {
    run_df = drf_run_df_add_data_paths(run_df, drf_dir=drf_dir, drf=drf)
  }

  if (!has_col(run_df,"code")) {
    stop("You need the column run_df$code.")
  }

  if (is.null(rows) & !is.null(cmd_types)) {
    rows = which(run_df$cmd_type %in% cmd_types)
  } else if (is.null(rows)) {
    rows = seq_len(NROW(run_df))
  }

  if (length(rows)==0) return(run_df)


  # Replace original code that load original data set
  temp = run_df[rows,]
  org_rows = rows[run_df$has_org_data[rows]]
  add_clear = drf_data_load_needs_clear(run_df[org_rows,])
  run_df$code[org_rows] = replace_stata_cmdline_path(run_df$code[org_rows], paste0('"',run_df$org_data_path[org_rows],'"'),add_clear=add_clear)

  # TO DO: cache replace:
  # If we load a cache, we load the cache file with a complete new load command
  # e.g. use "cachefile.dta", clear

  # ...
  run_df
}

drf_copy_org_data = function(project_dir=drf$project_dir, run_df=drf$run_df, runids=drf_runids(drf),  move_from_mod=TRUE, drf=NULL, overwrite=FALSE) {
  restore.point("drf_copy_org_data")

  require_project_dir(project_dir)
  if (!dir.exists(project_dir)) stop()

  load_df = run_df %>%
    filter(runid %in% runids, cmd_type %in% c("load","merge")) %>%
    filter(nzchar(found_path) & !is.na(found_path)) %>%
    select(found_path, org_data_path, is_intermediate, im_source_path) %>%
    distinct()

  if (NROW(load_df) == 0) return(invisible(NULL))

  # 1. Copy intermediate data
  im_df = load_df %>% filter(is_intermediate)
  if (NROW(im_df) > 0) {
    for (i in seq_len(NROW(im_df))) {
      src = im_df$im_source_path[i]
      dest = im_df$org_data_path[i]
      if (!overwrite && file.exists(dest)) next
      if (!file.exists(src)) {
        cat("\nIntermediate data source not found: ", src)
        next
      }
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      if (move_from_mod && startsWith(src, file.path(project_dir, "mod"))) {
        file.rename(src, dest)
      } else {
        file.copy(src, dest, overwrite = overwrite)
      }
    }
  }

  # 2. Copy original data
  org_df = load_df %>% filter(!is_intermediate)
  if (NROW(org_df) > 0) {
    dest_files = org_df$org_data_path
    data_files = org_df$found_path

    if (!overwrite) {
      has = file.exists(dest_files)
      dest_files = dest_files[!has]
      data_files = data_files[!has]
    }

    if (length(dest_files) > 0) {
      dirs = unique(dirname(dest_files))
      for (d in dirs) {
        dir.create(d, recursive=TRUE, showWarnings = FALSE)
      }

      mod_files = file.path(project_dir, "mod", data_files)
      has = file.exists(mod_files)
      if (any(has)) {
        if (move_from_mod) {
          file.rename(mod_files[has], dest_files[has])
        } else {
          file.copy(mod_files[has], dest_files[has], overwrite=overwrite)
        }
      }

      still_missing = !file.exists(dest_files)
      if (any(still_missing)) {
        org_files = file.path(project_dir, "org", data_files[still_missing])
        has = file.exists(org_files)
        if (any(has)) {
          file.copy(org_files[has], dest_files[still_missing][has], overwrite=overwrite)
        }
      }

      final_missing = !file.exists(dest_files)
      if (any(final_missing)) {
        cat("\nThe data set(s) ", paste0(data_files[final_missing], collapse=", "),
            " were not found in mod or org folder and could not be copied to drf/org_data.")
      }
    }
  }

  return(invisible(NULL))
}
```
!END_MODIFICATION drf_stata_data.R


### 5. Update `repboxDRF/R/drf_r_code.R`

!MODIFICATION drf_r_code.R
scope = "file"
file = "repboxDRF/R/drf_r_code.R"
is_new_file = false
description = "Translate load paths specifically according to intermediate/original classification."
---
```R
example = function() {
  # Should point to this project dir
  project_dir = "~/repbox/projects/aejapp_11_2_10"
  project_dir = "~/repbox/projects/test"

  if (FALSE)
    rstudioapi::filesPaneNavigate(project_dir)

  drf = drf_load(project_dir)
  drf$sc_df = drf_stata_code_df(drf, path_merge = "load_natural")
  drf$rc_df = drf_rcode_df(drf)
}


drf_make_r_trans_parcel = function(drf) {
  restore.point("drf_make_r_trans_parcel")
  run_df = drf_run_df_create_rcode(drf=drf)
  rt_df = run_df %>%
    filter(rcode != "") %>%
    select(runid, rcode)

  drf$parcels[["r_trans"]] = rt_df
  repdb_save_parcels(drf$parcels["r_trans"],file.path(drf$project_dir, "repdb"),check=TRUE)
  drf

}

# Writes stata code skeleton for direct replication of one or
# multiple regression commands
# The regression commands themselves will be palceholder of form
# {{runid-3562}}

# TO DO: omit unneccesary previous reg steps.
# They are currently always included in path since
# later regressions may need them if r() or something is used from it.



drf_run_df_create_rcode = function(run_df=drf$run_df, runids=drf_runids(drf), drf=NULL) {
  restore.point("drf_run_df_create_rcode")

  if (!has_col(run_df, "rcode")) {
    run_df$rcode = rep("", NROW(run_df))
  }
  if (!is.null(runids)) {
    rows = match(runids, run_df$runid)
  } else {
    rows = seq_len(NROW(run_df))
  }
  rows = sort(unique(rows[!is.na(rows)]))

  update_rows = rows
  # if (!overwrite) {
  #   cat("\ndrf_run_df_create_rcode with ovewrite=FALSE might be problematic with respect to row indices. Better make just a single translation.\n")
  #   update_rows = rows[run_df$rcode[rows] == ""]
  # }

  if (length(update_rows)==0) return(run_df)

  # Translate the execution trace up to the max row so `stata2r` maintains full context
  #max_row = max(update_rows)
  stata_code = run_df$cmdline[update_rows]

  # IMPORTANT: Replace internal newlines with spaces to keep 1-to-1 mapping with rows
  stata_code = gsub("\n", " ", stata_code, fixed = TRUE)

  r_df = stata2r::do_to_r(stata_code, return_df = TRUE)

  translated_code = r_df$r_code
  run_df$rcode[update_rows] = ifelse(is.na(translated_code), "", translated_code)

  # Overwrite 'load' commands with repbox's own data loading logic
  inds = update_rows[run_df$cmd_type[update_rows] %in% c("load")]
  if (length(inds)>0) {
    drf_rel_path = ifelse(run_df$is_intermediate[inds],
                          paste0("im_data/", sub("^.*?im_data/", "", run_df$org_data_path[inds])),
                          paste0("org_data/", run_df$found_path[inds]))
    code = paste0(
      'data = drf_load_data(project_dir, "', drf_rel_path ,'")\n',
      'data$stata2r_original_order_idx = seq_len(nrow(data))\n',
      'assign("has_original_order_idx", TRUE, envir = stata2r::stata2r_env)'
    )
    run_df$rcode[inds] = code
  }
  run_df$rcode = na.val(run_df$rcode, "")

  run_df
}


drf_rcode_df = function(drf,runids=NULL, path_merge = c("none", "load", "natural", "load_natural")[4], update_rcode = FALSE) {
  restore.point("drf_rcode_df")

  # perform path merge like as for stata code
  sc_df = drf_stata_code_df(drf, runids=runids, path_merge=path_merge)
  runids = unique(rc_df$runid)

  run_df = drf$run_df
  if (update_rcode) {
    run_df = drf_run_df_create_rcode(run_df, runids=runids)
  }

  run_df = drf$run_df %>%
    filter(runid %in% runids)

  rc_df = rc_df %>%
    left_join(run_df %>% select(runid, cmdline,rcode), by="runid") %>%
    mutate(code = rcode, pre = "", post="")
  rc_df
}
```
!END_MODIFICATION drf_r_code.R

### 6. Update `repboxDRF/R/drf_r_data.R`

!MODIFICATION drf_r_data.R
scope = "file"
file = "repboxDRF/R/drf_r_data.R"
is_new_file = false
description = "Load paths conditionally searching both generic and org_data DRF structures."
---
```R
# Data loading and cache for R metareg analysis

# Main aspect is caching

# We can have two types of caches:
# cache in memory mcache
# cache files fcache


# For debugging purposes helpful but may destroy some
# Stata pipeline so don't add by default
drf_set_add_org_row = function(add_org_row=TRUE) {
  options(repbox_data_add_org_row=add_org_row)
  return(add_org_row)
}

drf_load_data = function(project_dir, rel_path, ..., add_org_row=isTRUE(getOption("repbox_data_add_org_row"))) {
  restore.point("repbox_load_data")
  data = drf_cached_data(file=rel_path, project_dir=project_dir)
  if (!is.null(data)) return(data)
  
  file = file.path(project_dir, "drf", rel_path)
  if (!file.exists(file)) {
    file = file.path(project_dir, "drf/org_data", rel_path)
  }
  
  if (!file.exists(file)) {
    stop(paste0("The original data set ", rel_path, " is not yet stored in the DRF folder ", dirname(file)))
  }

  ext = tolower(tools::file_ext(file))
  if (ext=="dta") {
    data = haven::read_dta(file)
  } else if (ext=="rds") {
    data = readRDS(file)
  } else {
    data = rio::import(file)
  }

  if (isTRUE(add_org_row)) {
    data = bind_cols(tibble(.ORG.ROW = seq_len(NROW(data))), data)
  }

  drf_store_if_mcache_cand(data, file=rel_path, project_dir=project_dir)

  data
}
```
!END_MODIFICATION drf_r_data.R
