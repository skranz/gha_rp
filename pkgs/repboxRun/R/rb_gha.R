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

  if (!file.exists(from)) return(FALSE)

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
    recursive = TRUE,
    copy.mode = TRUE,
    copy.date = TRUE,
    overwrite = overwrite
  )

  if (!all(ok)) {
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
