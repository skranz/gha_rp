#' Create a ZIP file from the current org folder
#'
#' This is useful for Github Actions test runs if no original supplement
#' ZIP file is available anymore, but the project still has a complete
#' /org directory.
#'
#' @export
rb_make_org_zip = function(
  project_dir,
  zip_file = file.path(project_dir, "rp_zip", paste0(basename(project_dir), "_org.zip")),
  overwrite = FALSE,
  org_dir = file.path(project_dir, "org")
) {
  restore.point("rb_make_org_zip")

  project_dir = normalizePath(project_dir, mustWork = FALSE)
  org_dir = normalizePath(org_dir, mustWork = TRUE)

  if (!dir.exists(org_dir)) {
    stop("Cannot create supplement ZIP because the org directory does not exist.")
  }

  dir.create(dirname(zip_file), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(zip_file) && !overwrite) {
    return(normalizePath(zip_file, mustWork = TRUE))
  }
  if (file.exists(zip_file) && overwrite) {
    file.remove(zip_file)
  }

  oldwd = getwd()
  on.exit(setwd(oldwd), add = TRUE)
  setwd(org_dir)

  files = list.files(
    path = ".",
    recursive = TRUE,
    full.names = FALSE,
    all.files = TRUE,
    no.. = TRUE,
    include.dirs = FALSE
  )

  if (length(files) == 0) {
    stop("Cannot create supplement ZIP because the org directory is empty.")
  }

  utils::zip(
    zipfile = zip_file,
    files = files,
    flags = "-9Xq"
  )

  normalizePath(zip_file, mustWork = TRUE)
}

#' Return an available supplement ZIP or create one from /org
#'
#' The function first checks an explicitly supplied ZIP file, then the
#' repbox-side ZIP lookup, and finally can create a ZIP from /org.
#'
#' @export
rb_ensure_sup_zip = function(
  project_dir,
  sup_zip = NULL,
  zip_file = file.path(project_dir, "rp_zip", paste0(basename(project_dir), "_org.zip")),
  overwrite = FALSE,
  create_from_org = TRUE
) {
  restore.point("rb_ensure_sup_zip")

  project_dir = normalizePath(project_dir, mustWork = FALSE)

  if (!is.null(sup_zip)) {
    if (length(sup_zip) == 0 || is.na(sup_zip[1]) || !file.exists(sup_zip[1])) {
      stop("The supplied sup_zip does not exist.")
    }
    return(normalizePath(sup_zip[1], mustWork = TRUE))
  }

  guessed_zip = try(rb_get_sup_zip(project_dir), silent = TRUE)
  if (!inherits(guessed_zip, "try-error") &&
      length(guessed_zip) > 0 &&
      !is.na(guessed_zip[1]) &&
      file.exists(guessed_zip[1])) {
    return(normalizePath(guessed_zip[1], mustWork = TRUE))
  }

  if (!create_from_org) {
    return(NULL)
  }

  rb_make_org_zip(
    project_dir = project_dir,
    zip_file = zip_file,
    overwrite = overwrite
  )
}
