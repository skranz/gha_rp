# copies an R package source, used to create local packages
# in repbox github action repo for faster testing when packages change
# (pure Github install complicated, r-universe takes much time)
copy_r_package = function(source_dir,
                        dest_parent_dir,
                        copy_hidden_dirs = FALSE,
                        overwrite = FALSE) {
  if (!requireNamespace("stringi", quietly = TRUE)) {
    stop("Package 'stringi' is required.")
  }

  if (!dir.exists(source_dir)) {
    stop("source_dir does not exist or is not a directory: ", source_dir)
  }

  if (!dir.exists(dest_parent_dir)) {
    dir.create(dest_parent_dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (!dir.exists(dest_parent_dir)) {
    stop("Could not create dest_parent_dir: ", dest_parent_dir)
  }

  source_dir = normalizePath(source_dir, winslash = "/", mustWork = TRUE)
  dest_parent_dir = normalizePath(dest_parent_dir, winslash = "/", mustWork = TRUE)

  package_name = basename(source_dir)
  dest_dir = file.path(dest_parent_dir, package_name)

  if (dir.exists(dest_dir)) {
    dest_dir_norm = normalizePath(dest_dir, winslash = "/", mustWork = TRUE)

    if (identical(dest_dir_norm, source_dir)) {
      stop("Destination directory must not be the same as source_dir")
    }

    if (!overwrite) {
      stop("Destination already exists: ", dest_dir)
    }

    try(delete_copied_r_package(dest_dir))
  } else {
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (!dir.exists(dest_dir)) {
    stop("Failed to create destination directory: ", dest_dir)
  }

  rel_paths = list.files(
    path = source_dir,
    all.files = TRUE,
    recursive = TRUE,
    include.dirs = TRUE,
    no.. = TRUE
  )

  if (length(rel_paths) == 0L) {
    return(invisible(dest_dir))
  }

  full_paths = file.path(source_dir, rel_paths)
  info = file.info(full_paths)

  if (anyNA(info$isdir)) {
    stop("Could not inspect some paths under source_dir.")
  }

  if (!copy_hidden_dirs) {
    path_parts = stringi::stri_split_regex(rel_paths, "[/\\\\]+")

    in_hidden_dir = vapply(seq_along(path_parts), function(i) {
      parts = path_parts[[i]]

      dir_parts = if (isTRUE(info$isdir[i])) {
        parts
      } else {
        head(parts, -1L)
      }

      if (length(dir_parts) == 0L) {
        return(FALSE)
      }

      any(stringi::stri_detect_regex(dir_parts, "^\\."))
    }, logical(1))

    rel_paths = rel_paths[!in_hidden_dir]
    full_paths = full_paths[!in_hidden_dir]
    info = info[!in_hidden_dir, , drop = FALSE]
  }

  if (length(rel_paths) == 0L) {
    return(invisible(dest_dir))
  }

  target_paths = file.path(dest_dir, rel_paths)

  target_dirs = unique(c(
    target_paths[info$isdir],
    dirname(target_paths[!info$isdir])
  ))

  for (d in target_dirs) {
    if (!dir.exists(d)) {
      dir.create(d, recursive = TRUE, showWarnings = FALSE)
    }
  }

  file_mask = !info$isdir

  if (any(file_mask)) {
    ok = file.copy(
      from = full_paths[file_mask],
      to = target_paths[file_mask],
      overwrite = TRUE,
      copy.mode = TRUE,
      copy.date = TRUE
    )

    if (!all(ok)) {
      failed = rel_paths[file_mask][!ok]
      stop(
        "Failed to copy these files: ",
        paste(failed, collapse = ", ")
      )
    }
  }

  invisible(dest_dir)
}

delete_copied_r_package = function(dest_dir) {
  restore.point("delete_copied_r_package")
  #stop()
  if (!requireNamespace("stringi", quietly = TRUE)) {
    stop("Package 'stringi' is required.")
  }

  if (!dir.exists(dest_dir)) {
    stop("dest_dir does not exist or is not a directory: ", dest_dir)
  }

  dest_dir = normalizePath(dest_dir, winslash = "/", mustWork = TRUE)



  if (dest_dir %in% c("/", ".")) {
    stop("Refusing to operate on unsafe dest_dir: ", dest_dir)
  }

  description_path = file.path(dest_dir, "DESCRIPTION")
  r_dir = file.path(dest_dir, "R")
  git_dir = file.path(dest_dir, ".git")
  rstudio_dir = file.path(dest_dir, ".rstudio")
  namespace_path = file.path(dest_dir, "NAMESPACE")

  if (!file.exists(description_path)) {
    stop("Refusing to delete: missing DESCRIPTION in ", dest_dir)
  }

  if (!dir.exists(r_dir)) {
    stop("Refusing to delete: missing R directory in ", dest_dir)
  }

  if (dir.exists(git_dir)) {
    stop("Refusing to delete: .git directory exists in ", dest_dir)
  }

  if (dir.exists(rstudio_dir)) {
    stop("Refusing to delete: .rstudio directory exists in ", dest_dir)
  }

  desc = tryCatch(
    read.dcf(description_path),
    error = function(e) NULL
  )

  if (is.null(desc) || !"Package" %in% colnames(desc)) {
    stop("Refusing to delete: DESCRIPTION does not contain a Package field in ", dest_dir)
  }

  desc_pkg = as.character(desc[1, "Package"])
  dir_pkg = basename(dest_dir)

  if (!identical(desc_pkg, dir_pkg)) {
    stop(
      "Refusing to delete: DESCRIPTION Package field ('", desc_pkg,
      "') does not match directory name ('", dir_pkg, "')"
    )
  }

  has_package_markers = any(c(
    dir.exists(file.path(dest_dir, "man")),
    dir.exists(file.path(dest_dir, "tests")),
    dir.exists(file.path(dest_dir, "inst")),
    file.exists(namespace_path)
  ))

  if (!has_package_markers) {
    stop(
      "Refusing to delete: directory does not look enough like an R package copy: ",
      dest_dir
    )
  }

  entries = list.files(dest_dir, recursive = TRUE, no.. = TRUE,full.names = TRUE, include.dirs = FALSE)

  if (length(entries) == 0L) {
    return(invisible(dest_dir))
  }

  paths_to_delete = entries

  ok = file.remove(paths_to_delete)


  remaining = list.files(dest_dir, recursive = TRUE, no.. = TRUE,full.names = TRUE, include.dirs = FALSE)


  if (length(remaining) > 0L) {
    stop(
      "Deletion incomplete, remaining entries in ", dest_dir, ": ",
      paste(remaining, collapse = ", ")
    )
  }

  invisible(dest_dir)
}

