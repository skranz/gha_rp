rb_mod_file_df = function(
  dir,
  ignore_repbox_files = FALSE,
  repbox_prefix = "repbox_"
) {
  empty = data.frame(
    rel_path = character(0),
    abs_path = character(0),
    base = character(0),
    size = numeric(0),
    stringsAsFactors = FALSE
  )

  if (!dir.exists(dir)) return(empty)

  rel_path = list.files(
    path = dir,
    recursive = TRUE,
    full.names = FALSE,
    include.dirs = FALSE,
    all.files = TRUE,
    no.. = TRUE
  )

  if (length(rel_path) == 0) return(empty)

  rel_path = stringi::stri_replace_all_fixed(rel_path, "\\", "/")
  abs_path = file.path(dir, rel_path)
  fi = file.info(abs_path)

  keep = !is.na(fi$isdir) & !fi$isdir
  rel_path = rel_path[keep]
  abs_path = abs_path[keep]
  fi = fi[keep, , drop = FALSE]

  if (length(rel_path) == 0) return(empty)

  base = basename(rel_path)

  if (isTRUE(ignore_repbox_files)) {
    keep = !stringi::stri_startswith_fixed(base, repbox_prefix)
    rel_path = rel_path[keep]
    abs_path = abs_path[keep]
    fi = fi[keep, , drop = FALSE]
    base = base[keep]
  }

  if (length(rel_path) == 0) return(empty)

  data.frame(
    rel_path = rel_path,
    abs_path = normalizePath(abs_path, mustWork = FALSE),
    base = base,
    size = as.numeric(fi$size),
    stringsAsFactors = FALSE
  )
}

rb_find_generated_mod_files = function(
  project_dir,
  mod_dir = file.path(project_dir, "mod"),
  org_dir = file.path(project_dir, "org"),
  ignore_repbox_files = TRUE,
  repbox_prefix = "repbox_",
  compare_relative_path = TRUE,
  treat_same_name_same_size_as_existing = TRUE,
  only_generated = TRUE
) {
  mod_df = rb_mod_file_df(
    dir = mod_dir,
    ignore_repbox_files = ignore_repbox_files,
    repbox_prefix = repbox_prefix
  )
  org_df = rb_mod_file_df(
    dir = org_dir,
    ignore_repbox_files = ignore_repbox_files,
    repbox_prefix = repbox_prefix
  )

  if (NROW(mod_df) == 0) {
    mod_df$has_same_rel_path = logical(0)
    mod_df$org_rel_path = character(0)
    mod_df$same_rel_path_same_size = logical(0)
    mod_df$same_name_same_size = logical(0)
    mod_df$name_size_match_org_rel_path = character(0)
    mod_df$is_generated = logical(0)
    mod_df$change_type = character(0)
    return(mod_df)
  }

  rel_match = rep(NA_integer_, NROW(mod_df))
  has_same_rel_path = rep(FALSE, NROW(mod_df))
  same_rel_path_same_size = rep(FALSE, NROW(mod_df))

  if (isTRUE(compare_relative_path) && NROW(org_df) > 0) {
    rel_match = match(mod_df$rel_path, org_df$rel_path)
    has_same_rel_path = !is.na(rel_match)
    same_rel_path_same_size[has_same_rel_path] =
      mod_df$size[has_same_rel_path] == org_df$size[rel_match[has_same_rel_path]]
  }

  changed_existing = has_same_rel_path & !same_rel_path_same_size

  same_name_same_size = rep(FALSE, NROW(mod_df))
  name_size_match_org_rel_path = rep(NA_character_, NROW(mod_df))

  if (isTRUE(treat_same_name_same_size_as_existing) && NROW(org_df) > 0) {
    org_key = paste0(org_df$base, "\r", org_df$size)
    mod_key = paste0(mod_df$base, "\r", mod_df$size)

    key_match = match(mod_key, org_key)
    same_name_same_size = !is.na(key_match)
    name_size_match_org_rel_path[same_name_same_size] =
      org_df$rel_path[key_match[same_name_same_size]]
  }

  is_generated =
    changed_existing |
    (
      !has_same_rel_path &
      (
        !isTRUE(treat_same_name_same_size_as_existing) |
          !same_name_same_size
      )
    )

  change_type = ifelse(
    same_rel_path_same_size,
    "same_rel_path_same_size",
    ifelse(
      changed_existing,
      "changed_existing",
      ifelse(
        !has_same_rel_path & same_name_same_size,
        "matched_by_name_size",
        "new_file"
      )
    )
  )

  res = mod_df
  res$has_same_rel_path = has_same_rel_path
  res$org_rel_path = ifelse(
    has_same_rel_path,
    org_df$rel_path[rel_match],
    NA_character_
  )
  res$same_rel_path_same_size = same_rel_path_same_size
  res$same_name_same_size = same_name_same_size
  res$name_size_match_org_rel_path = name_size_match_org_rel_path
  res$is_generated = is_generated
  res$change_type = change_type

  res = res[order(res$rel_path), , drop = FALSE]
  rownames(res) = NULL

  if (isTRUE(only_generated)) {
    res = res[res$is_generated, , drop = FALSE]
    rownames(res) = NULL
  }

  res
}

rb_copy_generated_mod_files_to_dir = function(
  project_dir,
  to_dir,
  file_df = NULL,
  overwrite = TRUE,
  mod_dir = file.path(project_dir, "mod"),
  org_dir = file.path(project_dir, "org"),
  ignore_repbox_files = TRUE,
  repbox_prefix = "repbox_",
  compare_relative_path = TRUE,
  treat_same_name_same_size_as_existing = TRUE
) {
  if (is.null(file_df)) {
    file_df = rb_find_generated_mod_files(
      project_dir = project_dir,
      mod_dir = mod_dir,
      org_dir = org_dir,
      ignore_repbox_files = ignore_repbox_files,
      repbox_prefix = repbox_prefix,
      compare_relative_path = compare_relative_path,
      treat_same_name_same_size_as_existing = treat_same_name_same_size_as_existing,
      only_generated = TRUE
    )
  }

  if (NROW(file_df) == 0) {
    return(invisible(file_df))
  }

  required_cols = c("rel_path", "abs_path")
  if (!all(required_cols %in% names(file_df))) {
    stop("file_df must contain columns rel_path and abs_path.")
  }

  dest_files = file.path(to_dir, file_df$rel_path)
  dest_dirs = unique(dirname(dest_files))

  invisible(vapply(
    X = dest_dirs,
    FUN = dir.create,
    recursive = TRUE,
    showWarnings = FALSE,
    FUN.VALUE = logical(1)
  ))

  ok = file.copy(
    from = file_df$abs_path,
    to = dest_files,
    overwrite = overwrite,
    copy.mode = TRUE,
    copy.date = TRUE
  )

  if (!all(ok)) {
    stop(paste0("Could not copy all generated mod files to ", to_dir, "."))
  }

  invisible(file_df)
}
