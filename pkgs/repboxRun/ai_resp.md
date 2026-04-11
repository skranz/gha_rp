The cleanest place to add this is in `repboxRun`: one new helper file for detecting/copying generated `mod` files, plus two bundle/import changes in `rb_gha.R`. No change is needed in `run_minimal_stata_repro.R`, because it already calls `rb_make_gha_stata_bundle()`.

!MODIFICATION rb_mod_diff.R
scope = "file"
file = "/home/rstudio/repbox/repboxRun/R/rb_mod_diff.R"
description = "Add helper functions to detect generated or changed files in mod relative to org and copy them while preserving the mod subfolder structure."
------------------------------------------------------------------------------------------------------------------------------------------------------------

```r
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
```

!END_MODIFICATION rb_mod_diff.R

!MODIFICATION rb_make_gha_stata_bundle in rb_gha.R
scope = "function"
file = "/home/rstudio/repbox/repboxRun/R/rb_gha.R"
function_name = "rb_make_gha_stata_bundle"
description = "Extend the Github Actions bundle to include generated or changed mod files detected relative to org, while preserving the mod subfolder structure."
------------------------------------------------------------------------------------------------------------------------------------------------------------------

```r
#' Create a narrow Github Actions bundle for the raw Stata run
#'
#' The bundle contains repbox/stata, steps, problems, a copy
#' of the raw run manifest, and optionally generated or changed
#' files from /mod that are not already available in /org.
#'
#' @export
rb_make_gha_stata_bundle = function(
  project_dir,
  bundle_dir = file.path(project_dir, "gha_output", "bundle"),
  overwrite = TRUE,
  input_zip = NULL,
  manifest_extra = list(),
  include_generated_mod_files = TRUE,
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

    if (NROW(generated_mod_files) > 0) {
      rb_copy_generated_mod_files_to_dir(
        project_dir = project_dir,
        to_dir = file.path(bundle_dir, "mod"),
        file_df = generated_mod_files,
        overwrite = TRUE
      )
    }
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
```

!END_MODIFICATION rb_make_gha_stata_bundle in rb_gha.R

!MODIFICATION rb_import_gha_stata_bundle in rb_gha.R
scope = "function"
file = "/home/rstudio/repbox/repboxRun/R/rb_gha.R"
function_name = "rb_import_gha_stata_bundle"
description = "Import the generated mod files from the Github Actions bundle into the local mod directory in addition to repbox/stata, steps, and problems."
------------------------------------------------------------------------------------------------------------------------------------------------------------

```r
#' Import a raw Github Actions Stata bundle into a local project
#'
#' @export
rb_import_gha_stata_bundle = function(
  project_dir,
  bundle_dir,
  overwrite = TRUE,
  verify = TRUE,
  local_input_zip = NULL,
  import_mod_files = TRUE
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

  invisible(list(
    project_dir = project_dir,
    bundle_dir = bundle_dir,
    manifest = manifest,
    imported_mod_files = imported_mod_files
  ))
}
```

!END_MODIFICATION rb_import_gha_stata_bundle in rb_gha.R

A small behavioral note: with these changes, files in `mod` are bundled if they are either at the same relative path with a different size than in `org`, or they are absent at that relative path and no `org` file with the same basename and size exists. Files whose basename starts with `repbox_` are ignored by default.
