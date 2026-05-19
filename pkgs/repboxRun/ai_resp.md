Change **two functions** in:

```r
/home/rstudio/repbox/repboxRun/R/rb_gha.R
```

First, make the GHA bundle include selected `repdb/*.Rds` parcels, especially `stata_scalar.Rds`.

!MODIFICATION rb_make_gha_stata_bundle in rb_gha.R
scope = "function"
file = "/home/rstudio/repbox/repboxRun/R/rb_gha.R"
function_name = "rb_make_gha_stata_bundle"
description = "Copy selected repdb parcels, including stata_scalar.Rds, into the Github Actions result bundle."
---------------------------------------------------------------------------------------------------------------

```r
#' Create a narrow Github Actions bundle for the raw Stata run
#'
#' The bundle contains repbox/stata, steps, problems, selected repdb
#' parcels from the raw run, a copy of the raw run manifest, generated
#' or changed files from /mod that are not already available in /org,
#' and intermediate_data.
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
  treat_same_name_same_size_as_existing = TRUE,
  include_repdb_parcels = c("stata_scalar")
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

  if (length(include_repdb_parcels) > 0) {
    repdb_files = file.path(
      project_dir,
      "repdb",
      paste0(include_repdb_parcels, ".Rds")
    )
    repdb_files = repdb_files[file.exists(repdb_files)]

    if (length(repdb_files) > 0) {
      bundle_repdb_dir = file.path(bundle_dir, "repdb")
      dir.create(bundle_repdb_dir, recursive = TRUE, showWarnings = FALSE)

      ok = file.copy(
        from = repdb_files,
        to = file.path(bundle_repdb_dir, basename(repdb_files)),
        overwrite = TRUE,
        copy.mode = TRUE,
        copy.date = TRUE
      )

      if (!all(ok)) {
        stop("Could not copy all selected repdb parcels to the Github Actions bundle.")
      }
    }
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

  # Deprecated root intermediate_data folder mapping retained in case old setups exist.
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
```

!END_MODIFICATION rb_make_gha_stata_bundle in rb_gha.R

Second, make the local import restore `bundle/repdb/*.Rds` into the project’s local `repdb`.

!MODIFICATION rb_import_gha_stata_bundle in rb_gha.R
scope = "function"
file = "/home/rstudio/repbox/repboxRun/R/rb_gha.R"
function_name = "rb_import_gha_stata_bundle"
description = "Import bundled repdb parcels such as stata_scalar.Rds back into the local project."
--------------------------------------------------------------------------------------------------

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
  import_mod_files = TRUE,
  import_intermediate_data = TRUE,
  restore_intermediate_data_to_mod = TRUE,
  import_repdb_parcels = TRUE
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

  imported_repdb_files = character(0)
  if (isTRUE(import_repdb_parcels)) {
    repdb_from = file.path(bundle_dir, "repdb")
    if (dir.exists(repdb_from)) {
      rb_copy_dir_contents(
        from_dir = repdb_from,
        to_dir = file.path(project_dir, "repdb"),
        overwrite = overwrite
      )

      imported_repdb_files = list.files(
        path = repdb_from,
        recursive = TRUE,
        full.names = FALSE,
        all.files = TRUE,
        no.. = TRUE
      )
    }
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
    imported_repdb_files = imported_repdb_files,
    imported_mod_files = imported_mod_files,
    imported_intermediate_files = imported_intermediate_files
  ))
}
```

!END_MODIFICATION rb_import_gha_stata_bundle in rb_gha.R

The key new block is this one inside `rb_make_gha_stata_bundle()`, after copying `problems` and before copying generated `mod` files:

```r
if (length(include_repdb_parcels) > 0) {
  repdb_files = file.path(
    project_dir,
    "repdb",
    paste0(include_repdb_parcels, ".Rds")
  )
  repdb_files = repdb_files[file.exists(repdb_files)]

  if (length(repdb_files) > 0) {
    bundle_repdb_dir = file.path(bundle_dir, "repdb")
    dir.create(bundle_repdb_dir, recursive = TRUE, showWarnings = FALSE)

    ok = file.copy(
      from = repdb_files,
      to = file.path(bundle_repdb_dir, basename(repdb_files)),
      overwrite = TRUE,
      copy.mode = TRUE,
      copy.date = TRUE
    )

    if (!all(ok)) {
      stop("Could not copy all selected repdb parcels to the Github Actions bundle.")
    }
  }
}
```

With the default

```r
include_repdb_parcels = c("stata_scalar")
```

the GHA artifact will contain:

```r
repdb/stata_scalar.Rds
```

and the import function will copy it back to:

```r
<local project>/repdb/stata_scalar.Rds
```
