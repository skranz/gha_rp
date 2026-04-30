example = function() {
  library(repboxRun)

  project_dir = rb_get_project_dir("~/repbox/projects/test")

  if (FALSE) {
    rstudioapi::filesPaneNavigate(project_dir)
  }

  rb = rb_new(project_dir)
  rb = rb_update_file_info_parcel(rb, overwrite = FALSE, assume_org_complete = TRUE)
  rb = rb_update_script_parcels(rb, overwrite = FALSE)
  rb = rb_update_static_code_analysis(rb, overwrite = FALSE)

  rb = rb_run_stata_reproduction_raw(
    rb = rb,
    overwrite = FALSE,
    create_mod_dir = TRUE,
    capture_reg_info = TRUE,
    capture_scalars = TRUE
  )

  rb = rb_postprocess_stata_reproduction(
    rb = rb,
    overwrite = FALSE,
    build_run_info = TRUE,
    build_do_run_info = TRUE,
    build_reg_info = TRUE,
    build_drf = TRUE
  )

  rb
}

rb_create_mod_dir = function(rb) {
  restore.point("rb_update_mod_dir")
  rb_require_complete_org_dir(rb)

  project_dir = rb$project_dir
  if (is.null(project_dir)) stop("No project_dir")

  mod_dir = file.path(project_dir, "mod")
  org_dir = file.path(project_dir, "org")
  if (dir.exists(mod_dir)) {
    remove.dir(mod_dir, must.contain = project_dir)
  }

  copy.dir(org_dir, mod_dir, copy.date = TRUE)
  unzip.zips(mod_dir)
  make.project.files.info(project_dir, for.mod = TRUE, for.org = TRUE)
  rb
}

rb_slimify_org_dir = function(rb) {
  slimify.org.dir(rb$project_dir)
}

rb_has_stata_raw_reproduction = function(rb, project_dir = rb$project_dir) {
  file.exists(file.path(project_dir, "repbox", "stata", "repbox_results.Rds"))
}

rb_has_stata_postprocess = function(rb, project_dir = rb$project_dir) {
  file.exists(file.path(project_dir, "repdb", "stata_run_info.Rds"))
}

rb_has_stata_reproduction = function(rb, project_dir = rb$project_dir) {
  rb_has_stata_postprocess(rb = rb, project_dir = project_dir)
}

#' Run the raw Stata reproduction only
#'
#' This function is designed for the remote Github Actions run.
#' It executes the original Stata code with injections and stores the
#' raw Stata outputs in /repbox/stata, but does not create local-only
#' derivatives such as DRF or repdb postprocess outputs.
#'
#' @export
rb_run_stata_reproduction_raw = function(
  rb,
  overwrite = FALSE,
  create_mod_dir = TRUE,
  capture_reg_info = TRUE,
  capture_scalars = TRUE,
  stata_opts = repbox_stata_opts(
    extract.reg.info = capture_reg_info,
    extract.scalar.vals = capture_scalars
  ),
  input_zip = NULL,
  manifest_extra = list()
) {
  restore.point("rb_run_stata_reproduction_raw")
  library(repboxStata)

  rb = rb_update_file_info_parcel(
    rb = rb,
    overwrite = FALSE,
    assume_org_complete = TRUE
  )
  rb = rb_update_script_type_parcel(rb = rb, overwrite = FALSE)

  if (!rb_has_lang(rb, "stata")) {
    cat("The reproduction package has no Stata scripts.")
    return(rb)
  }

  if (!overwrite && rb_has_stata_raw_reproduction(rb)) {
    cat("\nRaw Stata reproduction already exists. Thus skipped.")
    return(rb)
  }

  if (create_mod_dir) {
    rb = rb_create_mod_dir(rb)
  }

  rb_require_complete_mod_dir(rb)
  project_dir = rb$project_dir
  if (is.null(project_dir)) stop("No project_dir")

  rb_log_step_start(rb, "stata_reproduction")

  repbox_stata_dir = file.path(project_dir, "repbox", "stata")
  if (dir.exists(repbox_stata_dir)) {
    rb_remove_dir(rb = rb, sub_dir = "repbox/stata")
  }
  dir.create(repbox_stata_dir, recursive = TRUE, showWarnings = FALSE)

  stata_opts$extract.reg.info = isTRUE(capture_reg_info)
  stata_opts$extract.scalar.vals = isTRUE(capture_scalars)

  parcels = repbox_project_run_stata(
    project_dir = project_dir,
    opts = stata_opts,
    parcels = rb$parcels
  )
  rb$parcels = parcels

  rb_write_stata_run_manifest(
    project_dir = project_dir,
    input_zip = input_zip,
    extra = manifest_extra
  )

  rb_log_step_end(rb, "stata_reproduction")
  rb
}

#' Postprocess a finished raw Stata reproduction
#'
#' This function is designed for the local machine after the raw
#' Github Actions output has been imported into the project.
#'
#' @export
rb_postprocess_stata_reproduction = function(
  rb,
  overwrite = FALSE,
  build_run_info = TRUE,
  build_do_run_info = TRUE,
  build_reg_info = TRUE,
  build_drf = TRUE
) {
  restore.point("rb_postprocess_stata_reproduction")
  library(repboxStata)
  suppressWarnings(suppressPackageStartupMessages(library(repboxStataReg)))

  project_dir = rb$project_dir
  if (is.null(project_dir)) stop("No project_dir")

  if (!rb_has_stata_raw_reproduction(rb)) {
    stop("No raw Stata reproduction found in repbox/stata.")
  }

  if (!any(c(build_run_info, build_do_run_info, build_reg_info, build_drf))) {
    return(rb)
  }

  rb_log_step_start(rb, "stata_postprocess")

  repbox_results = repbox_load_internal_repbox_results(project_dir)
  parcels = rb$parcels

  if (build_run_info || build_do_run_info) {
    parcels = repbox_save_stata_run_parcels(
      project_dir = project_dir,
      parcels = parcels,
      repbox_results = repbox_results
    )
  }

  if (build_do_run_info) {
    parcels = make_parcel_stata_do_run_info(project_dir, parcels)
  }

  if (build_reg_info) {
    suppressWarnings(suppressPackageStartupMessages(library(repboxStataReg)))
    parcels = rsr_make_reg_info(
      project_dir = project_dir,
      overwrite = overwrite,
      parcels = parcels,
      repbox_results = repbox_results
    )
  }

  rb$parcels = parcels

  if (build_drf) {
    rb$drf = repboxDRF::drf_create(
      project_dir = project_dir,
      parcels = parcels,
      overwrite = overwrite
    )
  }

  rb_log_step_end(rb, "stata_postprocess")
  rb
}

rb_run_stata_reproduction = function(
  rb,
  overwrite = FALSE,
  create_mod_dir = TRUE,
  capture_reg_info = TRUE,
  capture_scalars = TRUE,
  build_run_info = TRUE,
  build_do_run_info = TRUE,
  build_reg_info = capture_reg_info,
  build_drf = build_reg_info,
  stata_opts = repbox_stata_opts(
    extract.reg.info = capture_reg_info,
    extract.scalar.vals = capture_scalars
  ),
  input_zip = NULL,
  manifest_extra = list()
) {
  restore.point("rb_run_stata_reproduction")
  library(repboxStata)


  if (!rb_has_lang(rb, "stata")) {
    cat("The reproduction package has no Stata scripts.")
    return(rb)
  }

  if (overwrite || !rb_has_stata_raw_reproduction(rb)) {
    rb = rb_run_stata_reproduction_raw(
      rb = rb,
      overwrite = overwrite,
      create_mod_dir = create_mod_dir,
      capture_reg_info = capture_reg_info,
      capture_scalars = capture_scalars,
      stata_opts = stata_opts,
      input_zip = input_zip,
      manifest_extra = manifest_extra
    )
  } else {
    cat("\nRaw Stata reproduction already exists. Raw run skipped.\n")
  }

  needs_postprocess = overwrite ||
    !rb_has_stata_postprocess(rb) ||
    isTRUE(build_do_run_info) ||
    isTRUE(build_reg_info) ||
    isTRUE(build_drf)

  if (needs_postprocess) {
    rb = rb_postprocess_stata_reproduction(
      rb = rb,
      overwrite = overwrite,
      build_run_info = build_run_info,
      build_do_run_info = build_do_run_info,
      build_reg_info = build_reg_info,
      build_drf = build_drf
    )
  } else {
    cat("\nStata postprocess already exists. Postprocess skipped.\n")
  }

  rb
}

rb_run_r_reproduction = function(rb, overwrite = FALSE, r_opts = rb$opts$r_opts) {
  restore.point("rb_run_r_reproduction")
  rb_require_complete_mod_dir(rb)

  project_dir = rb$project_dir
  if (is.null(project_dir)) stop("No project_dir")

  rb_log_step_start(rb, "r_reproduction")

  parcels = rb$parcels
  parcels = repbox_project_run_r(project_dir, opts = r_opts, parcels = parcels)
  parcels = repbox_project_extract_r_results(project_dir, parcels, opts = r_opts)

  rb_log_step_end(rb, "r_reproduction")

  rb$parcels = parcels
  rb
}
