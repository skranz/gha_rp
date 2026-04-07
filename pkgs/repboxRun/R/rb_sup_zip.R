rb_get_file_info = function(project_dir) {
  parcel = read_rds_or_null(file.path(project_dir, "repdb/file_info.Rds"))
  parcel$file_info
}

rb_has_sup_zip = function(project_dir) {
  !is.empty(rb_find_zip_file(project_dir))
}

rb_get_sup_zip = function(project_dir) {
  artid = basename(project_dir)
  if (!require(repboxEJD,quietly = TRUE)) {
    cat("\nCannot get sup_zip repboxEJD is not installed.")
    return(NULL)
  }

  repboxEJD::ejd_find_downloaded_sup_zip(artid=artid)
}

rb_extract_zip_to_org = function(project_dir, zip_file=rb_get_sup_zip(project_dir), ...) {
  restore.point("rb_extract_zip_file")
  if (is.empty(zip_file)) return()
  repbox_sup_extract_zip(project_dir,sup_zip = zip_file,just_extract_code = FALSE)
}

