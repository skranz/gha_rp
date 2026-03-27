rb_has_file_info = function(project_dir) {
  repdb_has_parcel(project_dir, "file_info")
}


rb_update_file_info_parcel = function(rb, overwrite=FALSE, unzip_org=TRUE, assume_org_complete=FALSE, parcels=rb$parcels) {
  restore.point("rb_update_file_info")
  project_dir = rb$project_dir
  rb$has_file_info = rb_has_file_info(project_dir)
  if (!overwrite) {
    if (rb$has_file_info) {
      return(rb)
    }
  }

  file = file.path(project_dir, "repbox","org_files.Rds")
  file_df = NULL
  if (file.exists(file) & !overwrite) {
    file_df = readRDS(file)
  } else if (!assume_org_complete & !unzip_org) {
    cat("\nNo existing file info, no unzipping of supplement ZIP file and assume_org_complete=FALSE\n")
    return(rb)
  } else if (assume_org_complete) {
  } else {
    zip_file = rb_get_sup_zip(project_dir)
    if (is_empty(zip_file) & !assume_org_complete) {
      cat("\nNo existing file info, no supplement ZIP file and assume_org_complete=FALSE\n")
      rb$has_file_info
      return(rb)
    }
    if (!is.empty(zip_file)) {
      rb = rb_load_parcels(rb, "file_info")
      if (!rb_has_complete_org_dir(rb=rb)) {
        rb_extract_zip_to_org(project_dir,zip_file)
      }
    }
  }
  if (is.null(file_df)) {
    file_df = make.project.files.info(project_dir,for.mod=FALSE)$org
  }
  file_df = file_df %>%
    transmute(
      file_path = file,
      file_type = tolower(ext),
      timestamp = mtime,
      mb = size / 1e6
    )

  rb$parcels$file_info = file_df
  repdb_save_parcels(rb$parcels["file_info"],dir = file.path(project_dir, "repdb"))
  rb$has_file_info = TRUE
  rb
}

rb_update_script_type_parcel = function(rb, overwrite=FALSE) {
  restore.point("rb_update_script_type_parcel")
  rb = rb_ensure_parcel(rb, "file_info")
  file_info = rb_parcel(rb, "file_info")
  code_ext = c("do","ado","r","mod","nb","py","m", "sas","prg", "ztt","c","java","cpp","js","f95","f90", "tsp","g","lng","gms","jl")

  df = file_info %>%
    filter(file_type %in% code_ext) %>%
    group_by(file_type) %>%
    summarize(num_files = n(), kb = sum(mb)*1000)

  rb = rb_save_parcel(rb,"script_type",dat=df)
  rb
}

rb_update_script_parcels = function(rb, overwrite=FALSE) {
  restore.point("rb_update_script_parcels")
  rb = rb_ensure_parcel(rb, "file_info")
  file_info = rb_parcel(rb, "file_info")

  rb = rb_update_script_type_parcel(rb, overwrite=overwrite)

  project_dir = rb$project_dir
  rb$parcels = repbox_make_script_parcels(project_dir,parcels=rb$parcels, file_info=file_info,overwrite = overwrite)
  rb
}

rb_load_parcels = function(rb, parcel_names) {
  rb$parcels = repdb_load_parcels(rb$project_dir, parcel_names, parcels = rb$parcels)
  rb
}
