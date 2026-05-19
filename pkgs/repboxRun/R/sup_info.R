sup_save_basic_info = function(project_dir, parcels=list()) {
  restore.point("sup_save_basic_info")

  artid = basename(project_dir)

  # 1. Store info about all files
  file_df = repbox_get_org_sup_files(project_dir)

  file_info = file_df %>%
    mutate(artid = artid)

  # 2. Specific info for common file types
  file_type_info = file_info %>%
    filter(file_type %in% special_file_types()) %>%
    group_by(file_type) %>%
    summarize(
      artid = first(artid),
      num_files = n(),
      mb = sum(mb, na.rm=TRUE),
      latest_timestamp = suppressWarnings(max(timestamp))
    )


  # 3. Overview info over supplement
  meta_file = file.path(project_dir, "meta", "sup_meta.Rds")
  if (!file.exists(meta_file)) {
    sup = list()
  } else {
    sup = readRDS(meta_file)
  }
  sup = set_missing_fields(
    sup,
    artid = artid,
    license_type = NA,
    has_license_file = NA,
    sup_version = NA,
    sup_url = NA,
    sup_doi = NA,
    sup_repo = NA
  )
  sup$has_r = any(file_type_info$file_type=="r")
  sup$has_stata = any(file_type_info$file_type=="do")
  sup$mb = sum(file_info$mb)
  sup$latest_timestamp = max(file_info$timestamp)

  sup = as_tibble(sup)

  parcels$sup = list(sup=sup)
  parcels$file_info = list(file_info=file_info)
  parcels$file_type_info = list(file_type_info=file_type_info)
  repboxDB::repdb_save_parcels(parcels[c("sup", "file_info","file_type_info")], file.path(project_dir, "repdb"))

  parcels
}
