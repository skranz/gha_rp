copy_metareg_dbspec = function(source_dir, project_dir, metaid) {
  dest_dir = file.path(project_dir, "metareg", metaid, "repdb_spec")
  if (!dir.exists(dest_dir)) try(dir.create(dest_dir))
  source_files = list.files(source_dir,glob2rx("*.yml"), full.names = TRUE)
  file.copy(source_files, dest_dir, overwrite=TRUE)
}



repdb_save_metareg_parcels = function(parcels, project_dir, metaid,check=TRUE) {
  dest_dir = file.path(project_dir, "metareg", metaid, "repdb")
  if (!dir.exists(dest_dir)) try(dir.create(dest_dir))
  spec_dir = file.path(project_dir, "metareg", metaid, "repdb_spec")
  repdb_load_specs(dir=spec_dir)

  repdb_save_parcels(parcels=parcels,dir=dest_dir, check=check)
}

repdb_load_metareg_parcels = function(project_dir,parcel_names, parcels=list() ) {
  repdb_load_parcels(project_dir=project_dir,parcel_names = parcel_names, parcels = parcels)
}

repdb_load_metareg_specs = function(project_dir, metaid) {
  dest_dir = file.path(project_dir, "metareg", metaid, "repdb_spec")
  repdb_load_specs(dir=dest_dir)
}

