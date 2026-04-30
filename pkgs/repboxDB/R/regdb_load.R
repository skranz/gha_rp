
repdb_parcel_table = function(parcels,parcel_name,tab_name=NA) {
  parcel = parcels[[parcel_name]]
  if (!is.na(tab_name)) {
    parcel[[tab_name]]
  } else if (length(parcel)>0) {
    parcel[[1]]
  } else {
    NULL
  }
}

repdb_add_parcels = function(all_parcels, new_parcels) {
  all_parcels[names(new_parcels)] = new_parcels
  all_parcels
}


repdb_has_parcel = function(project_dir, parcel_name, parcel_dir = file.path(project_dir, "repdb")) {
  file.exists(file.path(parcel_dir, paste0(parcel_name, ".Rds")))
}


repdb_list_parcels = function(project_dir, metareg.dir = file.path(project_dir,"metareg"), other.dirs = paste0(project_dir,c("/repdb"))) {
  restore.point("repdb_list_parcels")
  dirs = list.dirs(metareg.dir,full.names = TRUE,recursive = FALSE)
  parcel.dirs = c(file.path(dirs, "repdb"), other.dirs)
  parcel.files = list.files(parcel.dirs,glob2rx("*.Rds"),full.names = TRUE)
  parcels = basename(parcel.files) %>% str.left.of(".Rds")
  metaids = basename(dirname(dirname(parcel.files)))

  tibble(parcel=parcels, path = parcel.files, metaid = metaids, prio=-nchar(metaids))

}

repdb_load_parcels = function(project_dir, parcel_names, parcels=NULL) {
  restore.point("repdb_load_parcels")
  parcel_names = setdiff(parcel_names, names(parcels))
  if (length(parcel_names)==0) return(parcels)

  parcel_df = repdb_list_parcels(project_dir)
  rows = match(parcel_names, parcel_df$parcel)
  use = !is.na(rows)
  paths = parcel_df$path[rows[use]]
  new_parcels = lapply(paths, readRDS)
  names(new_parcels) = parcel_names[use]

  # check old format and stop on error
  for (name in names(new_parcels)){
    parcel = new_parcels[[name]]
    if (is.list(parcel) & !is.data.frame(parcel)) {
      stop(paste0("Saved parcel ", name, " is still in deprecated old list format. Change code and re-run."))
    }
  }


  if (length(parcels)==0) return(new_parcels)
  c(parcels, new_parcels)
}
