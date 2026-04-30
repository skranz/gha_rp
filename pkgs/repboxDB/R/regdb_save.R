repdb_save_parcel_table = function(dat, project_dir, parcel_name, tab_name=parcel_name, parcel_dir = file.path(project_dir,"repdb")) {
  parcel = list(parcel=dat)
  names(parcel) = tab_name
  file = file.path(parcel_dir, paste0(parcel_name, ".Rds"))
  saveRDS(parcel, file)
}

repdb_save_parcels = function(parcels, dir, check=TRUE, check_missing_spec=FALSE) {
  restore.point("repdb_save_parcels")
  for (name in names(parcels)) {
    parcel = parcels[[name]]
    if (is.list(parcel) & !is.data.frame(parcel)) {
      stop(paste0("parcel ", name, " is still in deprecated old list format. Change code"))
    }
    if (NCOL(parcel)==0 | is.null(parcel)) {
      parcel = repdb_null_to_empty(NULL, name)
    }

    if (check) {
        repdb_check_data(parcel, name)
    }
    norm_parcel = dbspec_select_fields(parcel, repdb_get_spec(name))
    file = file.path(dir,paste0(name,".Rds"))
    if (!dir.exists(dir)) {
      cat("\nCreate repdb directory ", dir,"\n.")
      dir.create(dir,recursive = TRUE)
    }
    saveRDS(norm_parcel, file)
  }
}


repdb_save_rds = function(dat,  dir,table=c("reg","regcoef","regvar")[1],   spec = repdb_get_spec(table)) {
  restore.point("repdb_save_rds")
  file = file.path(dir, paste0(table,".Rds"))
  if (!dir.exists(dir)) {
    cat("\nCreate repdb directory ", dir,"\n.")
    dir.create(dir)
  }
  dbspec_save_rds(dat, file, spec)
}

repdb_select_fields = function(dat, table,spec = repdb_get_spec(table), ignore=NULL, null_as_empty=FALSE) {
  dbspec_select_fields(dat, spec, ignore, null_as_empty)
}

repdb_check_data = function(dat, table,spec = repdb_get_spec(table), check_missing_spec = TRUE) {
  restore.point("repdb_check_data")
  if (is.null(spec)) {
    stop(paste0("No repboxDB spec defined for parcel ",table, ".\nAdd in repboxDB/inst/repdb or for a synonym add in the function repdb_spec_map in R/repdb_spec.R. Then re-install repboxDB."))
  }
  dbspec_check_data(dat, spec)
}
