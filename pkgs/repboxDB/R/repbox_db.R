# Converts the results of the original repbox reproduction
# into repdb tables

example = function() {
  project_dir = "C:/libraries/repbox/projects_reg/testsupp"
  project_dir = "~/repbox/projects_reg/aejapp_3_1_3"
  repbox_to_repdb(project_dir)

  rstudioapi::filesPaneNavigate(project_dir)
  rstudioapi::filesPaneNavigate("~/repbox/repboxDB/inst/repdb")
}

repbox_to_repdb = function(project_dir, parcels=list()) {
  restore.point("repbox_results_to_repdb")
  repdb_load_specs(libs="repboxDB")
  repdb.dir = file.path(project_dir, "repbox","repdb")
  if (!dir.exists(repdb.dir)) dir.create(repdb.dir)
  file_df = repbox_file_to_repdb(project_dir)
  invisible(parcels)
}

repbox_file_to_repdb = function(project_dir, ignore="repbox_") {
  restore.point("repbox_file_to_repdb")

  artid = basename(project_dir)
  org_files = readRDS.or.null(file.path(project_dir, "repbox","org_files.Rds"))
  mod_files = readRDS.or.null(file.path(project_dir, "repbox","mod_files.Rds"))
  if (!is.null(org_files)) {
    org_files = mutate(org_files, type="org")  }

  if (!is.null(mod_files)) {
    mod_files = mutate(mod_files, type="mod") %>% filter(!startsWith(base,"repbox_"))
  }


  file_df = bind_rows(
    org_files,
    mod_files
  )

  file_df = file_df %>%
    group_by(file) %>%
    arrange(desc(type)) %>%
    mutate(
      org_mtime = ifelse(type=="org", as.character(mtime),NA_character_),
      mod_mtime = ifelse(type=="mod", as.character(mtime),NA_character_),
      mb_org = ifelse(type=="org", size / 1e6,NA_real_),
      mb_mod = ifelse(type=="mod", size / 1e6,NA_real_),

    ) %>%
    summarize(
      artid = artid,
      file_name = first(base),
      file_type = tolower(first(ext)),
      mb = first(size) / 1e6,
      mb_org = first(size) / 1e6,
      mb_mod = first(size) / 1e6,
      mtime_org = first(na.omit(org_mtime)),
      mtime_mod = first(na.omit(mod_mtime)),
      in_org = any(type=="org"),
      in_mod = any(type=="mod")
    ) %>%
    rename(file_path = file)

  parcels = list(repbox_file = list(repbox_file=file_df))
  repdb_save_parcels(parcels["repbox_file"],dir = file.path(project_dir,"repdb"))
  invisible(parcels)
}


file_path_relative_to_supp = function(file_path, keep_from, wdir=NULL, supp.dir=NULL) {
  restore.point("file_path_relative_to")
  result = file_path

  empty_rows = file_path == ""
  abs_rows = is_abs_path(file_path)
  rel_rows = which(!abs_rows & !empty_rows)
  abs_rows = which(abs_rows)

  res_abs = abs_file_path_relative_to_supp(file_path[abs_rows], first(keep_from))
  result[abs_rows] = res_abs

  if (length(rel_rows)==0) {
    return(result)
  }

  res_rel = rel_file_path_relative_to_supp(file_path[rel_rows],first(keep_from), wdir[rel_rows],first(supp.dir))
  result[rel_rows] = res_rel

  result


}

rel_file_path_relative_to_supp = function(file_path, keep_from, wdir, supp.dir) {
  restore.point("rel_file_path_relative_to_supp")

  abs_wdir = abs_file_path_relative_to_supp(wdir, keep_from)

  ..rows = which(startsWith(file_path,"../"))
  while(length(..rows)>0) {
    file_path[..rows] = str.right.of(file_path[..rows],"../")
    abs_wdir[..rows] = dirname(abs_wdir[..rows])
    ..rows = which(startsWith(file_path,"../"))
  }
  paste0(abs_wdir, "/", file_path)
}


abs_file_path_relative_to_supp = function(file_path, keep_from) {
  restore.point("abs_file_path_relative_to_supp")
  res_abs = str.right.of(file_path, keep_from,not.found = rep(NA, length(file_path)))
  root.rows = which(endsWith(file_path, str.remove.ends(keep_from,0,1)) & is.na(res_abs))
  res_abs[root.rows] = "."
  if (any(is.na(res_abs))) {
    stop("Not all absolute paths could be translated.")
  }
  return(res_abs)
}

is_abs_path = function(x) {
  startsWith(x, "/") | startsWith(x,"~") | grepl(":",x, fixed=TRUE)
}
