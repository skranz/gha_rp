remove.dir = function(dir.to.be.removed, recursive=TRUE, must.contain = "/projects") {
  if (!has.substr(dir.to.be.removed,must.contain)) {
    stop(paste0("Sorry, for security reasons currently only directories that contain the path ", must.contain, " can be removed."))
  }
  if (!dir.exists(dir.to.be.removed)) return()
  unlink(dir.to.be.removed,recursive = recursive)
}

rb_has_complete_mod_dir = function(rb,project_dir=rb$project_dir,...) {
  rb_has_complete_org_dir(rb=rb, org_dir=file.path(project_dir, "mod"))
}

# We use heuristics
rb_has_complete_org_dir = function(rb=NULL, project_dir=rb$project_dir, zip_file=rb_get_sup_zip(project_dir), use_file_info=TRUE, file_info=NULL, org_dir = file.path(project_dir, "org")) {
  restore.point("rb_has_complete_org")
  file_info = NULL
  org_dir = file.path(project_dir, "org")
  if (!dir.exists(org_dir)) return(FALSE)

  if (use_file_info & is.null(file_info) & !is.null(rb)) {
    file_info=rb_parcel(rb, "file_info")
  }
  if (use_file_info & is.null(file_info)) {
    file_info = rb_get_file_info(project_dir)
  }
  if (is.null(file_info) & is.empty(zip_file)) return(NA)
  org_files = list.files(file.path(project_dir, "org"), recursive = TRUE)
  restore.point("khfhdkfhdf")
  if (!is.null(file_info)) {
    return(all(basename(file_info$file_path) %in% basename(org_files)))
  } else if (!is.empty(zip_file)) {
    need_files = unzip(zip_file, list=TRUE)$Name
    need_ext = tolower(tools::file_ext(need_files))
    need_files = need_files[!need_ext %in% c("", "zip","tar","gz","tar.gz") & ! startsWith(need_files,"__MACOSX")]
    return(all(need_files %in% org_files))
  }

}



unzip.zips = function(dir, remove=TRUE) {
  zip.files = list.files(dir, glob2rx("*.zip"), ignore.case=TRUE,full.names=TRUE,recursive = TRUE)
  for (zip.file in zip.files) {
    try(unzip(zip.file, exdir = dirname(zip.file)))
  }
  gz.files = list.files(dir, glob2rx("*.gz"), ignore.case=TRUE,full.names=TRUE,recursive = TRUE)
  for (gz.file in gz.files) {
    try(R.utils::gunzip(gz.file,remove=remove))
  }
  bz.files = list.files(dir, glob2rx("*.bz2"), ignore.case=TRUE,full.names=TRUE,recursive = TRUE)
  for (bz.file in bz.files) {
    try(R.utils::bunzip2(bz.file,remove=remove))
  }

  tar.files = list.files(dir, glob2rx("*.tar"), ignore.case=TRUE,full.names=TRUE,recursive = TRUE)
  for (tar.file in tar.files) {
    try(utils::untar(tar.file,exdir = dirname(tar.file)))
  }

}

repbox_get_org_sup_files = function(project_dir) {
  restore.point("repbox_get_org_sup_files")

  file = file.path(project_dir, "meta","sup_files.Rds")
  if (file.exists(file)) {
    file_df = readRDS(file)
    return(file_df)
  }

  if (repbox_has_just_extracted_code(project_dir)) {
    stop("The project has just extracted code files, but no general file ifo meta/sup_files.Rds exists, which should have created when project was initialized.")
  }

  file = file.path(project_dir, "repbox","org_files.Rds")
  if (file.exists(file)) {
    file_df = readRDS(file)
  } else {
    if (dir.exists(file.path(project_dir,"org"))) {
      file_df = make.project.files.info(project_dir,for.mod=FALSE)$org
    } else {
      stop("Cannot anymore generate file_df since org folder does not exist and no previous file info was stored...")
    }
  }

  file_df = file_df %>%
    transmute(
      file_path = file,
      file_type = tolower(ext),
      timestamp = mtime,
      mb = size / 1e6
    )

  return(file_df)
}

make.project.files.info = function(project_dir, for.org = TRUE, for.mod=TRUE) {
  restore.point("make.project.files.info")
  oldwd = getwd()
  org.fi = mod.fi = NULL

  if (!dir.exists(file.path(project_dir,"repbox"))) {
    dir.create(file.path(project_dir,"repbox"))
  }

  dir = file.path(project_dir,"org")
  if (for.org & dir.exists(dir)) {


    setwd(dir)
    files = list.files(dir,recursive = TRUE,include.dirs = FALSE)
    fi = as.data.frame(file.info(files))
    rownames(fi) = NULL
    fi$file = files
    fi$base = basename(files)
    fi$ext = tools::file_ext(files)
    fi
    org.fi = fi
    saveRDS(fi,file.path(project_dir,"repbox/org_files.Rds"))
  }

  dir = file.path(project_dir,"mod")
  if (for.mod & dir.exists(dir)) {
    setwd(dir)
    files = list.files(dir,recursive = TRUE,include.dirs = FALSE)
    fi = as.data.frame(file.info(files))
    rownames(fi) = NULL
    fi$file = files
    fi$base = basename(files)
    fi$ext = tools::file_ext(files)
    mod.fi = fi
    saveRDS(fi,file.path(project_dir,"repbox/mod_files.Rds"))
  }

  if (!is.null(oldwd)) setwd(oldwd)
  list(org = org.fi, mod=mod.fi)
}


repbox_make_script_parcels = function(project_dir, parcels=list(), overwrite=FALSE, file_info=parcels[["file_info"]]) {
  restore.point("repbox_parcel_script")

  if (!overwrite) {
    if (any(repboxDB::repdb_has_parcel(project_dir, c("stata_file", "stata_source", "r_file", "r_source")))) {
      return(parcels)
    }
  }

  if (is.null(file_info)) {
    stop("Please ensure that file_info parcel is loaded.")
  }


  script_df = file_info %>%
    filter(file_type %in% c("do","r")) %>%
    arrange(file_type, file_path) %>%
    mutate(
      artid=basename(project_dir),
      file_name = basename(file_path),
      sup_dir = paste0(project_dir, "/org"),
      long_path = paste0(sup_dir,"/", file_path),
      script_num = seq_len(n()),
      file_exists = file.exists(long_path),
      source_added = file_exists
    )
  text = rep(NA_character_, NROW(script_df))
  num_lines = rep(NA_integer_, NROW(script_df))

  for (i in seq_len(NROW(script_df))) {
    if (script_df$file_exists[i]) {
      txt = readLines(script_df$long_path[i],encoding = "UTF-8",warn = FALSE)
      text[i] = merge.lines(txt)
      num_lines[i] = length(txt)
    }
  }
  script_df$num_lines = num_lines

  # To avoid possible encoding errors later
  text = iconv(text,sub="?",from="UTF-8",to="UTF-8")

  script_df$text = text

  do_df = script_df %>% filter(file_type=="do")
  r_df = script_df %>% filter(file_type == "r")

  new_parcels = list(
    stata_file = do_df,
    stata_source = do_df,
    r_file = r_df,
    r_source = r_df
  )
  repdb_save_parcels(new_parcels, dir = file.path(project_dir, "repdb") )
  return(repdb_add_parcels(parcels, new_parcels))
}

special_file_types = function() {
  unlist(list(
    code = c("do","ado","r","mod","nb","py","m", "sas","prg", "ztt","c","java","cpp","js","f95","f90", "tsp","g","lng","gms","jl","rmd"),
    data = c("dta","csv","xls","xlsx","mat","dat","sas7bdat","rdata","rds", "xml","json", "tab"),
    archive =  c("7z","gz","tar","zip")
  ))
}
