# parcels with different names that use certain spec
repdb_spec_map = function() {
  c(
    stata_file="script_file",r_file = "script_file",
    stata_source = "script_source",r_source = "script_source",
    stata_reg_run_info = "stata_run_info",
    reg_cmdpart = "cmdpart", quasi_reg_cmdpart = "cmdpart",
    reg_rb = "reg", regcoef_rb = "regcoef", regcoef_so = "regcoef", regcoef_sb_mfx="regcoef", regcoef_rb_mfx="regcoef", regscalar_so="regscalar", regstring_so = "regstring", regscalar_rb="regscalar", regstring_rb="regstring"
  )
}


repdb_null_to_empty = function(df, table) {
  if (!is.null(df)) return(df)
  spec = spec = repdb_get_spec(table)
  dbspec_make_empty(spec)
}



repdb_field_names = function(table) {
  repdb_get_spec(table)$fields$field
}

repdb_has_spec = function(table) {
  specs = getOption("repdb.specs")
  if (is.null(specs))  repdb_load_specs()
  if (table %in% names(specs)) return(TRUE)
  map = repdb_spec_map()
  table %in% names(map)
}

repdb_get_spec = function(table, allow_missing=TRUE) {
  restore.point("repdb_get_spec")
  specs = getOption("repdb.specs")
  if (is.null(specs))  repdb_load_specs()

  specs = getOption("repdb.specs")
  spec = specs[[table]]

  if (is.null(spec)) {
    map = repdb_spec_map()
    spec_name = map[table]
    if (!is.na(spec_name)) {
      spec = specs[[spec_name]]
    }
  }

  if (is.null(spec) & allow_missing) {
    return(NULL)
  } else if (is.null(spec)) {
    stop("The specification for table ", table, " was not loaded.")
  }
  spec
}

get_lib_path = function(lib) {
  si = devtools::session_info()[[2]]
  row = match(lib, si$package)
  si$loadedpath[row]
}

repdb_spec_files = function(libs = c("repboxDB","repboxCodeText")) {
  restore.point("repdb_load_specs")
  dir = sapply(libs, function(lib) {
    d = system.file("repdb",package=lib)
    # Also detect path if lib was loaded via devtools
    if (is.null(d) | is.na(d) | d=="") {
      lib_path = get_lib_path(lib)
      d = file.path(lib_path,"inst","repdb")
    }
    d
  })
  spec_files = list.files(dir,glob2rx("*.yml"), full.names = TRUE)
  spec_files

}


repdb_load_specs = function(dir = NULL, libs = c("repboxDB","repboxCodeText")) {
  restore.point("repdb_load_specs")
  if (is.null(dir)) {
    dir = sapply(libs, function(lib) {
      d = robust_system_file("repdb",lib)
    })
  }
  spec_files = list.files(dir,glob2rx("*.yml"), full.names = TRUE)
  spec_bases = basename(spec_files)
  spec_names = tools::file_path_sans_ext(spec_bases)

  specs = lapply(spec_files, function(file) {
    res = try(dbspec_load(paste0(file)))
    if (is(res,"try-error")) {
      stop(paste0("There was a problem when loading the data base specification ", file,". Please correct the specification file."),call. = FALSE)
    }
    res
  })
  names(specs) = spec_names



  old.specs = getOption("repdb.specs")
  if (!is.null(old.specs)) {
    old.cols = setdiff(names(old.specs), names(specs))
    specs[old.cols] = old.specs[old.cols]
  }

  options(repdb.specs = specs)
  invisible(specs)
}

robust_system_file = function(file, lib) {
  sfile = system.file(file,package=lib)
  # Also detect path if lib was loaded via devtools
  if (is.null(sfile) | is.na(sfile) | sfile=="") {
    lib_path = get_lib_path(lib)
    sfile = file.path(lib_path,"inst",file)
  }
  sfile
}

