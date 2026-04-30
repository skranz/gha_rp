example = function() {
  dat = data.frame(x =1:3, a="a",date = Sys.Date())
}

repdb_make_spec_from_data = function(tabname, dat, add_artid=FALSE, spec_dir = "~/repbox/repboxDB/inst/repdb", overwrite=FALSE) {
  restore.point("repdb_make_spec_file_from_data")
  spec_file = file.path(spec_dir, paste0(tab_name, ".yml"))
  if (!overwrite) {
    if (file.exists(spec_file)) return()
  }

  field_type_fun = function(val) {
    if (is.integer(val)) return("int")
    if (is.logical(val)) return("bool")
    if (is.numeric(val)) return("num")
    if (is(val,"Date")) return("date")
    if (is(val,"POSIXct")) return("datetime")
    return("")
  }

  field_types = sapply(dat, field_type_fun)
  cols = setdiff(colnames(dat),"artid")

  field_txt = paste0("  ", cols, ":\n","    descr:\n",
                     ifelse(field_types=="","",paste0("    type: ", field_types,"\n")))

  txt =
paste0("name: ",tabname,"
descr:

unique_index:

fields:
", if (add_artid) "
  artid:","

", paste0(field_txt, collapse="\n"))

  writeLines(txt, spec_file)
}


repdb_make_spec_template = function(dat, to_clipboard=TRUE) {
  field_type_fun = function(val) {
    if (is.integer(val)) return("int")
    if (is.logical(val)) return("bool")
    if (is.numeric(val)) return("num")
    if (is(val,"Date")) return("date")
    if (is(val,"POSIXct")) return("datetime")
    return("")
  }

  field_types = sapply(dat, field_type_fun)
  cols = setdiff(colnames(dat),"artid")

  field_txt = paste0("  ", cols, ":\n","    descr:\n",
                     ifelse(field_types=="","",paste0("    type: ", field_types,"\n")))

  txt =
paste0("name:
descr:

unique_index:

fields:
  artid:

", paste0(field_txt, collapse="\n"))
  cat(txt)
  #if (to_clipboard) clipr::write_clip(txt)
  invisible(txt)
}
