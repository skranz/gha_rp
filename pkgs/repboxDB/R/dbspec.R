# Functions to work with database field specifications
# as used for repdb

# At some point can be put into a separate package

dbspec_load = function(file) {
  restore.point("load_dbspec")
  spec = yaml.load_file(file)

  field_li = lapply(spec$fields, function(entry) {
    if (!is.null(entry$values)) {
      entry$values = list(entry$values)
    }
    if (!is.null(entry$val_descr)) {
      entry$val_descr = list(entry$val_descr)
    }
    if (is.null(entry$type)) {
      entry$type = ""
    }
    entry
  })
  field = names(field_li)
  df = bind_rows(field_li)

  fields = bind_cols(tibble(field=field),bind_rows(field_li))
  spec$fields = fields
  spec
}

dbspec_make_empty = function(spec) {
  f = spec$fields
  constr = case_when(
    f$type == "int" ~ "integer(0)",
    f$type == "num" ~ "numeric(0)",
    f$type == "bool" ~ "logical(0)",
    f$type == "" ~ "character(0)"
  )
  code = paste0("data.frame(",
    paste0(f$field, " = ", constr, collapse=",\n"),
    "\n)"
  )
  empty = eval(parse(text=code))
  empty
}

dbspec_select_fields = function(dat, spec, ignore = NULL, null_as_empty=TRUE) {
  restore.point("dbspec_select_fields")
  if (is.null(dat)) {
    if (!null_as_empty) return(NULL)
    dat = dbspec_make_empty(spec)
  }
  fields = setdiff(spec$fields$field, ignore)
  res = dat[fields]
  # Don't want to save by accident a grouped tibble
  if (is(res,"grouped_df")) res = ungroup(res)
  res
}

dbspec_save_rds = function(dat, file, spec, check=TRUE) {
  restore.point("dbspec_save_rds")
  if (NROW(dat)==0) {
    saveRDS(dbspec_make_empty(spec), file)
    return(invisible())
  }
  if (check) dbspec_check_data(dat, spec)
  df = dat[spec$fields$field]
  saveRDS(df, file)
  invisible(df)
}


dbspec_check_data = function(dat, spec) {
  restore.point("dbspec_check_data")
  if (is.null(spec)) stop("No specification provided!")

  if (is.null(dat)) return(TRUE)
  name = spec$name
  err = function(...) stop(paste0("Specification error for table ", name, ". ",...),call. = FALSE)

  spec = spec$fields
  fields = spec$field
  missing_fields = fields[!fields %in% names(dat)]
  if (length(missing_fields)>0) {
    return(err("Your data set is missing the following variable(s):\n\n", paste0(missing_fields, collapse=", ")))
  }

  types = spec$type
  rows = which(types %in% c("int","num"))
  for (i in rows) {
    f = fields[i]
    v = dat[[ f ]]
    if (!is.numeric(v)) {
      if (!all(is.na(v))) {
        return(err("Your field ", f, " must be of type ", types[i],"."))
      }
    }
    if (types[i]=="int") {
      if (!all(v == round(v), na.rm = TRUE)) {
        return(err("Your field ", f, " must be a integer."))
      }
    }
  }

  rows = which(types %in% c("bool"))
  for (i in rows) {
    f = fields[i]
    v = dat[[ f ]]
    if (is.logical(v)) next
    if (!is.numeric(v)) {
      return(err("Your field ", f, " must be a bool or 0/1 dummy."))
    }
    if (any(!v %in% c(0,1), na.rm = TRUE)) {
      return(err("Your field ", f, " must be a bool or 0/1 dummy."))
    }
  }

  return(TRUE)
}

