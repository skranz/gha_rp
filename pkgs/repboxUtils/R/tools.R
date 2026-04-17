example = function() {
  make_valid_filename("dh(5/sdh.txt")
}

is_empty_str = function(x) {
  is.na(x) | is.true(x=="")
}

write_utf8 <- function(x, file, bom=F) {
  x = paste0(x, collapse="\n")
  con <- file(file, "wb")
  if(bom) writeBin(BOM, con, endian="little")
  writeBin(charToRaw(x), con, endian="little")
  close(con)
}

#' Read a text file that was saved in UTF-8 format
#' @export
read_utf8 <- function(file,sep.lines=FALSE,warn=FALSE,...) {
  text <- readLines(file,encoding = "UTF-8",warn=warn,...)
  if (!sep.lines) text = paste0(text, collapse="\n")
  text
}

#' Read an RDS file if it exists. Otherwise return NULL
#' @export
read_rds_or_null = function (file) {
    if (!file.exists(file))
        return(NULL)
    readRDS(file)
}

copy_into_list <- function(source = parent.frame(), dest = list(), exclude = NULL) {
  # Get all objects in the source environment
  all_vars <- ls(envir = source, all.names = TRUE)

  # Filter out variables to exclude
  if (length(exclude)>0) {
    vars_to_copy <- setdiff(all_vars, exclude)
  } else {
    vars_to_copy <- all_vars
  }

  # Vectorized assignment using mget() to get multiple objects at once
  if (length(vars_to_copy) > 0) {
    values <- mget(vars_to_copy, envir = source)
    dest[names(values)] <- values
  }

  # Return the updated destination list
  return(dest)
}

as_integer = function(x) {
  suppressWarnings(as.integer(x))
}

make_valid_filename <- function(filename) {
  # First, convert accented characters to their ASCII equivalents,
  # then replace any character not matching [A-Za-z0-9_.-] with an underscore.
  stri_replace_all_regex(stri_trans_general(filename, "Latin-ASCII"), "[^\\w.-]+", "_")
}

atomic_class = function(val) {
  val = as.vector(val)
  cl = class(val)
  if (length(cl)>1) return(cl[length(cl)])
  cl
}

to_utf8 = function(str) {
  str = iconv(str, to="UTF-8", sub="")
  str = enc2utf8(str)
  str
}

select = dplyr::select

# Checks if x fits a Stat abbreviation
# E.g. is_stata_abbr("regr","reg","regress")
# would return TRUE
is_stata_abbr = function(x, short, long) {
  nlong = nchar(long)
  nshort = nchar(short)
  nx = nchar(x)

  ok = rep(FALSE, NROW(x))
  rows = which(nx >= nshort & nx <= nlong)
  if (length(rows)==0) return(ok)

  comp = stringi::stri_sub(long, 1, nx[rows])
  ok[x == comp] = TRUE
  ok
}

locals.vars.as.list = function(source = sys.frame(sys.parent(1))) {
  as.list(source)
}

copy.dir = function(from, to, ...) {
  if (!dir.exists(to)) dir.create(to, recursive = TRUE)
  res = file.copy(list.files(from, full.names = TRUE), to, recursive = TRUE, ...)
  invisible(all(res))
}

# removes all files in
remove.all.files.in.dir = function(clear.dir, recursive=FALSE) {
  if (!dir.exists(clear.dir)) return(TRUE)
  remove.files = list.files(clear.dir,all.files = FALSE,full.names = TRUE,recursive = recursive)
  file.remove(remove.files)
}

is_empty = function(x) {
  if (is.null(x) | all(is.na(x))) return(TRUE)
  if (isTRUE(x=="")) return(TRUE)
  return(FALSE)
}

is.empty = function(x) {
  if (is.null(x) | all(is.na(x))) return(TRUE)
  if (isTRUE(x=="")) return(TRUE)
  return(FALSE)
}

shorten.str = function(str,len=100, marker = "...") {
  is.bigger = nchar(str)>len
  str[is.bigger] = paste0(substring(str[is.bigger],1,len-nchar(marker)),marker)
  str
}


first.non.null = function(...) {
  args = list(...)
  for (val in args) {
    if (!is.null(val)) return(val)
  }
  return(NULL)

}

ws_to_single_space = function(str) {
  gsub("\t"," ", str) %>%  trimws_around(" ")
}

trimws_around = function(str, around, whitespace="[\t ]*") {
  pattern = paste0(whitespace,around,whitespace)
  str = gsub(pattern, around, str)
  str
}

set_missing_fields = function(x, ...) {
  new_vals = list(...)
  cols = names(new_vals)
  use = which(!cols %in% names(x))
  for (i in use) {
    x[[cols[i] ]] = new_vals[[i]]
  }
  x

}

set.fields = function(x, fields) {
  x[names(fields)] = fields
  x
}

select.cols = function(x, cols, create.missing=TRUE) {
  if (create.missing) {
    x = create.missing.cols(x,cols)
  }
  x[cols]
}



create.missing.cols = function(x, cols, val=NA) {
  cols = setdiff(cols,names(x))
  for (col in cols) {
    x[[col]] = val
  }
  x
}

has_col = function(x, col) {
  col %in% names(x)
}

has.col = function(x, col) {
  col %in% names(x)
}

rename.col = function(x, old, new) {
  restore.point("rename.col")
  pos = match(old,names(x))
  names(x)[pos] = new
  x
}

change_val = function(x, org, new) {
  if (length(org)==1) {
    rows = which(x==org)
  } else {
    rows = which(x %in% org)
  }
  x[rows] = new
  x
}

ensure_cols = function(x, cols, default=NA) {
  new = setdiff(cols, names(x))
  for (col in new) {
    x[[col]] = rep(default, NROW(x))
  }
  x
}

str.cutout = function(str, left, right) {
  paste0(substring(str,1, left-1),substring(str,right+1))
}

shorten.spaces = function(str) {
  return(ws_to_single_space(str))
}

rename.cols = function(x, old.cols, new.cols) {
  restore.point("rename.cols")
  inds = match(old.cols, names(x))
  old.cols = old.cols[!is.na(inds)]
  new.cols = new.cols[!is.na(inds)]
  inds = inds[!is.na(inds)]
  names(x)[inds] = new.cols
  x
}

na.if.null = function(x) {
  if (is.null(x)) return(NA)
  x
}


left_of = function(str, pattern, fixed=TRUE, not.found = str) {
  restore.point("left_of")
  if (length(not.found)==1) not.found = rep(not.found, length(str))
  if (fixed) {
    pos = stringi::stri_locate_first_fixed(str, pattern)
  } else {
    pos = stringi::stri_locate_first_regex(str, pattern)
  }
  res = substring(str, 1, pos[, 1] - 1)
  rows = is.na(pos[, 1])
  res[rows] = not.found[rows]
  res
}

omit.na.or.similar = function(dat, cols=names(dat)) {
  if (NROW(dat)==0) return(dat)
  rem = rep(FALSE, NROW(dat))
  for (col in cols) {
    if (is.numeric(dat[[col]])) {
      rem[!is.finite(dat[[col]])] = TRUE
    } else {
      rem[is.na(dat[[col]])] = TRUE
    }
  }
  dat[!rem, ]
}

standardizePath = function(file, mustWork=FALSE) {
  file = normalizePath(file, winslash = "/", mustWork=mustWork)
}


seq_rows = function(dat) {
  seq_len(NROW(dat))
}

random.string = function (n = 1, nchar = 12) {
  if (n == 1) {
    paste0(sample(c(LETTERS, letters), nchar, TRUE), collapse = "")
  }
  else {
    unlist(replicate(n, paste0(sample(c(LETTERS, letters),
                                      nchar, TRUE), collapse = "")))
  }
}


# Only adds col values if dat is not NULL
add_col = function(dat, ...) {
  if (is.null(dat)) return(dat)
  cols = list(...)
  if (NROW(dat)==0) {
    for (c in names(cols)) {
      dat[[c]] = rep(cols[[c]],0)
    }
  } else {
    for (c in names(cols)) {
      dat[[c]] = cols[[c]]
    }
  }
  dat
}


my_rank = function(x) {
  restore.point("my_rank")
  if (length(x)==0) return(integer(0))
  vals = unique(x)
  match(x, vals)
}

ensure_empty_types = function(x, cols, type="character") {
  if (NROW(x)>0) return(x)
  if (type=="character") {
    for (col in cols) {
      x[[col]] = character(0)
    }
  }
  x
}

rle_table = function(x) {
  rle = rle(x)
  start = c(1,cumsum(rle$lengths)+1)
  end = start[-1]-1
  start = start[-length(start)]
  tibble(start=start, end=end, length=rle$lengths, value = rle$values)
}

rle_block = function(x, ignore_val=NULL) {
  rle = rle(x)
  block_vals = seq_along(rle$values)
  if (length(ignore_val)>0) {
    zero_rows = rle$values %in% ignore_val
    block_vals[zero_rows] = 0
    uni = unique(block_vals[!zero_rows])
    block_vals[!zero_rows] = match(block_vals[!zero_rows],uni)
  }
  rep(block_vals, rle$lengths)
}

first.non.null = function(...) {
  args = list(...)
  for (val in args) {
    if (!is.null(val)) return(val)
  }
  return(NULL)

}

list_find_null_entries = function(li) {
  which(!is.na(match(li,list(NULL))))
}

list_remove_null_entries = function(li) {
  null.rows = which(!is.na(match(li,list(NULL))))
  if (length(null.rows)>0) {
    return(li[-null.rows])
  }
  li
}
bind_rows_with_parent_fields = function(parent.df,df.field,  fields, parent.fields, parent.row.field = "..PARENT.ROW") {
  restore.point("bind_rows_with_parent_fields")
  li = parent.df[[df.field]]
  names(li) = NULL

  null_rows = list_find_null_entries(li)

  if (length(null_rows)>0) {
    li = li[-null_rows]
    parent.df = parent.df[-null_rows,]
  }

  df = bind_rows(li, .id=parent.row.field)
  df[[parent.row.field]] = as.integer(df[[parent.row.field]])
  for (field in fields) {
    df[[field]] = parent.df[[field]][df[[parent.row.field]]]
  }

  use.cols = setdiff(colnames(df), parent.row.field)
  df = df[,use.cols]
}



readRDS.or.null = function(file) {
  if (!file.exists(file)) return(NULL)
  readRDS(file)
}

na.remove = function(x) {
  x[!is.na(x)]
}

na.false = function(x) {
  na.val(x, FALSE)
}

na.val = function(x, val=0) {
  x[is.na(x)] = val
  x
}

is.true = function(x) {
  val = x == TRUE
  val[is.na(x)] = FALSE
  val
}

most.common = function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

remove_cols = function(x, cols) {
  x[,setdiff(colnames(x),cols)]
}

remove.cols = function(x, cols) {
  x = x[,setdiff(colnames(x),cols)]
}


# Perform a left join but first drop columns in y or x
# that also exist in x (y) and are not part of by
left_join_overwrite = function(x,y,by,..., yfields=NULL, drop_in="y") {
  if (!is.null(yfields)) {
    y = y[,intersect(names(y),union(yfields, by)), drop=FALSE]
  }
  drop.cols = setdiff(intersect(colnames(x), colnames(y)),by)

  if (length(drop.cols)>0) {
    if (drop_in=="y") {
      y = y[, setdiff(colnames(y), drop.cols)]
    } else {
      x = x[, setdiff(colnames(x), drop.cols)]
    }
  }
  left_join(x,y,by=by,...)
}


parse_eval = function(cmd, envir=parent.frame()) {
  if (is.null(cmd)) return(invisible(NULL))
  eval(parse(text=cmd),envir = envir)
}



left_join_overlap = function(x, y, by_x, by_y, mult="all", type="any", nomatch=NA) {
  dt_x = as.data.table(x)
  dt_y = as.data.table(y,key = by_y)
  dt_ol = foverlaps(dt_x, dt_y, mult=mult, type=type, which=FALSE, by.x=by_x)
  as_tibble(dt_ol)
}

match_overlap = function(xstart,xend,ystart,yend, mult="all", type="any") {
  restore.point("match_all_rounded")
  dt_x = data.table(ind_x = seq_along(xstart), xstart=xstart, xend=xend,key = c("xlow","xhigh"))
  dt_y = data.table(ind_y = seq_along(ystart), ystart=ystart, yend=yend,key = c("ylow","yhigh"))

  ol = foverlaps(dt_x, dt_y, mult=mult, type=type, which=TRUE, nomatch=NULL)
  res = tibble(ind_x=dt_x$ind_x[ol[[1]]],ind_y = dt_y$ind_y[ol[[2]]])
  res
}
# Remove from x all locations that overlap with a location in y
remove.overlapping.loc = function(x, y, by=c("line", "start","end"), mode=c("remove","keep","mark")[1]) {
  restore.point("remove.overlapping.loc")
  library(data.table)
  loc1 = as.data.table(x)
  setkeyv(loc1, by)

  loc2 = as.data.table(y)
  setkeyv(loc2, by)

  ol = foverlaps(loc1, loc2, by.x=by, by.y=by, which=TRUE, nomatch=NA)
  not.ol = sort(unique(ol$xid[is.na(ol$yid)]))
  if (mode=="mark") {
    x$overlapping = TRUE
    x$overlapping[!not.ol] = FALSE
    return(x)
  }
  if (mode =="remove") {
    x[not.ol,,drop=FALSE]
  } else if (mode == "keep") {
    if (length(not.ol)==0) return(x)
    x[-not.ol,,drop=FALSE]
  }
}

keep.overlapping.loc = function(...) {
  remove.overlapping.loc(..., mode="keep")
}

locate_all_as_df = function(txt, regexpr.li) {
  restore.point("locate_all_as_df")
  locs.li = lapply(regexpr.li, function(regexpr) {
    str_locate_all(txt, regexpr)
  })

  loc.df = bind_rows(lapply(seq_along(locs.li), function(i) {
    type = names(locs.li)[i]
    locs = bind_rows(lapply(seq_along(locs.li[[i]]), function(row) {
      loc = locs.li[[i]][[row]]
      if (NROW(loc)==0) return(NULL)
      loc = as_tibble(loc)
      loc$line = row
      loc$type = type
      loc$pos = 1:NROW(loc)
      loc
    }))
    locs
  }))
  if (NROW(loc.df)==0) {
    loc.df = tibble(start=integer(0),end=integer(0), line=integer(0), type=character(0), pos=integer(0))
  }

  loc.df$str = substring(txt[loc.df$line], loc.df$start, loc.df$end)
  loc.df$nchar = loc.df$end-loc.df$start
  loc.df
}


ends.with.text = function(txt, end.size=4) {
  str = trimws(txt)
  rhs = substring(str, nchar(str)-end.size,nchar(str))
  grepl("[a-zA-z]",rhs,fixed=FALSE)

}

seq_rows = function(x) {
  seq_len(NROW(x))
}

from_to = function(from, to, min=from, max=to) {
  from = max(from, min)
  to = min(to, max)
  if (from > to) return(NULL)
  from:to
}


remove.start.strings = function(str, starts) {
  restore.point("remove.start.strings")
  for (s in starts) {
    rows = which(startsWith(str, s))
    if (length(rows)>0) {
      str[rows] = trimws(str.right.of(str[rows],s))
    }
  }
  str
}


read_lines_from_tail = function(file, n=10) {
  restore.point("read_lines_from_tail")
  # Only faster under unix, which has a tail command
  file = normalizePath(file)
  if (.Platform$OS.type == "unix") {
    cmd = paste0("tail -n ", n, ' "', file,'"')
    res = system(cmd, intern=TRUE)
  } else {
    txt = readLines(file)
    res = tail(txt,n)
  }
  res
}

parse.as.call = function (text, allow.null = TRUE)
{
  if (is.null(text)) {
    if (allow.null)
      return(NULL)
    stop("parse.as.call was called with text=NULL")
  }
  parse(text = text)[[1]]
}

find.call.variables = function(call){
  if (is.name(call))
    return(as.character(call))
  if (length(call) <= 1)
    return(NULL)
  names = lapply(call[-1], function(e1) {
    find.call.variables(e1)
  })
  names = unique(unlist(names, use.names = FALSE))
  names
}

rstudio_job = function(name, expr, libs=NULL, wdir = getwd(), script.dir = tempdir(), importEnv=TRUE) {
  expr = substitute(expr)
  restore.point("as_job")
  code = deparse1(expr)

  #code = paste0('setwd("',wdir,'")\n', code)
  if (!is.null(libs)) {
    code = paste0(paste0("library(", libs,")", collapse="\n"), "\n", code)
  }
  cat("\nRun \n\n",code,"\n\n")

  script.file = file.path(script.dir, paste0(name,".R"))
  writeLines(code, script.file)
  rstudioapi::jobRunScript(path = script.file,workingDir = wdir, name = name,importEnv = importEnv)

}

writeUtf8 = function(x, file, bom=F) {
  if (length(x)>1) {
    x = paste0(x, collapse="\n")
  }
  con <- file(file, "wb")
  if(bom) writeBin(BOM, con, endian="little")
  writeBin(charToRaw(x), con, endian="little")
  close(con)
}


create_dirs_of_files = function (files, must.have = NULL) {
  restore.point("create_dirs_of_files")
  dirs = unique(dirname(files))
  if (!is.null(must.have)) {
    if (!all(stringi::stri_detect_fixed(files, must.have))) {
        stop(paste0("Not all directories contain the pattern ",
            must.have))
    }

  }
  for (dir in dirs) {
      if (!dir.exists(dir)) {
          dir.create(dir, recursive = TRUE)
      }
  }
}

file.exists.in.any.dir = function(files, dirs) {
  exists = rep(FALSE,length(files))
  for (dir in dirs) {
    exists = exits | file.exists(file.path(dir,files))
  }
  exists
}

# Get a function but set its enclosing environment
# by default to the environment of the caller
fun_with_env = function(fun, env=parent.frame()) {
  environment(fun) = env
  fun
}


with_random_seed = function(expr, seed = 1234567890) {
  old.seed = get(".Random.seed", .GlobalEnv)
  set.seed(seed)
  ret = eval(expr)
  restore.point("with_random_seed")
  assign(".Random.seed", old.seed, .GlobalEnv)
  runif(1)
  return(ret)
}

is_valid_zip_file <- function(path) {

  ## ---- preliminaries ---------------------------------------------------
  if (!file.exists(path)) return(FALSE)

  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)

  ## ---- 1) signature check ---------------------------------------------
  sig <- readBin(con, what = "raw", n = 2L)
  # legal signatures for local-file header, EOCD for empty archive, or spanned archives
  if (!identical(sig[1],0x50)) return(FALSE)
  if (!identical(sig[2],0x4b)) return(FALSE)
  return(TRUE)

  valid_sig <- list(
    c(0x50, 0x4b, 0x03, 0x04),  # PK\x03\x04  -> normal archive
    c(0x50, 0x4b, 0x05, 0x06),  # PK\x05\x06  -> empty archive
    c(0x50, 0x4b, 0x07, 0x08)   # PK\x07\x08  -> spanned
  )
  if (!any(vapply(valid_sig, identical, logical(1), sig)))
    return(FALSE)
}

is_valid_pdf_file <- function(path, quick=FALSE) {
  con  <- file(path, "rb")
  on.exit(close(con), add = TRUE)

  # Read first 8 bytes for the header
  header <- rawToChar(readBin(con, "raw", n = 8))
  starts_ok <- grepl("^%PDF-[12]\\.[0-9]", header)
  if (!starts_ok) return(FALSE)
  if (quick) return(starts_ok)

  # Read last ~10 bytes for the EOF marker
  seek(con, where = -10, origin = "end")
  tail   <- rawToChar(readBin(con, "raw", n = 10))

  ends_ok   <- grepl("%%EOF", tail)

  starts_ok && ends_ok
}

zip_dir = function(dir, dest_file, overwrite=TRUE) {
  if (!dir.exists(dir)) {
    cat("\nDirectory does not exist: ", dir,"\n")
    return(FALSE)
  }
  if (!overwrite & file.exists(dest_file)) {
    cat("\nZIP file", dest_file, "already exists.\n")
    return(FALSE)
  }

  dir = normalizePath(dir, mustWork = TRUE)
  dest_file = normalizePath(dest_file, mustWork = FALSE)

  files = list.files(dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)

  old_wd = getwd()
  on.exit(setwd(old_wd), add = TRUE)

  setwd(dirname(dir))
  utils::zip(zipfile = dest_file, files = file.path(basename(dir), files))
  return(TRUE)
}

