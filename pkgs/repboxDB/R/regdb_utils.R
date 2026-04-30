
join_coalesce = function(x, y,by, xcols, ycols=xcols, yinds=NULL) {
  restore.point("join_coalesce")
  if (is.null(yinds)) {
    yinds = match(x[[by]], y[[by]])
  }

  for (i in seq_along(xcols)) {
    xcol = xcols[i]
    if (!xcol %in% names(x)) {
      x[[xcol]] = y[[ycols[i]]][yinds]
    } else {
      x[[xcol]] = coalesce(x[[xcol]],y[[ycols[i]]] [yinds])
    }
  }
  x
}

add_coalesce = function(dat, col, val_cols=NULL, default=NA) {
  restore.point("add_coalesce")
  val_cols = intersect(val_cols, names(dat))
  if (length(val_cols)==0) {
    dat[[col]] = rep(default,NROW(dat))
  } else if (length(val_cols)==1) {
    dat[[col]] = dat[[val_cols]]
  } else {
    dat[[col]] = do.call(coalesce,dat[val_cols])
  }
  dat
}
