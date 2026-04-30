readRDS.or.null = function(file) {
  if (!file.exists(file)) return(NULL)
  readRDS(file)
}

is.true = function(x) {
  val = x == TRUE
  val[is.na(x)] = FALSE
  val
}

has.col = function(x, col) {
  col %in% names(x)
}
