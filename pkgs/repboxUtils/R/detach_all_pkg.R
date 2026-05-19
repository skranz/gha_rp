example = function() {
  detach_all_packages()
}
detach_all_packages = function(which = loadedNamespaces(), except = c("base","stats","graphics")) {
  stop("Don't run: does not nicely work yet.")
  loaded_packages <- setdiff(which,except)

  for (i in 1:10) {
    # Loop over each package and detach it
    for (pkg in loaded_packages) {
      suppressWarnings(try(detach(paste0("package:", pkg), character.only = TRUE, unload = TRUE), silent = TRUE))
    }
  }
  loadedNamespaces()
}
