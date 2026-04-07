cat("\nInstall local R packages\n")
install.packages("remotes")
inst = function(pkg, force=TRUE) {
  suppressPackageStartupMessages(remotes::install_local(pkg, force=force))

}
inst("pkgs/repboxStata", force=force)
inst("pkgs/repboxRun",force=force)

cat("\nDone local package installation.\n")
