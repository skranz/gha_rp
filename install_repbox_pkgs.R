cat("\nInstall local R packages\n")
install.packages("remotes")
inst = function(pkg, force=TRUE) {
  cat("\nInstall local ", pkg,"\n")
  suppressPackageStartupMessages(remotes::install_local(pkg, force=force))

}
pkgs = basename(list.dirs("pkgs", full.names=FALSE, recursive = FALSE))
for (pkg in pkgs) inst(paste0("pkgs/", pkg))

#inst("pkgs/repboxStata")
#inst("pkgs/repboxRun")
#inst("pkgs/repboxUtils")

cat("\nDone local package installation.\n")
