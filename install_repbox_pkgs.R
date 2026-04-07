cat("\nInstall local R packages\n")
#remotes::install_local("pkgs/GithubActions")
remotes::install_local("pkgs/repboxStata", force=TRUE)
remotes::install_local("pkgs/repboxRun",force=TRUE)

cat("\nDone local package installation.\n")
