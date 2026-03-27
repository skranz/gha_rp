options(
  repos = c(CRAN = "https://cloud.r-project.org"),
  warn = 1
)

Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS = "true")

for (nm in c(
  "GITHUB_PAT",
  "GITHUB_TOKEN",
  "GH_TOKEN",
  "GITHub_PAT",
  "GITHUB_API_TOKEN"
)) {
  if (nzchar(Sys.getenv(nm, ""))) {
    message("Unset ", nm, " so public GitHub installs use anonymous access.")
    Sys.unsetenv(nm)
  }
}

ensure_cran = function(pkgs) {
  pkgs = unique(pkgs)
  missing = pkgs[!vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(missing) == 0) return(invisible(TRUE))
  install.packages(missing, dependencies = TRUE)
  invisible(TRUE)
}

ensure_cran(c("remotes"))

install_from_candidates = function(pkg, repos, force = TRUE, dependencies = TRUE) {
  if (!force && requireNamespace(pkg, quietly = TRUE)) {
    message("Skipping ", pkg, ": already installed.")
    return(invisible(TRUE))
  }

  last_error = NULL

  for (repo in unique(repos)) {
    message("Installing ", pkg, " from ", repo, " ...")

    res = try(
      remotes::install_github(
        repo = repo,
        dependencies = dependencies,
        upgrade = "never",
        force = force,
        build_vignettes = FALSE,
        auth_token = NULL
      ),
      silent = TRUE
    )

    if (!inherits(res, "try-error")) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        stop("Installation of ", pkg, " from ", repo, " finished, but the package still cannot be loaded.")
      }
      message("Installed ", pkg, " from ", repo)
      return(invisible(TRUE))
    }

    last_error = res
    message("Failed from ", repo)
  }

  stop(
    "Could not install ", pkg, ".\n",
    "Tried: ", paste(unique(repos), collapse = ", "), "\n",
    "Last error:\n", as.character(last_error)
  )
}

# Basic helpers
install_from_candidates(
  "restorepoint",
  c("skranz/restorepoint"),
  force = TRUE,
  dependencies = TRUE
)

install_from_candidates(
  "stringtools",
  c("skranz/stringtools"),
  force = TRUE,
  dependencies = TRUE
)

install_from_candidates(
  "dbmisc",
  c("skranz/dbmisc"),
  force = TRUE,
  dependencies = TRUE
)

install_from_candidates(
  "GithubActions",
  c("skranz/GithubActions"),
  force = TRUE,
  dependencies = TRUE
)

# repbox core pieces
install_from_candidates(
  "repboxUtils",
  c("repboxr/repboxUtils", "skranz/repboxUtils"),
  force = TRUE,
  dependencies = TRUE
)

install_from_candidates(
  "repboxDB",
  c("repboxr/repboxDB", "skranz/repboxDB"),
  force = TRUE,
  dependencies = TRUE
)

install_from_candidates(
  "repboxStata",
  c("repboxr/repboxStata", "skranz/repboxStata"),
  force = TRUE,
  dependencies = TRUE
)

install_from_candidates(
  "repboxStataReg",
  c("repboxr/repboxStataReg", "skranz/repboxStataReg"),
  force = TRUE,
  dependencies = TRUE
)

install_from_candidates(
  "repboxRun",
  c("repboxr/repboxRun", "skranz/repboxRun"),
  force = TRUE,
  dependencies = TRUE
)

message("\nSelected repbox Github packages installed.")
