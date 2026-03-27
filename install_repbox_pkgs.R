if (!require("remotes"))
  install.packages("remotes")

options(
  repos = c(CRAN = "https://cloud.r-project.org"),
  warn = 1
)

Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS = "true")

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
        build_vignettes = FALSE
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

pkg_specs = list(
  restorepoint = c("skranz/restorepoint"),
  stringtools = c("skranz/stringtools"),
  dbmisc = c("skranz/dbmisc"),
  GithubActions = c("skranz/GithubActions"),

  repboxEvaluate = c("repboxr/repboxEvaluate", "skranz/repboxEvaluate"),
  repboxUtils = c("repboxr/repboxUtils", "skranz/repboxUtils"),
  repboxDB = c("repboxr/repboxDB", "skranz/repboxDB"),
  ExtractSciTab = c("repboxr/extractSciTab", "repboxr/ExtractSciTab", "skranz/ExtractSciTab"),
  repboxRfun = c("repboxr/repboxRfun", "repboxr/repboxrfun", "skranz/repboxRfun"),

  repboxCodeText = c("repboxr/repboxCodeText", "skranz/repboxCodeText"),
  repboxArt = c("repboxr/repboxArt", "skranz/repboxArt"),
  repboxDoc = c("repboxr/repboxDoc", "skranz/repboxDoc"),
  repboxR = c("repboxr/repboxR", "skranz/repboxR"),
  repboxStata = c("repboxr/repboxStata", "skranz/repboxStata"),
  repboxReg = c("repboxr/repboxReg", "skranz/repboxReg"),
  repboxStataReg = c("repboxr/repboxStataReg", "skranz/repboxStataReg"),
  repboxRegmap = c("repboxr/repboxRegmap", "skranz/repboxRegmap"),
  repboxDRF = c("repboxr/repboxDRF", "skranz/repboxDRF"),
  metaregBase = c("skranz/metaregBase", "repboxr/metaregBase"),

  repboxMap = c("repboxr/repboxMap", "skranz/repboxMap"),
  repboxHtml = c("repboxr/repboxHtml", "skranz/repboxHtml"),
  repboxEJD = c("repboxr/repboxEJD", "skranz/repboxEJD"),
  repboxRun = c("repboxr/repboxRun", "skranz/repboxRun")
)

install_order = c(
  "restorepoint",
  "stringtools",
  "dbmisc",
  "GithubActions",
  "repboxEvaluate",
  "repboxUtils",
  "repboxDB",
  "ExtractSciTab",
  "repboxRfun",
  "repboxCodeText",
  "repboxArt",
  "repboxDoc",
  "repboxR",
  "repboxStata",
  "repboxReg",
  "repboxStataReg",
  "repboxRegmap",
  "repboxDRF",
  "metaregBase",
  "repboxMap",
  "repboxHtml",
  "repboxEJD",
  "repboxRun"
)

install_repbox_stack = function(force = TRUE, max_rounds = 3) {
  remaining = install_order

  for (round in seq_len(max_rounds)) {
    message("\n=== install round ", round, " ===")
    start_remaining = remaining

    for (pkg in start_remaining) {
      ok = TRUE

      if (!force && requireNamespace(pkg, quietly = TRUE)) {
        next
      }

      tryCatch(
        install_from_candidates(pkg, pkg_specs[[pkg]], force = force, dependencies = TRUE),
        error = function(e) {
          ok <<- FALSE
          message("Still not ready: ", pkg)
          message(conditionMessage(e))
        }
      )

      if (ok && requireNamespace(pkg, quietly = TRUE)) {
        remaining = setdiff(remaining, pkg)
      }
    }

    if (length(remaining) == 0) {
      message("\nAll repbox Github packages installed.")
      return(invisible(TRUE))
    }

    if (identical(start_remaining, remaining)) {
      break
    }
  }

  stop(
    "Some packages could not be installed after ", max_rounds, " rounds: ",
    paste(remaining, collapse = ", ")
  )
}

install_repbox_stack(force = TRUE)
