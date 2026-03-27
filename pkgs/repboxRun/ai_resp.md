Below are the concrete code changes. I added a small `repboxRun` helper that can create a ZIP from `/org` if no original supplement ZIP exists, updated the staging helper in `repboxGithub` to use it automatically, and added a full example script that shows the end-to-end Github Actions test flow, including staging, committing the updated config, triggering the workflow, downloading the raw bundle, and doing local postprocessing.

!MODIFICATION rb_input_zip.R
scope = "file"
file = "/home/rstudio/repbox/repboxRun/R/rb_input_zip.R"
description = "Add helpers to find or create a supplement ZIP, including creating a ZIP from the org folder when no original ZIP exists."
-----------------------------------------------------------------------------------------------------------------------------------------

```r
#' Create a ZIP file from the current org folder
#'
#' This is useful for Github Actions test runs if no original supplement
#' ZIP file is available anymore, but the project still has a complete
#' /org directory.
#'
#' @export
rb_make_org_zip = function(
  project_dir,
  zip_file = file.path(project_dir, "meta", paste0(basename(project_dir), "_org.zip")),
  overwrite = FALSE,
  org_dir = file.path(project_dir, "org")
) {
  restore.point("rb_make_org_zip")

  project_dir = normalizePath(project_dir, mustWork = FALSE)
  org_dir = normalizePath(org_dir, mustWork = TRUE)

  if (!dir.exists(org_dir)) {
    stop("Cannot create supplement ZIP because the org directory does not exist.")
  }

  dir.create(dirname(zip_file), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(zip_file) && !overwrite) {
    return(normalizePath(zip_file, mustWork = TRUE))
  }
  if (file.exists(zip_file) && overwrite) {
    file.remove(zip_file)
  }

  oldwd = getwd()
  on.exit(setwd(oldwd), add = TRUE)
  setwd(org_dir)

  files = list.files(
    path = ".",
    recursive = TRUE,
    full.names = FALSE,
    all.files = TRUE,
    no.. = TRUE,
    include.dirs = FALSE
  )

  if (length(files) == 0) {
    stop("Cannot create supplement ZIP because the org directory is empty.")
  }

  utils::zip(
    zipfile = zip_file,
    files = files,
    flags = "-9Xq"
  )

  normalizePath(zip_file, mustWork = TRUE)
}

#' Return an available supplement ZIP or create one from /org
#'
#' The function first checks an explicitly supplied ZIP file, then the
#' repbox-side ZIP lookup, and finally can create a ZIP from /org.
#'
#' @export
rb_ensure_sup_zip = function(
  project_dir,
  sup_zip = NULL,
  zip_file = file.path(project_dir, "meta", paste0(basename(project_dir), "_org.zip")),
  overwrite = FALSE,
  create_from_org = TRUE
) {
  restore.point("rb_ensure_sup_zip")

  project_dir = normalizePath(project_dir, mustWork = FALSE)

  if (!is.null(sup_zip)) {
    if (length(sup_zip) == 0 || is.na(sup_zip[1]) || !file.exists(sup_zip[1])) {
      stop("The supplied sup_zip does not exist.")
    }
    return(normalizePath(sup_zip[1], mustWork = TRUE))
  }

  guessed_zip = try(rb_get_sup_zip(project_dir), silent = TRUE)
  if (!inherits(guessed_zip, "try-error") &&
      length(guessed_zip) > 0 &&
      !is.na(guessed_zip[1]) &&
      file.exists(guessed_zip[1])) {
    return(normalizePath(guessed_zip[1], mustWork = TRUE))
  }

  if (!create_from_org) {
    return(NULL)
  }

  rb_make_org_zip(
    project_dir = project_dir,
    zip_file = zip_file,
    overwrite = overwrite
  )
}
```

!END_MODIFICATION rb_input_zip.R

!MODIFICATION gha_rp_stage_sup_zip in gha_rp.R
scope = "function"
file = "/home/rstudio/repbox/repboxGithub/R/gha_rp.R"
function_name = "gha_rp_stage_sup_zip"
description = "Update supplement staging so it can automatically create a ZIP from /org when no original ZIP file is available."
--------------------------------------------------------------------------------------------------------------------------------

```r
#' Copy the original supplement ZIP to the server location used by gha_rp
#'
#' If no original ZIP is available, the function can create one from the
#' current /org folder via repboxRun::rb_make_org_zip.
#'
#' @export
gha_rp_stage_sup_zip = function(
  project_dir,
  server_dir,
  sup_zip = NULL,
  server_file_name = NULL,
  url_base = NULL,
  overwrite = TRUE,
  create_zip_from_org = TRUE,
  local_zip_file = file.path(project_dir, "meta", paste0(basename(project_dir), "_org.zip")),
  overwrite_local_zip = FALSE
) {
  restore.point("gha_rp_stage_sup_zip")

  project_dir = normalizePath(project_dir, mustWork = FALSE)

  sup_zip = repboxRun:::rb_ensure_sup_zip(
    project_dir = project_dir,
    sup_zip = sup_zip,
    zip_file = local_zip_file,
    overwrite = overwrite_local_zip,
    create_from_org = create_zip_from_org
  )

  if (is.null(sup_zip) || !file.exists(sup_zip)) {
    stop("Could not determine a valid supplement ZIP file.")
  }
  sup_zip = normalizePath(sup_zip, mustWork = TRUE)

  artid = basename(project_dir)
  safe_base = basename(sup_zip)
  safe_base = stringi::stri_replace_all_regex(safe_base, "[^A-Za-z0-9._-]+", "_")

  if (is.null(server_file_name)) {
    server_file_name = stringi::stri_paste(artid, safe_base, sep = "_")
  }

  dir.create(server_dir, recursive = TRUE, showWarnings = FALSE)
  server_file = file.path(server_dir, server_file_name)

  ok = file.copy(
    from = sup_zip,
    to = server_file,
    overwrite = overwrite,
    copy.mode = TRUE,
    copy.date = TRUE
  )
  if (!isTRUE(ok)) {
    stop("Could not copy the supplement ZIP to the server directory.")
  }

  res = list(
    artid = artid,
    sup_zip = sup_zip,
    server_dir = normalizePath(server_dir, mustWork = FALSE),
    server_file = normalizePath(server_file, mustWork = FALSE),
    server_file_name = server_file_name,
    size_mb = file.size(server_file) / 1e6,
    md5 = unname(tools::md5sum(server_file))
  )

  if (!is.null(url_base)) {
    res$url = gha_rp_url_join(url_base, server_file_name)
  }

  res
}
```

!END_MODIFICATION gha_rp_stage_sup_zip in gha_rp.R

!MODIFICATION gha_rp_example_test_repro.R
scope = "file"
file = "/home/rstudio/repbox/repboxGithub/inst/examples/gha_rp_example_test_repro.R"
description = "Add a concrete end-to-end example script that stages input, updates gha_rp, triggers the Github Actions test run, downloads the raw results, and performs local postprocessing."
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

```r
# Example end-to-end test script for the minimal Github Actions
# Stata reproduction in /home/rstudio/repbox/gha/gha_rp.
#
# Adapt all paths and repo names to your setup.
#
# This script also works if the project has no original supplement ZIP
# anymore, but still has a complete /org folder. In that case
# gha_rp_stage_sup_zip() creates a ZIP from /org automatically.

library(repboxGithub)
library(repboxRun)

run_cmd = function(cmd, args, wd = NULL) {
  cat("\n> ", paste(c(cmd, args), collapse = " "), "\n", sep = "")
  status = system2(command = cmd, args = args, stdout = "", stderr = "")
  if (!identical(status, 0L)) {
    stop(paste0("Command failed: ", paste(c(cmd, args), collapse = " ")))
  }
  invisible(TRUE)
}

# ------------------------------------------------------------
# 1. Example paths and repo names
# ------------------------------------------------------------

# Local repbox project.
# Example 1: project has an original supplement ZIP somewhere that
# rb_get_sup_zip(project_dir) can find.
#
# Example 2: project only has /org and no ZIP. Then the helper
# automatically creates meta/<artid>_org.zip from /org.
project_dir = "/home/rstudio/repbox/projects_test/aejapp_1_2_4"

# Local checkout of the Github Actions repo.
gha_repo_dir = "/home/rstudio/repbox/gha/gha_rp"

# Github repo slug used by git and gh.
github_repo = "YOUR_GITHUB_USER_OR_ORG/gha_rp"

# Workflow file in the gha_rp repo.
workflow_file = "run_remote_stata.yml"

# Directory on your server where the supplement ZIP should be copied.
server_dir = "/home/rstudio/repbox/server_files/gha_rp_inputs"

# Public base URL that maps to server_dir.
url_base = "https://econ.mathematik.uni-ulm.de/repbox_temp/gha_rp_inputs"

# ------------------------------------------------------------
# 2. Stage the supplement ZIP and refresh gha_rp/run_config.R
# ------------------------------------------------------------

# If you want to force using a specific ZIP file, set sup_zip explicitly.
# Otherwise keep sup_zip = NULL.
sup_zip = NULL

prep = gha_rp_prepare_repo(
  project_dir = project_dir,
  server_dir = server_dir,
  url_base = url_base,
  repo_dir = gha_repo_dir,
  sup_zip = sup_zip,
  overwrite = TRUE,
  timeout = 10 * 60,
  create_mod_dir = TRUE,
  capture_reg_info = TRUE,
  capture_scalar_info = TRUE,
  stop.on.error = TRUE,
  work_dir = "gha_work",
  output_dir = "gha_output/bundle",
  artifact_name = "gha-rp-results"
)

cat("\nPrepared Github Actions input.\n")
print(prep$staged_sup)
print(prep$run_config)

# ------------------------------------------------------------
# 3. Commit and push the updated gha_rp/run_config.R
# ------------------------------------------------------------

# The workflow reads run_config.R from the Github repo, so the changed
# config must be committed and pushed before triggering the run.

run_cmd("git", c("-C", gha_repo_dir, "status", "--short"))

commit_msg = paste0("Test raw Stata repro for ", basename(project_dir))

run_cmd("git", c("-C", gha_repo_dir, "add", "run_config.R"))
run_cmd("git", c("-C", gha_repo_dir, "commit", "-m", commit_msg))
run_cmd("git", c("-C", gha_repo_dir, "push"))

# ------------------------------------------------------------
# 4. Trigger the Github Actions workflow
# ------------------------------------------------------------

# This uses the gh CLI. You need to be logged in already.
run_cmd("gh", c("workflow", "run", workflow_file, "--repo", github_repo))

# Show recent runs for this workflow.
run_cmd("gh", c(
  "run", "list",
  "--repo", github_repo,
  "--workflow", workflow_file,
  "--limit", "5"
))

# Optional:
# You can watch the run interactively in a terminal with:
#
# system2("gh", c("run", "watch", "--repo", github_repo))
#
# Or inspect it in the Github web UI.

# ------------------------------------------------------------
# 5. After the workflow finished: download the raw Stata bundle
# ------------------------------------------------------------

# Run the next block only after the Github Actions job finished
# successfully.

gha_rp_download_stata_raw_results(
  repo = github_repo,
  project_dir = project_dir,
  overwrite = TRUE,
  verify = TRUE,
  local_input_zip = NULL
)

cat("\nRaw Github Actions bundle imported into the local project.\n")

# ------------------------------------------------------------
# 6. Local postprocess after the remote raw run
# ------------------------------------------------------------

# Now do the easy-to-debug local steps.

rb = repboxRun:::rb_new(
  project_dir = project_dir,
  copy_existing = FALSE,
  fail_action = "error"
)

# Make sure the file and script parcels exist locally.
rb = repboxRun:::rb_update_file_info_parcel(
  rb = rb,
  overwrite = FALSE,
  assume_org_complete = TRUE
)

rb = repboxRun:::rb_update_script_parcels(
  rb = rb,
  overwrite = FALSE
)

# Turn the raw Stata outputs into local derivatives.
rb = repboxRun:::rb_postprocess_stata_reproduction(
  rb = rb,
  overwrite = TRUE,
  build_run_info = TRUE,
  build_do_run_info = TRUE,
  build_reg_info = TRUE,
  build_drf = TRUE
)

# Optional local follow-up steps:
#
# rb = repboxRun:::rb_update_docs(
#   rb = rb,
#   overwrite = FALSE,
#   init_docs = TRUE,
#   process_docs = TRUE
# )
#
# rb = repboxRun:::rb_update_static_code_analysis(
#   rb = rb,
#   overwrite = FALSE,
#   lang = c("stata", "r")
# )

cat("\nLocal postprocess finished.\n")

# ------------------------------------------------------------
# 7. Notes for the no-ZIP case
# ------------------------------------------------------------

# If your project only had /org and no original ZIP:
#
# - gha_rp_prepare_repo() will create
#   /meta/<artid>_org.zip locally.
# - That ZIP is then copied to server_dir and used by Github Actions.
# - The raw run manifest stores the md5 of the ZIP used remotely.
#
# If you want to explicitly create the ZIP yourself first, you can do:
#
# local_zip = repboxRun::rb_make_org_zip(
#   project_dir = project_dir,
#   overwrite = TRUE
# )
# print(local_zip)
```

!END_MODIFICATION gha_rp_example_test_repro.R

The example script is deliberately written as a concrete sequence you can adapt line by line.
