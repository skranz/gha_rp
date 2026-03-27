source("install_repbox_pkgs.R", local=TRUE)

library(repboxRun)
options(warn = 1)

source("run_config.R", local = TRUE)

if (!exists("gha_rp_run_config", inherits = FALSE)) {
  stop("run_config.R must define gha_rp_run_config.")
}

cfg = gha_rp_run_config

repo_dir = getwd()
if (!startsWith(cfg$work_dir, "/")) {
  cfg$work_dir = file.path(repo_dir, cfg$work_dir)
}
if (!startsWith(cfg$output_dir, "/")) {
  cfg$output_dir = file.path(repo_dir, cfg$output_dir)
}

dir.create(cfg$work_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cfg$output_dir, recursive = TRUE, showWarnings = FALSE)

project_dir = file.path(cfg$work_dir, cfg$artid)
input_dir = file.path(project_dir, "input")
dir.create(input_dir, recursive = TRUE, showWarnings = FALSE)

sup_zip = file.path(input_dir, basename(cfg$sup_url))

message("Downloading supplement ZIP from ", cfg$sup_url)
utils::download.file(
  url = cfg$sup_url,
  destfile = sup_zip,
  mode = "wb",
  quiet = FALSE
)

message("Initializing project in ", project_dir)
repboxRun::repbox_init_project(
  project_dir = project_dir,
  sup_zip = sup_zip,
  pdf_files = NULL,
  html_files = NULL,
  overwrite_org = TRUE,
  just_extract_code = FALSE
)

rb = repboxRun:::rb_new(
  project_dir = project_dir,
  copy_existing = FALSE,
  fail_action = if (isTRUE(cfg$stop.on.error)) "error" else "msg"
)

rb = repboxRun:::rb_update_file_info_parcel(
  rb = rb,
  overwrite = FALSE,
  assume_org_complete = TRUE
)

rb = repboxRun:::rb_update_script_type_parcel(
  rb = rb,
  overwrite = FALSE
)

stata_opts = repboxStata::repbox_stata_opts(
  timeout = cfg$timeout,
  all.do.timeout = cfg$timeout,
  extract.reg.info = isTRUE(cfg$capture_reg_info),
  extract.scalar.vals = isTRUE(cfg$capture_scalar_info)
)

manifest_extra = list(
  run_source = "gha_rp",
  run_config = cfg
)

message("Running raw Stata reproduction")
rb = repboxRun:::rb_run_stata_reproduction_raw(
  rb = rb,
  overwrite = isTRUE(cfg$overwrite),
  create_mod_dir = isTRUE(cfg$create_mod_dir),
  capture_reg_info = isTRUE(cfg$capture_reg_info),
  capture_scalar_info = isTRUE(cfg$capture_scalar_info),
  stata_opts = stata_opts,
  input_zip = sup_zip,
  manifest_extra = manifest_extra
)

message("Creating narrow Github Actions bundle")
repboxRun:::rb_make_gha_stata_bundle(
  project_dir = project_dir,
  bundle_dir = cfg$output_dir,
  overwrite = TRUE,
  input_zip = sup_zip,
  manifest_extra = manifest_extra
)

writeLines(
  text = capture.output(sessionInfo()),
  con = file.path(cfg$output_dir, "sessionInfo.txt")
)

message("Finished. Output bundle is in ", cfg$output_dir)
