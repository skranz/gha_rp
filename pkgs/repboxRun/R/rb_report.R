# Project level markdown report for the current repbox state.
#
# Main entry point:
#   rb_report_md(project_dir)
#
# By default the function writes:
#   reports/reproduction_status.md
#
# Set write_file = FALSE to return the markdown text instead.

rb_report_md = function(
  rb = NULL,
  project_dir = if (!is.null(rb)) rb$project_dir else NULL,
  file = file.path(project_dir, "reports", "reproduction_status.md"),
  write_file = TRUE,
  include_repdb_table = TRUE,
  max_problem_rows = 10,
  render_html = TRUE,
  html_file = NULL,
  write_metareg_issue_report = TRUE,
  metareg_issue_file = file.path(project_dir, "reports", "metareg_issues.md"),
  metareg_issue_render_html = FALSE,
  metareg_issue_html_file = NULL,
  metareg_issue_opts = rb_report_metareg_issue_opts()
) {
  restore.point("rb_report_md")



  if (is.null(project_dir)) {
    stop("You must provide project_dir or rb.")
  }

  project_dir = normalizePath(project_dir, mustWork = FALSE)
  artid = basename(project_dir)

  if (is.null(html_file)) {
    html_file = rb_report_default_html_file(file)
  }
  if (is.null(metareg_issue_html_file)) {
    metareg_issue_html_file = rb_report_default_html_file(metareg_issue_file)
  }

  metareg_issue_status = NULL
  if (isTRUE(write_metareg_issue_report)) {
    metareg_issue_status = rb_report_metareg_issue_status(
      project_dir = project_dir,
      opts = metareg_issue_opts,
      include_notes = TRUE
    )
  }

  lines = c(
    paste0("# Reproduction status report: ", artid),
    "",
    paste0("- Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste0("- Project dir: `", project_dir, "`"),
    ""
  )

  lines = c(
    lines,
    "## Milestones",
    "",
    rb_report_md_table(
      rb_report_milestone_df(
        project_dir,
        html_file = html_file,
        html_will_be_written = isTRUE(write_file) && isTRUE(render_html)
      )
    ),
    "",
    "## Overall statistics",
    "",
    rb_report_overall_lines(project_dir),
    "",
    "## Issues and problems",
    "",
    rb_report_issue_lines(project_dir, max_rows = max_problem_rows),
    "",
    "## Stata reproduction",
    "",
    rb_report_stata_lines(project_dir),
    "",
    "## Direct replication format",
    "",
    rb_report_drf_lines(project_dir),
    "",
    "## metaregBase",
    "",
    rb_report_metareg_lines(project_dir)
  )

  if (isTRUE(write_metareg_issue_report) &&
      !is.null(metareg_issue_status) &&
      isTRUE(metareg_issue_status$has_reportable_issues)) {
    extra = paste0("- Separate metaregBase issue report: `", metareg_issue_file, "`")
    if (isTRUE(metareg_issue_render_html)) {
      extra = paste0(extra, " and `", metareg_issue_html_file, "`")
    }
    extra = paste0(extra, ".")

    lines = c(
      lines,
      "",
      "## Detailed metaregBase issue report",
      "",
      extra
    )
  }

  if (isTRUE(include_repdb_table)) {
    lines = c(
      lines,
      "",
      "## Stored repdb parcels",
      "",
      rb_report_repdb_lines(project_dir)
    )
  }

  txt = paste0(lines, collapse = "\n")

  if (!isTRUE(write_file)) {
    return(txt)
  }

  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  writeLines(txt, file)

  if (isTRUE(render_html)) {
    rb_report_render_html(
      md_file = file,
      html_file = html_file,
      title = paste0("Reproduction status report: ", artid)
    )
  }

  metareg_issue_result = NULL
  if (isTRUE(write_metareg_issue_report)) {
    metareg_issue_result = rb_report_metareg_issue_report(
      project_dir = project_dir,
      file = metareg_issue_file,
      write_file = TRUE,
      render_html = metareg_issue_render_html,
      html_file = metareg_issue_html_file,
      only_if_issues = TRUE,
      include_notes = TRUE,
      opts = metareg_issue_opts,
      status = metareg_issue_status
    )
  }

  invisible(list(
    md_file = file,
    html_file = if (isTRUE(render_html)) html_file else NULL,
    metareg_issue_report = metareg_issue_result
  ))
}


rb_report_default_html_file = function(md_file) {
  if (stringi::stri_detect_regex(md_file, "[.]md$")) {
    return(stringi::stri_replace_last_regex(md_file, "[.]md$", ".html"))
  }
  paste0(md_file, ".html")
}


rb_report_render_html = function(md_file, html_file = NULL, title = NULL) {
  restore.point("rb_report_render_html")

  if (!file.exists(md_file)) {
    stop(paste0("Markdown file does not exist: ", md_file))
  }

  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package 'rmarkdown' is required to render the HTML report.")
  }

  if (is.null(html_file)) {
    html_file = rb_report_default_html_file(md_file)
  }

  if (is.null(title) || !nzchar(title)) {
    title = tools::file_path_sans_ext(basename(md_file))
  }

  md_lines = readLines(md_file, warn = FALSE, encoding = "UTF-8")
  tmp_rmd = tempfile(fileext = ".Rmd")

  safe_title = stringi::stri_replace_all_fixed(title, '"', '\\"')

  yaml = c(
    "---",
    paste0('title: "', safe_title, '"'),
    "output:",
    "  html_document:",
    "    df_print: paged",
    "---",
    ""
  )

  writeLines(c(yaml, md_lines), tmp_rmd, useBytes = TRUE)

  dir.create(dirname(html_file), recursive = TRUE, showWarnings = FALSE)

  rmarkdown::render(
    input = tmp_rmd,
    output_file = basename(html_file),
    output_dir = dirname(html_file),
    quiet = TRUE,
    envir = new.env(parent = globalenv())
  )

  invisible(html_file)
}


rb_report_metareg_issue_opts = function(
  show_org_data = TRUE,
  show_reg_data = TRUE,
  show_pre_reg_data = TRUE,
  data_head_rows = 10,
  data_tail_rows = 0,
  data_width = 1000,
  max_cases = Inf,
  just_runid = NULL,
  ignore_flags = NULL,
  data_add_org_row = FALSE,
  max_rel_diff_tol = 0.01,
  max_deviation_tol = 1e-6
) {
  as.list(environment())
}


rb_report_metareg_issue_parcels = function(project_dir, parcels = list()) {
  repboxDB::repdb_load_parcels(
    project_dir,
    c(
      "regcoef_diff",
      "reg",
      "reg_rb",
      "reg_cmdpart",
      "regvar",
      "regxvar",
      "regsource",
      "regcoef",
      "regcoef_rb",
      "regcoef_so"
    ),
    parcels = parcels
  )
}


rb_report_metareg_issue_status = function(project_dir, opts = rb_report_metareg_issue_opts(), include_notes = TRUE) {
  restore.point("rb_report_metareg_issue_status")

  library(metaregBase)
  project_dir = normalizePath(project_dir, mustWork = FALSE)

  if (!file.exists(file.path(project_dir, "drf", "path_df.fst"))) {
    return(list(
      available = FALSE,
      has_reportable_issues = FALSE,
      flags = NULL,
      parcels = NULL,
      drf = NULL
    ))
  }

  if (!requireNamespace("metaregBase", quietly = TRUE)) {
    return(list(
      available = FALSE,
      has_reportable_issues = FALSE,
      flags = NULL,
      parcels = NULL,
      drf = NULL
    ))
  }

  if (!requireNamespace("repboxDRF", quietly = TRUE)) {
    return(list(
      available = FALSE,
      has_reportable_issues = FALSE,
      flags = NULL,
      parcels = NULL,
      drf = NULL
    ))
  }

  parcels = rb_report_metareg_issue_parcels(project_dir)
  drf = repboxDRF::drf_load(project_dir, parcels = parcels)

  flags = metaregBase:::mrb_test_generate_flags(project_dir, parcels, drf, opts = opts)

  if (is.null(flags) || NROW(flags) == 0) {
    return(list(
      available = TRUE,
      has_reportable_issues = FALSE,
      flags = flags,
      parcels = parcels,
      drf = drf
    ))
  }

  is_problem = ifelse(is.na(flags$is_problem), FALSE, flags$is_problem)
  is_note = ifelse(is.na(flags$is_note), FALSE, flags$is_note)

  has_reportable_issues = any(is_problem | (isTRUE(include_notes) & is_note))

  list(
    available = TRUE,
    has_reportable_issues = has_reportable_issues,
    flags = flags,
    parcels = parcels,
    drf = drf
  )
}


rb_report_metareg_issue_report = function(
  rb = NULL,
  project_dir = if (!is.null(rb)) rb$project_dir else NULL,
  file = file.path(project_dir, "reports", "metareg_issues.md"),
  write_file = TRUE,
  render_html = TRUE,
  html_file = NULL,
  only_if_issues = TRUE,
  include_notes = TRUE,
  opts = rb_report_metareg_issue_opts(),
  status = NULL
) {
  restore.point("rb_report_metareg_issue_report")

  if (is.null(project_dir)) {
    stop("You must provide project_dir or rb.")
  }

  project_dir = normalizePath(project_dir, mustWork = FALSE)
  artid = basename(project_dir)

  if (is.null(html_file)) {
    html_file = rb_report_default_html_file(file)
  }

  if (is.null(status)) {
    status = rb_report_metareg_issue_status(
      project_dir = project_dir,
      opts = opts,
      include_notes = include_notes
    )
  }

  if (!isTRUE(status$available)) {
    return(invisible(NULL))
  }

  if (isTRUE(only_if_issues) && !isTRUE(status$has_reportable_issues)) {
    return(invisible(NULL))
  }

  body = metaregBase:::mrb_test_report(
    project_dir = project_dir,
    parcels = status$parcels,
    drf = status$drf,
    opts = opts
  )

  txt = paste0(
    c(
      paste0("# metaregBase issue report: ", artid),
      "",
      paste0("- Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      paste0("- Project dir: `", project_dir, "`"),
      "",
      body
    ),
    collapse = "\n"
  )

  if (!isTRUE(write_file)) {
    return(txt)
  }

  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  writeLines(txt, file)

  if (isTRUE(render_html)) {
    rb_report_render_html(
      md_file = file,
      html_file = html_file,
      title = paste0("metaregBase issue report: ", artid)
    )
  }

  invisible(list(
    md_file = file,
    html_file = if (isTRUE(render_html)) html_file else NULL,
    has_reportable_issues = TRUE
  ))
}


rb_report_read_parcel = function(project_dir, parcel_name, dir = file.path(project_dir, "repdb")) {
  file = file.path(dir, paste0(parcel_name, ".Rds"))
  obj = read_rds_or_null(file)
  if (is.null(obj)) {
    return(NULL)
  }

  if (is.list(obj) && !inherits(obj, "data.frame") && parcel_name %in% names(obj)) {
    return(obj[[parcel_name]])
  }

  if (is.list(obj) && length(obj) == 1 && inherits(obj[[1]], "data.frame")) {
    return(obj[[1]])
  }

  obj
}


rb_report_first_existing_col = function(df, cols) {
  cols = cols[cols %in% names(df)]
  if (length(cols) == 0) {
    return(NA_character_)
  }
  cols[[1]]
}


rb_report_fmt_num = function(x, digits = 4) {
  if (length(x) == 0) {
    return("NA")
  }

  x = suppressWarnings(as.numeric(x[[1]]))
  if (is.na(x)) {
    return("NA")
  }

  if (isTRUE(abs(x - round(x)) < 1e-12)) {
    return(format(round(x), scientific = FALSE, trim = TRUE))
  }

  formatC(x, digits = digits, format = "fg", flag = "#")
}


rb_report_fmt_value = function(x) {
  if (length(x) == 0) {
    return("")
  }

  if (is.list(x) && !is.data.frame(x)) {
    x = unlist(x, recursive = TRUE, use.names = FALSE)
  }

  if (length(x) > 1) {
    x = paste0(x, collapse = ", ")
  }

  x = x[[1]]
  if (is.null(x) || is.na(x)) {
    return("")
  }

  if (inherits(x, "POSIXt")) {
    return(format(x, "%Y-%m-%d %H:%M:%S"))
  }

  if (is.numeric(x)) {
    return(rb_report_fmt_num(x))
  }

  as.character(x)
}


rb_report_md_escape = function(x) {
  x = rb_report_fmt_value(x)
  x = stringi::stri_replace_all_fixed(x, "|", "\\|")
  x = stringi::stri_replace_all_regex(x, "\\s*\\n\\s*", "<br>")
  x
}


rb_report_md_table = function(df) {
  if (is.null(df) || NROW(df) == 0) {
    return("_No entries._")
  }

  out = as.data.frame(df, stringsAsFactors = FALSE)
  out[] = lapply(out, function(col) {
    vapply(col, rb_report_md_escape, FUN.VALUE = character(1))
  })

  header = paste0("| ", paste0(names(out), collapse = " | "), " |")
  sep = paste0("| ", paste0(rep("---", NCOL(out)), collapse = " | "), " |")
  rows = apply(out, 1, function(row) {
    paste0("| ", paste0(row, collapse = " | "), " |")
  })

  paste0(c(header, sep, rows), collapse = "\n")
}


rb_report_done_text = function(done) {
  ifelse(isTRUE(done), "done", "missing")
}


rb_report_nrun = function(df) {
  if (is.null(df) || !inherits(df, "data.frame") || NROW(df) == 0 || !("runid" %in% names(df))) {
    return(0L)
  }
  dplyr::n_distinct(df$runid)
}


rb_report_max_or_na = function(x) {
  x = suppressWarnings(as.numeric(x))
  x = x[!is.na(x)]
  if (length(x) == 0) {
    return(NA_real_)
  }
  max(x)
}


rb_report_current_stage = function(project_dir) {
  has_file_info = file.exists(file.path(project_dir, "repdb", "file_info.Rds"))
  has_static = file.exists(file.path(project_dir, "repdb", "stata_cmd.Rds")) ||
    file.exists(file.path(project_dir, "repdb", "r_cmd.Rds"))
  has_raw = file.exists(file.path(project_dir, "repbox", "stata", "repbox_results.Rds"))
  has_post = file.exists(file.path(project_dir, "repdb", "stata_run_info.Rds"))
  has_drf = file.exists(file.path(project_dir, "drf", "path_df.fst"))
  has_mr_base = file.exists(file.path(project_dir, "repdb", "reg.Rds"))
  has_mr_r = file.exists(file.path(project_dir, "repdb", "reg_rb.Rds"))

  if (has_mr_r) {
    return("metaregBase R rerun available")
  }
  if (has_mr_base) {
    return("metaregBase base parcels available")
  }
  if (has_drf) {
    return("DRF created")
  }
  if (has_post) {
    return("Stata postprocess completed")
  }
  if (has_raw) {
    return("raw Stata reproduction completed")
  }
  if (has_static) {
    return("static code analysis completed")
  }
  if (has_file_info) {
    return("supplement inventory created")
  }
  "project initialized only"
}


rb_report_milestone_df = function(project_dir, html_file = NULL, html_will_be_written = FALSE) {
  file_info = rb_report_read_parcel(project_dir, "file_info")
  reg = rb_report_read_parcel(project_dir, "reg")
  reg_rb = rb_report_read_parcel(project_dir, "reg_rb")
  stata_run_info = rb_report_read_parcel(project_dir, "stata_run_info")

  html_files = list.files(
    file.path(project_dir, "reports"),
    pattern = glob2rx("*.html"),
    recursive = TRUE,
    full.names = TRUE
  )

  html_count = length(html_files)
  html_exists = html_count > 0

  if (isTRUE(html_will_be_written) && !is.null(html_file)) {
    html_file_norm = normalizePath(html_file, mustWork = FALSE)
    known_html = normalizePath(html_files, mustWork = FALSE)
    if (!html_file_norm %in% known_html) {
      html_count = html_count + 1L
    }
    html_exists = TRUE
  }

  meta_done = file.exists(file.path(project_dir, "meta", "art_meta.Rds")) &&
    file.exists(file.path(project_dir, "meta", "sup_meta.Rds"))

  stata_post_detail = "repdb/stata_run_info.Rds"
  if (!is.null(stata_run_info) && NROW(stata_run_info) > 0) {
    run_info_fields = c("reg_runs", "ok_reg_runs", "ok_runs", "total_runs")
    run_info_fields = run_info_fields[run_info_fields %in% names(stata_run_info)]
    if (length(run_info_fields) > 0) {
      vals = paste0(
        run_info_fields,
        "=",
        vapply(run_info_fields, function(col) rb_report_fmt_value(stata_run_info[[col]][1]), "")
      )
      stata_post_detail = paste0(vals, collapse = ", ")
    }
  }

  tibble::tibble(
    step = c(
      "Metadata",
      "Supplement inventory",
      "Static Stata code analysis",
      "Static R code analysis",
      "Raw Stata reproduction",
      "Stata postprocess",
      "Direct replication format (DRF)",
      "metaregBase base parcels",
      "metaregBase R rerun",
      "HTML reports"
    ),
    status = c(
      rb_report_done_text(meta_done),
      rb_report_done_text(!is.null(file_info) && NROW(file_info) > 0),
      rb_report_done_text(file.exists(file.path(project_dir, "repdb", "stata_cmd.Rds"))),
      rb_report_done_text(file.exists(file.path(project_dir, "repdb", "r_cmd.Rds"))),
      rb_report_done_text(file.exists(file.path(project_dir, "repbox", "stata", "repbox_results.Rds"))),
      rb_report_done_text(file.exists(file.path(project_dir, "repdb", "stata_run_info.Rds"))),
      rb_report_done_text(file.exists(file.path(project_dir, "drf", "path_df.fst"))),
      rb_report_done_text(!is.null(reg) && NROW(reg) > 0),
      rb_report_done_text(!is.null(reg_rb) && NROW(reg_rb) > 0),
      rb_report_done_text(html_exists)
    ),
    details = c(
      "meta/art_meta.Rds and meta/sup_meta.Rds",
      if (!is.null(file_info) && NROW(file_info) > 0) paste0(NROW(file_info), " files recorded") else "repdb/file_info.Rds",
      "repdb/stata_cmd.Rds",
      "repdb/r_cmd.Rds",
      "repbox/stata/repbox_results.Rds",
      stata_post_detail,
      "drf/path_df.fst",
      if (!is.null(reg) && NROW(reg) > 0) paste0(rb_report_nrun(reg), " regressions") else "repdb/reg.Rds",
      if (!is.null(reg_rb) && NROW(reg_rb) > 0) paste0(rb_report_nrun(reg_rb), " regressions") else "repdb/reg_rb.Rds",
      if (html_exists) paste0(html_count, " html files") else "reports/*.html"
    )
  )
}


rb_report_common_stat_line = function(df, mapping) {
  if (is.null(df) || NROW(df) == 0) {
    return("")
  }

  fields = names(mapping)
  fields = fields[fields %in% names(df)]
  if (length(fields) == 0) {
    return("")
  }

  vals = paste0(
    unname(mapping[fields]),
    " ",
    vapply(fields, function(col) rb_report_fmt_value(df[[col]][1]), "")
  )
  paste0(vals, collapse = ", ")
}


rb_report_metareg_stats = function(project_dir, max_rel_diff_tol = 0.01, max_deviation_tol = 1e-6) {
  reg = rb_report_read_parcel(project_dir, "reg")
  regcoef = rb_report_read_parcel(project_dir, "regcoef")
  reg_rb = rb_report_read_parcel(project_dir, "reg_rb")
  regcoef_rb = rb_report_read_parcel(project_dir, "regcoef_rb")
  regcoef_so = rb_report_read_parcel(project_dir, "regcoef_so")
  diff = rb_report_read_parcel(project_dir, "regcoef_diff")

  has_any =
    (!is.null(reg) && NROW(reg) > 0) ||
    (!is.null(regcoef) && NROW(regcoef) > 0) ||
    (!is.null(reg_rb) && NROW(reg_rb) > 0) ||
    (!is.null(regcoef_rb) && NROW(regcoef_rb) > 0) ||
    (!is.null(regcoef_so) && NROW(regcoef_so) > 0) ||
    (!is.null(diff) && NROW(diff) > 0)

  if (!has_any) {
    return(NULL)
  }

  n_sb = max(rb_report_nrun(reg), rb_report_nrun(regcoef))
  n_rb = max(rb_report_nrun(reg_rb), rb_report_nrun(regcoef_rb))
  n_so = rb_report_nrun(regcoef_so)

  n_rb_errors = 0L
  rb_error_runids = integer(0)
  if (!is.null(reg_rb) && NROW(reg_rb) > 0 && "error_in_r" %in% names(reg_rb)) {
    error_flag = ifelse(is.na(reg_rb$error_in_r), FALSE, reg_rb$error_in_r)
    n_rb_errors = sum(error_flag)
    if ("runid" %in% names(reg_rb)) {
      rb_error_runids = unique(reg_rb$runid[error_flag])
    }
  }

  summarize_diff = function(df, variant1, variant2) {
    if (is.null(df) || NROW(df) == 0) {
      return(list(
        compared = 0L,
        identical = 0L,
        problem = 0L,
        max_rel = NA_real_,
        max_dev = NA_real_,
        problem_runids = integer(0)
      ))
    }

    keep =
      df$compare_what == "all" &
      df$variant1 == variant1 &
      df$variant2 == variant2
    sub = df[keep, , drop = FALSE]

    if (NROW(sub) == 0) {
      return(list(
        compared = 0L,
        identical = 0L,
        problem = 0L,
        max_rel = NA_real_,
        max_dev = NA_real_,
        problem_runids = integer(0)
      ))
    }

    identical = ifelse(is.na(sub$identical), FALSE, sub$identical)
    problem = (!identical) & (
      is.na(sub$max_deviation) |
      sub$max_deviation > max_deviation_tol |
      is.na(sub$max_rel_diff) |
      sub$max_rel_diff > max_rel_diff_tol
    )

    list(
      compared = dplyr::n_distinct(sub$runid),
      identical = sum(identical),
      problem = sum(problem),
      max_rel = rb_report_max_or_na(sub$max_rel_diff),
      max_dev = rb_report_max_or_na(sub$max_deviation),
      problem_runids = unique(sub$runid[problem])
    )
  }

  sb_rb = summarize_diff(diff, "sb", "rb")
  sb_so = summarize_diff(diff, "sb", "so")

  list(
    n_sb = n_sb,
    n_rb = n_rb,
    n_so = n_so,
    n_rb_errors = n_rb_errors,
    rb_error_runids = rb_error_runids,
    sb_rb = sb_rb,
    sb_so = sb_so
  )
}


rb_report_overall_lines = function(project_dir) {
  lines = c(
    paste0("- Current stage: ", rb_report_current_stage(project_dir), ".")
  )

  file_info = rb_report_read_parcel(project_dir, "file_info")
  if (!is.null(file_info) && NROW(file_info) > 0) {
    num_files = NROW(file_info)
    total_mb = sum(suppressWarnings(as.numeric(file_info$mb)), na.rm = TRUE)
    num_stata = sum(file_info$file_type %in% c("do", "ado"), na.rm = TRUE)
    num_r = sum(file_info$file_type %in% c("r", "rmd"), na.rm = TRUE)
    num_data = sum(
      file_info$file_type %in% c("dta", "csv", "xls", "xlsx", "mat", "dat", "sas7bdat", "rdata", "rds", "xml", "json", "tab"),
      na.rm = TRUE
    )

    lines = c(
      lines,
      paste0("- Supplement inventory: ", num_files, " files (", rb_report_fmt_num(total_mb), " MB)."),
      paste0("- Script files: ", num_stata, " Stata and ", num_r, " R."),
      paste0("- Data files: ", num_data, ".")
    )
  } else {
    lines = c(lines, "- Supplement inventory has not been created yet.")
  }

  stata_run_info = rb_report_read_parcel(project_dir, "stata_run_info")
  stata_run_line = rb_report_common_stat_line(
    stata_run_info,
    c(
      total_runs = "total runs",
      ok_runs = "ok runs",
      reg_runs = "regressions",
      ok_reg_runs = "ok regressions"
    )
  )
  if (nzchar(stata_run_line)) {
    lines = c(lines, paste0("- Stata run summary: ", stata_run_line, "."))
  }

  meta_stats = rb_report_metareg_stats(project_dir)
  if (!is.null(meta_stats)) {
    lines = c(
      lines,
      paste0(
        "- metaregBase coverage: ",
        meta_stats$n_sb, " base regressions, ",
        meta_stats$n_rb, " R reruns, ",
        meta_stats$n_so, " original Stata outputs."
      )
    )

    if (meta_stats$sb_rb$compared > 0) {
      lines = c(
        lines,
        paste0(
          "- metaregBase sb vs rb: ",
          meta_stats$sb_rb$identical, " identical, ",
          meta_stats$sb_rb$problem, " flagged, max deviation ",
          rb_report_fmt_num(meta_stats$sb_rb$max_dev), "."
        )
      )
    }
  }

  lines
}


rb_report_issue_lines = function(project_dir, max_rows = 10) {
  lines = character(0)

  problem_df = rb_report_read_parcel(project_dir, "problem")
  if (!is.null(problem_df) && NROW(problem_df) > 0) {
    type_col = rb_report_first_existing_col(problem_df, c("problem_type", "type"))
    msg_col = rb_report_first_existing_col(problem_df, c("problem_descr", "msg", "message"))

    show_rows = seq_len(min(NROW(problem_df), max_rows))
    for (i in show_rows) {
      type = if (!is.na(type_col)) rb_report_fmt_value(problem_df[[type_col]][i]) else "problem"
      msg = if (!is.na(msg_col)) rb_report_fmt_value(problem_df[[msg_col]][i]) else ""
      lines = c(lines, paste0("- Stored problem [", type, "]: ", msg))
    }

    if (NROW(problem_df) > max_rows) {
      lines = c(lines, paste0("- ... and ", NROW(problem_df) - max_rows, " more stored problems."))
    }
  }

  do_run_info = rb_report_read_parcel(project_dir, "stata_do_run_info")
  if (!is.null(do_run_info) && NROW(do_run_info) > 0) {
    if ("timeout" %in% names(do_run_info)) {
      num_timeout = sum(ifelse(is.na(do_run_info$timeout), FALSE, do_run_info$timeout))
      if (num_timeout > 0) {
        lines = c(lines, paste0("- Stata do files with timeout: ", num_timeout, "."))
      }
    }

    if ("has_parse_err" %in% names(do_run_info)) {
      num_parse = sum(ifelse(is.na(do_run_info$has_parse_err), FALSE, do_run_info$has_parse_err))
      if (num_parse > 0) {
        lines = c(lines, paste0("- Stata do files with parse errors: ", num_parse, "."))
      }
    }

    if ("num_load_data_err" %in% names(do_run_info)) {
      num_load_err = sum(suppressWarnings(as.numeric(do_run_info$num_load_data_err)), na.rm = TRUE)
      if (num_load_err > 0) {
        lines = c(lines, paste0("- Stata data load errors recorded across do files: ", rb_report_fmt_num(num_load_err), "."))
      }
    }
  }

  meta_stats = rb_report_metareg_stats(project_dir)
  if (!is.null(meta_stats)) {
    if (meta_stats$n_rb_errors > 0) {
      runids = head(meta_stats$rb_error_runids, 5)
      lines = c(
        lines,
        paste0(
          "- metaregBase R translation or execution errors: ",
          meta_stats$n_rb_errors,
          if (length(runids) > 0) paste0(". Example runids: ", paste0(runids, collapse = ", ")) else ""
        )
      )
    }

    if (meta_stats$sb_rb$problem > 0) {
      runids = head(meta_stats$sb_rb$problem_runids, 5)
      lines = c(
        lines,
        paste0(
          "- metaregBase sb vs rb mismatches above tolerance: ",
          meta_stats$sb_rb$problem,
          if (length(runids) > 0) paste0(". Example runids: ", paste0(runids, collapse = ", ")) else ""
        )
      )
    }

    if (meta_stats$sb_so$problem > 0) {
      runids = head(meta_stats$sb_so$problem_runids, 5)
      lines = c(
        lines,
        paste0(
          "- metaregBase sb vs so mismatches above tolerance: ",
          meta_stats$sb_so$problem,
          if (length(runids) > 0) paste0(". Example runids: ", paste0(runids, collapse = ", ")) else ""
        )
      )
    }
  }

  if (length(lines) == 0) {
    return("- No stored problems or mismatch flags were found.")
  }

  lines
}


rb_report_stata_lines = function(project_dir) {
  raw_file = file.path(project_dir, "repbox", "stata", "repbox_results.Rds")
  run_info = rb_report_read_parcel(project_dir, "stata_run_info")
  do_run_info = rb_report_read_parcel(project_dir, "stata_do_run_info")
  manifest = rb_read_stata_run_manifest(project_dir)

  has_any = file.exists(raw_file) ||
    (!is.null(run_info) && NROW(run_info) > 0) ||
    (!is.null(do_run_info) && NROW(do_run_info) > 0) ||
    !is.null(manifest)

  if (!has_any) {
    return("- No Stata reproduction outputs were found.")
  }

  lines = c(
    paste0("- Raw Stata results: ", ifelse(file.exists(raw_file), "present", "missing"), ".")
  )

  if (!is.null(manifest)) {
    lines = c(
      lines,
      paste0("- Remote manifest recorded: ", ifelse(isTRUE(manifest$repbox_results_exists), "yes", "no"), ".")
    )

    if (!is.null(manifest$github_run_id) && !is.na(manifest$github_run_id) && nzchar(manifest$github_run_id)) {
      lines = c(lines, paste0("- Github Actions run id: ", manifest$github_run_id, "."))
    }
    if (!is.null(manifest$github_sha) && !is.na(manifest$github_sha) && nzchar(manifest$github_sha)) {
      lines = c(lines, paste0("- Github commit SHA: `", manifest$github_sha, "`."))
    }
  }

  run_line = rb_report_common_stat_line(
    run_info,
    c(
      total_runs = "total runs",
      ok_runs = "ok runs",
      reg_runs = "regressions",
      ok_reg_runs = "ok regressions"
    )
  )
  if (nzchar(run_line)) {
    lines = c(lines, paste0("- Run info: ", run_line, "."))
  }

  if (!is.null(do_run_info) && NROW(do_run_info) > 0) {
    num_timeout = if ("timeout" %in% names(do_run_info)) sum(ifelse(is.na(do_run_info$timeout), FALSE, do_run_info$timeout)) else 0L
    num_parse = if ("has_parse_err" %in% names(do_run_info)) sum(ifelse(is.na(do_run_info$has_parse_err), FALSE, do_run_info$has_parse_err)) else 0L
    num_load_err = if ("num_load_data_err" %in% names(do_run_info)) sum(suppressWarnings(as.numeric(do_run_info$num_load_data_err)), na.rm = TRUE) else 0

    lines = c(
      lines,
      paste0(
        "- Do-file summary: ",
        NROW(do_run_info), " files, ",
        num_timeout, " timeouts, ",
        num_parse, " parse errors, ",
        rb_report_fmt_num(num_load_err), " data load errors."
      )
    )
  }

  lines
}


rb_report_drf_lines = function(project_dir) {
  drf_file = file.path(project_dir, "drf", "path_df.fst")
  if (!file.exists(drf_file)) {
    return("- DRF has not been created yet.")
  }

  if (!requireNamespace("repboxDRF", quietly = TRUE)) {
    return("- DRF files are present, but repboxDRF is not installed in this session.")
  }

  drf = repboxDRF::drf_load(project_dir)
  lines = c(
    paste0("- DRF path rows: ", NROW(drf$path_df), "."),
    paste0("- Unique runids in DRF: ", length(unique(drf$path_df$runid)), "."),
    paste0("- Analysis pids in DRF: ", length(unique(drf$path_df$pid)), ".")
  )

  if (!is.null(drf$run_df) && NROW(drf$run_df) > 0) {
    lines = c(lines, paste0("- Stored Stata command rows: ", NROW(drf$run_df), "."))
  }

  lines
}


rb_report_metareg_lines = function(project_dir) {
  stats = rb_report_metareg_stats(project_dir)
  if (is.null(stats)) {
    return("- No metaregBase parcels were found.")
  }

  lines = c(
    paste0("- Base regressions with parcels: ", stats$n_sb, "."),
    paste0("- R reruns available: ", stats$n_rb, "."),
    paste0("- Original Stata outputs available: ", stats$n_so, "."),
    paste0("- R translation or execution errors: ", stats$n_rb_errors, ".")
  )

  if (stats$sb_rb$compared > 0) {
    lines = c(
      lines,
      paste0(
        "- sb vs rb comparisons: ",
        stats$sb_rb$compared, " runs compared, ",
        stats$sb_rb$identical, " identical, ",
        stats$sb_rb$problem, " flagged, max relative diff ",
        rb_report_fmt_num(stats$sb_rb$max_rel), ", max deviation ",
        rb_report_fmt_num(stats$sb_rb$max_dev), "."
      )
    )
  } else {
    lines = c(lines, "- sb vs rb comparisons: none stored.")
  }

  if (stats$sb_so$compared > 0) {
    lines = c(
      lines,
      paste0(
        "- sb vs so comparisons: ",
        stats$sb_so$compared, " runs compared, ",
        stats$sb_so$identical, " identical, ",
        stats$sb_so$problem, " flagged, max relative diff ",
        rb_report_fmt_num(stats$sb_so$max_rel), ", max deviation ",
        rb_report_fmt_num(stats$sb_so$max_dev), "."
      )
    )
  } else {
    lines = c(lines, "- sb vs so comparisons: none stored.")
  }

  lines
}


rb_report_repdb_lines = function(project_dir) {
  repdb_dir = file.path(project_dir, "repdb")
  files = list.files(repdb_dir, pattern = glob2rx("*.Rds"), full.names = TRUE)
  if (length(files) == 0) {
    return("- No repdb parcels stored yet.")
  }

  df = tibble::tibble(
    parcel = tools::file_path_sans_ext(basename(files)),
    mb = file.size(files) / 1e6,
    timestamp = format(file.mtime(files), "%Y-%m-%d %H:%M:%S")
  ) %>%
    dplyr::arrange(parcel) %>%
    dplyr::mutate(mb = vapply(mb, rb_report_fmt_num, ""))

  rb_report_md_table(df)
}
