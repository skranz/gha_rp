example = function() {
  library(repboxRun)
  project_dir = "~/repbox/projects/aejapp_1_3_4"
  rb_deploy_run_tpl(project_dir, overwrite=TRUE)
  rb_deploy_f2p_toml(project_dir, overwrite=TRUE)
  rb_run_scripts_job(project_dir, c("pre_gha","gha","post_gha"))
  rb_run_scripts_job(project_dir, c("post_gha"))
  rb_run_scripts(project_dir, "pre_gha")
  rstudioapi::filesPaneNavigate(project_dir)




}

rb_run_scripts_job = function(project_dirs, tpl_names = NULL, job_file="~/repbox/jobs/rb_script_job.R") {
  restore.point("rb_run_scripts_job")
  job_code = paste0('
suppressWarnings(suppressPackageStartupMessages({
library(repboxRun)
library(ExtractSciTab)
library(repboxEJD)
library(repboxR)
library(repboxStata)
library(repboxTableTools)
library(repboxDoc)
}))

project_dirs = c(', paste0('"',project_dirs,'"', collapse=","),')
tpl_names = c(', paste0('"',tpl_names,'"', collapse=","),')
rb_run_scripts(project_dirs, tpl_names)
  ')


  name = ""
  if (length(project_dirs)==1) {
    name = paste0(basename(project_dirs), ": ")
  } else {
     name = paste0(name,NROW(project_dirs)," projects: ")
  }
  if (length(tpl_names)<=3) {
    name = paste0(name, paste0(tpl_names, collapse=", "))
  } else {
    name = paste0(name,NROW(tpl_names)," scripts")
  }

  writeLines(job_code, job_file)
  rstudioapi::jobRunScript(job_file, name=name)
}

rb_run_scripts = function(project_dirs, tpl_names) {
  for (project_dir in project_dirs) {
    for (tpl_name in tpl_names) {
      script_file = paste0(project_dir, "/run/run_", tpl_name,".R")
      if (!file.exists(script_file)) {
        cat("\nWarning: Script ", script_file, " does not exist.\n")
        next
      }
      log_file = paste0(project_dir, "/run/log_", tpl_name,".txt")
      cat("\n**************************************************************\nRun ", script_file, "\n**************************************************************\n")
      source_with_log(script_file,log_file)
    }
  }
}


rb_deploy_run_tpl = function(project_dir, tpl_names = NULL,overwrite=FALSE, args = list(overwrite=overwrite)) {
  restore.point("rb_deploy_run_tpl")
  project_dir = normalizePath(project_dir)
  tpl_dir = system.file("tpl", package="repboxRun")
  if (length(tpl_names)==0) {
    tpl_files = list.files(tpl_dir, glob2rx("*.R"),full.names = FALSE)
    tpl_names = str.between(tpl_files, "tpl_", ".R")
  }
  run_dir = file.path(project_dir, "run")
  if (!dir.exists(run_dir)) dir.create(run_dir, recursive = TRUE)
  tpl_name = tpl_names[[1]]

  args$project_dir = project_dir
  args$default_project_dir = project_dir

  rx_pattern = paste0("{{", names(args),"}}")
  rx_repl = unlist(args)




  for (tpl_name in tpl_names) {
    file = paste0(tpl_dir, "/tpl_", tpl_name, ".R")
    txt = read_utf8(file)
    txt = stringi::stri_replace_all_fixed(txt, rx_pattern, rx_repl, vectorize_all = FALSE)
    out_file = paste0(run_dir, "/run_", tpl_name,".R")
    writeUtf8(txt, out_file)

  }
  cat("\nGenerated ", length(tpl_names), " run files in ", run_dir, "\n")
  invisible(tpl_name)
}


rb_deploy_f2p_toml = function(project_dir, tpl_names = NULL,overwrite=FALSE, args = list(overwrite=overwrite)) {
  restore.point("rb_deploy_f2p_toml")
  project_dir = normalizePath(project_dir)
  tpl_dir = system.file("tpl", package="repboxRun")
  #stop()
  if (length(tpl_names)==0) {
    tpl_files = list.files(tpl_dir, glob2rx("tpl_f2p*.toml"),full.names = FALSE)
    tpl_names = str.between(tpl_files, "tpl_", ".toml")
  }
  tpl_name = tpl_names[[1]]

  if (length(tpl_names)==0) return()

  run_dir = file.path(project_dir, "run")
  if (!dir.exists(run_dir)) dir.create(run_dir, recursive = TRUE)

  args$project_dir = project_dir
  rx_pattern = paste0("{{{", names(args),"}}}")
  rx_repl = unlist(args)
  artid = basename(project_dir)

  for (tpl_name in tpl_names) {
    file = paste0(tpl_dir, "/tpl_", tpl_name, ".toml")
    txt = read_utf8(file)
    txt = stringi::stri_replace_all_fixed(txt, rx_pattern, rx_repl, vectorize_all = FALSE)
    out_file = paste0(run_dir, "/",tpl_name,"_",artid,".toml")
    writeUtf8(txt, out_file)

  }
  cat("\nGenerated ", length(tpl_names), " f2p TOML files in ", project_dir, "\n")
  invisible(tpl_name)
}


source_with_log = function(script_file, log_file,
                           echo = TRUE,
                           keep.source = TRUE,
                           append = FALSE,
                           envir = parent.frame()) {
  log_con = file(log_file, open = if (append) "at" else "wt")
  output_sink_start = sink.number(type = "output")
  old_warn = getOption("warn")
  options(warn = 1)

  timestamp = function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  log_line = function(prefix, text) {
    line = paste0("[", timestamp(), "] ", prefix, ": ", text)
    if (isOpen(log_con)) {
      writeLines(line, log_con)
      flush(log_con)
    }
  }

  cleanup = function() {
    options(warn = old_warn)

    while (sink.number(type = "output") > output_sink_start) {
      ok = tryCatch(
        {
          sink(type = "output")
          TRUE
        },
        error = function(e) FALSE
      )
      if (!ok) break
    }

    tryCatch(
      {
        if (isOpen(log_con)) close(log_con)
      },
      error = function(e) NULL
    )
  }

  on.exit(cleanup(), add = TRUE)

  writeLines(
    paste0("\n===== source_with_log started: ", timestamp(), " ====="),
    log_con
  )
  flush(log_con)

  sink(log_con, type = "output", split = TRUE)

  result = tryCatch(
    withCallingHandlers(
      source(
        script_file,
        echo = echo,
        keep.source = keep.source,
        local = envir
      ),
      message = function(m) {
        log_line("MESSAGE", conditionMessage(m))
      },
      warning = function(w) {
        log_line("WARNING", conditionMessage(w))
      }
    ),
    error = function(e) {
      log_line("ERROR", conditionMessage(e))
      stop(e)
    }
  )

  writeLines(
    paste0("===== source_with_log finished: ", timestamp(), " =====\n"),
    log_con
  )
  flush(log_con)

  invisible(result)
}
