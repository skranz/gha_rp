repbox_set_problem_options = function(project_dir=NULL, fail_action="error", metaid=NA_character_, forced_fail_action = NULL) {
  options(repbox.problem.options = list(project_dir=project_dir, fail_action=fail_action, metaid=metaid, forced_fail_action=forced_fail_action))
}

repbox_problem_opts = function(project_dir, fail_action="error") {
  getOption("repbox.problem.options")
}

repbox_runid_problems = function() {
  getOption("repbox.runid.problems")
}

repbox_problem_set_runid = function(runid=NA_integer_) {
  options(repbox.problem.runid = runid)
  if (!is.na(runid)) {
    options(repbox.runid.problems = list())
  }
}

repbox_problem_get_runid = function() {
  runid = getOption("repbox.problem.runid")
  if (is.null(runid)) runid = NA_integer_
  runid
}




#' A function that deals with failures depending on the on_fail action
make_fail_fun = function(fail_action) {
  if (fail_action=="error") {
    fail_fun = function(...) stop(paste0(...),call. = FALSE)
  } else if (fail_action=="warn") {
    fail_fun = function(...) warning(paste0(...), call. = FALSE)
  } else if (fail_action=="msg") {
    fail_fun = function(...) cat(paste0("\n",...,"\n"))
  } else {
    fail_fun = function(...) {}
  }
  fail_fun
}

repbox_set_current_project_dir = function(project_dir) {
  options(repbox.current.project.dir = project_dir)
}

repbox_get_current_project_dir = function() {
  getOption("repbox.current.project.dir")
}

#' A function that deals with failures depending on the on_fail action
repbox_problem = function(msg, type, fail_action=opts$fail_action, project_dir=opts$project_dir,  extra=list(),metaid=opts$metaid, runid=repbox_problem_get_runid(), opts=repbox_problem_opts()) {
  restore.point("repbox_problem")


  if (!is.null(opts$forced_fail_action)) {
    fail_action = opts$forced_fail_action
  }


  type = make_valid_filename(type)
  if (is.null(project_dir)) {
    stop("project_dir not specfied")
  }
  prob = list(type=type,msg=msg, metaid=metaid, runid=runid, extra=extra)

  hash = digest::digest(prob)

  prob$hash = hash
  prob$time = Sys.time()
  if (is.null(metaid)) metaid = NA
  if (is.na(metaid) | isTRUE(metaid %in% c("base"))) {
    problem_dir = file.path(project_dir,"problems")
  } else {
    problem_dir = file.path(project_dir,"metareg", metaid, "problems")
  }

  if (!dir.exists(problem_dir)) dir.create(problem_dir,recursive = TRUE)

  short_hash = substr(hash, 1,8)
  if (is.na(runid)) {
    #num_files = length(list.files(problem_dir))
    #prob_num = num_files +1
    saveRDS(prob, paste0(problem_dir,"/problem_", type,"__", short_hash, ".Rds"))
  } else {
    saveRDS(prob, paste0(problem_dir,"/runid_problem_", runid,"__", type,"__", short_hash, ".Rds"))
  }

  if (!is.na(runid)) {
    runid_problems = getOption("repbox.runid.problems")
    if (is.null(runid_problems)) {
      runid_problems = list(prob)
    } else {
      runid_problems = c(runid_problems, list(prob))
    }
    options(repbox.runid.problems = runid_problems)
  }

  if (fail_action=="error") {
    stop(msg,call. = FALSE)
  } else if (fail_action=="warn") {
    warning(msg, call. = FALSE)
  } else if (fail_action=="msg") {
    cat(paste0("\n",msg,"\n"))
  }
  invisible(prob)
}


try_catch_repbox_problems <- function(expr,project_dir, warn_action="msg", err_action="msg", runid=NULL, msg_prefix="", err_val=NULL) {
  warn_li = list()
  err <- NULL
  value <- withCallingHandlers(
    tryCatch(expr, error=function(e) {
      err <<- paste0(as.character(e), collapse="\n")#captureOutput(print(e))#
      NULL
    }), warning=function(w) {
      warn_li[[length(warn_li)+1]] <<- captureOutput(print(w), collapse="\n")#as.character(w)
      invokeRestart("muffleWarning")
    })

  if (!is.null(err)) {
    value = err_val
    repbox_problem(paste0(msg_prefix,err),"error", fail_action=err_action, project_dir=project_dir, runid=runid)
  }
  for (w in warn_li) {
    repbox_problem(paste0(msg_prefix,w),"warning", fail_action=warn_action, project_dir=project_dir, runid=runid)
  }

  list(value=value, warnings=warn_li, error=err, has_err = !is.null(err))
}


tryCatchWarningsAndError <- function(expr) {
  warn_li = list()
  err <- NULL
  value <- withCallingHandlers(
    tryCatch(expr, error=function(e) {
      err <<- e
      NULL
    }), warning=function(w) {
      warn_li[[length(warn_li)+1]] <<- w
      invokeRestart("muffleWarning")
    })
  list(value=value, warnings=warn_li, error=err)
}
