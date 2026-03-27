
# Can be made a bit more sophisticated later
rb_get_project_dir = function(project_dir) {
  return(project_dir)
}

rb_new = function(project_dir, just_steps=NULL, ignore_steps=NULL, copy_existing=TRUE, parent_env = parent.frame(), fail_action="error") {
  restore.point("rb_make_rb")

  if (!dir.exists(project_dir))
    stop(paste0("project_dir=", as.character(project_dir), " does not exist. You must manually create it."))

  if (copy_existing) {
    old_rb = parent_env$rb
    if (isTRUE(old_rb$project_dir==project_dir)) {
      rb = old_rb
      rb$just_steps = just_steps
      rb$ignore_steps = ignore_steps
      rb$fail_action = fail_action
      rb_set_problem_opts(rb)
      return(old_rb)
    }
  }

  rb = list(
    project_dir = project_dir,
    just_steps = just_steps,
    ignore_steps = ignore_steps,
    parcels = list(),
    opts =  rb_options(),
    fail_action = fail_action
  )
  rb_set_problem_opts(rb)
  rb
}

rb_set_problem_opts = function(rb) {
  repbox_set_problem_options(rb$project_dir,fail_action = rb$fail_action)
  invisible(rb)
}

rb_shall_perform_step = function(rb, step) {
  if (step %in% rb$ignore_steps) return(FALSE)
  if (!is.null(rb$just_steps)) {
    if (!step %in% rb$just_steps) return(FALSE)
  }
  return(TRUE)
}



get_rb_opts = function() {
  rb_options()
}

rb_options = function(install_from = "local_ejd",stop.on.error = TRUE, stata_version = 17, store_data_caches=TRUE, timeout = 60*5, remove_existing_problems=TRUE,stata_opts = NULL,r_opts = NULL, problem_fail_action= if(stop.on.error) "error" else "msg", ...) {
  opts = as.list(environment())
  if (is.null(stata_opts)) {
    if (require(repboxStata))
      opts$stata_opts = repbox_stata_opts(timeout = timeout,all.do.timeout = timeout)
  }
  if (is.null(r_opts)) {
    if (require(repboxR))
      opts$r_opts = repbox_r_opts()
  }
  opts
}

rb_ensure_parcel = function(rb, parcel_name,  parcels = rb$parcels) {
  restore.point("rb_ensure_parcel")
  if (is.null(rb$parcels)) stop("rb$parcels is NULL. Did you correctly initiate rb?")
  rb$parcels = repdb_load_parcels(rb$project_dir,parcel_names = parcel_name, parcels = rb$parcels)
  is.null = sapply(rb$parcels[parcel_name], is.null)
  null_names = names(is.null[is.null])
  if (length(null_names)) {
    stop(paste0("Parcels ", paste0(null_names, collapse=", "), " have not yet been generated."))
  }
  rb
}

rb_save_parcel = function(rb, parcel_name, dat, dir = file.path(rb$project_dir,"repdb"), check=FALSE) {
  if (!missing(dat)) {
    parcel = dat
    rb$parcels[[parcel_name]] = parcel
  } else {
    dat = NULL
  }




  restore.point("rb_save_parcel")
  if (check & !repdb_has_spec(parcel_name) & !is.null(dat)) {
    repdb_make_spec_from_data(parcel_name, dat)
  }
  if (!repdb_has_spec(parcel_name)) {
    stop(paste0("No repboxDB spec for ", parcel_name))
    file = file.path(dir,paste0(parcel_name,".Rds"))
    saveRDS(rb$parcels[[parcel_name]], file)
  } else {
    repdb_save_parcels(rb$parcels[parcel_name],dir=dir,check = check)
  }
  rb
}

rb_parcel = function(rb,parcel_name) {
  rb$parcels[[parcel_name]]
}

rb_set_flag = function(rb,...) {
  flags = list(...)
  rb[names(flags)] = flags
  rb
}

rb_require_complete_org_dir = function(rb, msg=NULL) {
  restore.point("rb_require_complete_org_dir")
  if (is.null(msg)) msg=paste0("You need a complete org directory for ", rb$project_dir)
  if (isTRUE(!rb_has_complete_org_dir(rb=rb))) {
    stop(msg)
  }
}

rb_require_complete_mod_dir = function(rb, msg=NULL) {
  if (is.null(msg)) msg="You need a complete org directory"
  if (isTRUE(!rb_has_complete_mod_dir(rb=rb))) {
    stop(msg)
  }
}

rb_has_stata = function(rb) {
  rb_has_lang(rb, "stata")
}

rb_has_r = function(rb) {
  rb_has_lang(rb, "r")
}

rb_has_lang = function(rb, lang = c("stata","r")[1]) {
  rb=rb_load_parcels(rb, "script_type")
  lang_df = rb_parcel(rb,"script_type")
  if (is.null(lang_df)) return(FALSE)
  if (lang=="stata") {
    return("do" %in% lang_df$file_type)
  } else {
    return(lang %in% lang_df$file_type)
  }
}
