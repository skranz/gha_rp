#' Returns all steps of a repbox analysis
#'
#' Can be used as argument steps in repbox_run_project
repbox_steps_all = function() {
  repbox_steps_from(TRUE)
}

#' Returns steps of a repbox analysis specified by starting or ending steps
#'
#' Can be used as argument steps in repbox_run_project
#'
#' The arguments of this function are the diffent possible steps in a repbox analysis
repbox_steps_from = function(file_info = FALSE, static_code=file_info, art=static_code, reproduction=art, reg=reproduction, mr_base=reg, repbox_repdb = mr_base, map=repbox_repdb, html=map) {
  list(file_info = file_info, static_code = static_code, art=art, reproduction=reproduction, reg=reg, mr_base=mr_base,repbox_repdb = repbox_repdb, map=map, html=html)
}

#' Specify options for repbox analysis
#'
#' @export
repbox_run_opts = function(stop.on.error = TRUE, stata_version = 17, slimify = FALSE, slimify_org=slimify, store_data_caches=TRUE, timeout = 60*5, remove_existing_problems=TRUE, make_script_parcel=TRUE, stata_opts = repbox_stata_opts(timeout = timeout,all.do.timeout = timeout),r_opts = repbox_r_opts(), art_opts = repbox_art_opts(), map_opts=repbox_map_opts(), html_opts = repbox_html_opts(), problem_fail_action= if(stop.on.error) "error" else "msg") {
  list(
    stop.on.error = stop.on.error,
    problem_fail_action = problem_fail_action,
    stata_version = stata_version,
    timeout = timeout,
    store_data_caches = store_data_caches,
    slimify = slimify,
    slimify_org = slimify,
    remove_existing_problems = remove_existing_problems,
    make_script_parcel = make_script_parcel,
    stata_opts = stata_opts,
    r_opts = r_opts,
    art_opts = art_opts,
    map_opts = map_opts,
    html_opts = html_opts
  )
}
