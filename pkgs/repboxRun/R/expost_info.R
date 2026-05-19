example = function() {
  parent_dir = "~/repbox/projects_static"
  parent_dir = "~/repbox/projects_gha"
  project_dirs = list.dirs(parent_dir, recursive = FALSE)
  for (project_dir in project_dirs) {
    repbox_store_project_problems(project_dir)
  }

  project_dir = first(project_dirs)
  for (project_dir in project_dirs) {
    repbox_store_step_timing(project_dir)
  }

  project_dir = project_dirs[1]
}

repbox_store_step_timing = function(project_dir, parcels=list()) {
  step_files = list.files(file.path(project_dir,"steps"), glob2rx("*.Rds"),full.names = TRUE)

  base = basename(step_files)

  time_li = lapply(step_files, function(file) {
    dat = readRDS(file)
    as.numeric(dat[[1]])
  }) %>% unlist()


  df = data.frame(
    step = str.left.of(base, "."),
    start_end = str.between(base, ".","."),
    time = time_li
  ) %>% arrange(step, start_end)



  step_timing = df %>%
    group_by(step) %>%
    summarize(
      sec = ifelse(n()==2, as.numeric(time[1])-as.numeric(time[2]), NA_real_)
    ) %>%
    mutate(artid = basename(project_dir))

  repdb_dir = file.path(project_dir,"repdb")
  parcels$step_timing = list(step_timing=step_timing)
  repdb_save_parcels(parcels["step_timing"],dir = repdb_dir)
  parcels
}

repbox_store_project_problems = function(project_dir, parcels=list()) {
  restore.point("repbox_store_project_problems")

  prob_li = vector("list",0)

  parcels = repdb_load_parcels(project_dir, "stata_do_run_info",parcels = parcels)
  info = parcels$stata_do_run_info$stata_do_run_info
  if (NROW(info)>0) {
    num = sum(info$timeout, na.rm = TRUE)
    if (num > 0) {
      prob_li[[length(prob_li)+1]] = list(type="timeout", msg = paste0(num, " do files had a timeout."))
    }
    num = sum(info$num_load_data_err, na.rm=TRUE)
    if (num > 0) {
      prob_li[[length(prob_li)+1]] = list(type="missing_data", msg = paste0(num, " data load commands did not work."))
    }
    num = sum(info$has_parse_err, na.rm=TRUE)
    if (num > 0) {
      prob_li[[length(prob_li)+1]] = list(type="parse_error", msg = paste0(num, " do files had a parse errors."))
    }
  }


  problem_dir = file.path(project_dir,"problems")
  repdb_dir = file.path(project_dir,"repdb")
  prob_files = list.files(problem_dir,full.names = TRUE)

  prob_li = c(
    prob_li,
    lapply(prob_files, readRDS)
  )
  if (length(prob_li)==0) {
    problem_rds = file.path(repdb_dir,"problem.Rds")
    if (file.exists(problem_rds)) {
      file.remove(problem_rds)
    }
    return(parcels)
  }

  prob_df = lapply(prob_li, function(x) {
    as.data.frame(x[1:2])
  }) %>%
    bind_rows()
  names(prob_df) = c("problem_type", "problem_descr")

  prob_df$artid = basename(project_dir)

  parcels$problem = list(problem=prob_df)
  repdb_save_parcels(parcels["problem"],dir = repdb_dir)
  parcels
}


repbox_store_step_info = function(project_dir) {

}

