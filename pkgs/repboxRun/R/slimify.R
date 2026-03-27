example = function() {

  project.dirs = list.files("~/repbox/projects_ejd",recursive = FALSE,full.names = TRUE)
  projects = basename(project.dirs)

  # Just slimify projects that are already in DB
  db = get.repbox.ejd.db()
  db.df = dbGet(db, "project")
  db.projects = db.df$project
  keep = projects %in% db.projects
  project.dirs = project.dirs[keep]


  slim.df = bind_rows(lapply(project.dirs,function(project_dir) {
    res = NULL
    try(res <- slimify.solved.project(project_dir, force=FALSE))
    res
  }))
  saveRDS(slim.df, "~/repbox/slimify_results.Rds")

  project_dir = "~/repbox/projects_ejd/ecta_85_3_4"
  rstudioapi::filesPaneNavigate(paste0(project_dir,"/repbox"))
  res = slimify.solved.project(project_dir, force=TRUE)
}

check.size = function(project_dir) {
  all.files = list.files(project_dir,recursive=TRUE, full.names=TRUE)
  fi = file.info(all.files)
  fi$mb = fi$size / 1e6
  fi$file = all.files
  fi$ext = tools::file_ext(fi$file)
  fi = arrange(fi, desc(mb))
  fi$file[1]
}


slimify.solved.project = function(project_dir, max.org.mb = 10, max.log.mb = 1, max.stata.res.mb = 40, max.matching.mb = 10, max.cmd.mb = 0, force=FALSE, keep.org.code = FALSE) {
  restore.point("slimify.solved.project")
  slim.file = file.path(project_dir,"repbox/slimify.Rds")
  if (file.exists(slim.file)) {
    slimify = readRDS(slim.file)
    if (!force) return(slimify)
    had.old = TRUE
  } else {
    had.old = FALSE
  }
  cat("\nslimify ", project_dir)

  all.files = list.files(project_dir,recursive=TRUE, full.names=TRUE)
  all.files = enc2utf8(all.files)
  fi = file.info(all.files)
  fi$mb = fi$size / 1e6
  fi$file = all.files
  fi$ext = tools::file_ext(fi$file)

  project_dir = normalizePath(project_dir, mustWork=FALSE)
  org.fi = filter(fi, startsWith(file, file.path(project_dir,"org")))
  mod.fi = filter(fi, startsWith(file, file.path(project_dir,"mod")))
  repbox.fi = filter(fi, startsWith(file, file.path(project_dir,"repbox")))
  log.fi = filter(repbox.fi, startsWith(file, file.path(project_dir,"repbox/stata/logs")))
  stata.res.fi = filter(repbox.fi, startsWith(file, file.path(project_dir,"repbox/stata/repbox_results.Rds")))
  matching.fi = filter(repbox.fi, startsWith(file, file.path(project_dir,"repbox/matched_tabs.Rds")))
  cmd.fi = filter(repbox.fi, startsWith(file, file.path(project_dir,"repbox/stata/cmd")))


  pre.mb = sum(fi$mb, na.rm=TRUE)
  mod.mb = sum(mod.fi$mb, na.rm=TRUE)
  org.mb = sum(org.fi$mb, na.rm=TRUE)
  repbox.mb = sum(repbox.fi$mb, na.rm=TRUE)
  log.mb = sum(log.fi$mb, na.rm=TRUE)
  stata.res.mb = sum(stata.res.fi$mb, na.rm=TRUE)
  matching.mb = sum(matching.fi$mb, na.rm=TRUE)
  cmd.mb = sum(cmd.fi$mb, na.rm=TRUE)

  # Remove mod folder including do files
  remove.dir(file.path(project_dir, "mod"))
  #non.do.files = filter(mod.fi,ext!="do")$file
  #file.remove(non.do.files)

  # Remove org folder
  slimify.org = org.mb > max.org.mb
  if (slimify.org) {
    if (!keep.org.code) {
      remove.dir(file.path(project_dir, "org"))
    } else {
      non.code.files = filter(org.fi,! tolower(ext) %in% c("do","r","py","m","rmd","ado","jl"))$file
      file.remove(non.code.files)
    }
  }

  slimify.log = log.mb > max.log.mb
  if (slimify.log) {
    file.remove(log.fi$file)
  }

  slimify.stata.res = stata.res.mb > max.stata.res.mb
  if (slimify.stata.res) {
    file.remove(stata.res.fi$file)
  }

  slimify.matching = matching.mb > max.matching.mb
  if (slimify.matching) {
    file.remove(matching.fi$file)
  }

  slimify.cmd = cmd.mb > max.cmd.mb
  if (slimify.cmd) {
    file.remove(cmd.fi$file)
  }


  post.files = list.files(project_dir,recursive=TRUE, full.names=TRUE)
  post.files = enc2utf8(post.files)
  post.fi = filter(fi, file %in% post.files)
  post.mb = sum(post.fi$mb, na.rm=TRUE)

  if (!had.old) {
    slimify = tibble(project=basename(project_dir),pre.mb,post.mb,slimify.org, slimify.log, slimify.stata.res, slimify.matching, slimify.cmd,  mod.mb, org.mb, repbox.mb, log.mb, stata.res.mb,matching.mb, cmd.mb, max.org.mb, max.log.mb, max.stata.res.mb, max.matching.mb, max.cmd.mb, kept.org.code = keep.org.code | !slimify.org, project_dir=project_dir,timestamp = Sys.time())
  } else {
    slimify$post.mb = post.mb
    slimify$slimify.log = slimify.log | slimify$slimify.log
    slimify$slimify.org = slimify.org | slimify$slimify.org
    slimify$slimify.stata.res = slimify.stata.res | slimify$slimify.stata.res
    slimify$slimify.matching = slimify.matching | slimify$slimify.matching
    slimify$slimify.cmd = slimify.log | isTRUE(slimify$slimify.cmd)
    slimify$max.log.mb = min(c(slimify$max.log.mb, max.log.mb))
    slimify$max.org.mb = min(c(slimify$max.org.mb, max.org.mb))
    slimify$max.stata.res.mb = min(c(slimify$max.stata.res.mb, max.stata.res.mb))
    slimify$max.matching.mb = min(c(slimify$max.matching.mb, max.matching.mb))
    slimify$max.cmd.mb = min(c(slimify$max.cmd.mb, max.cmd.mb))
    slimify$kept.org.code = keep.org.code | !slimify.org
    slimify$timestamp = Sys.time()
  }

  cat(paste0(" from ", round(pre.mb,3), " MB to ", round(post.mb,3)," MB.\n"))

  saveRDS(slimify, file.path(project_dir,"repbox","slimify.Rds"))
  return(slimify)
}



slimify.org.dir = function(project_dir,  keep.org.code = TRUE) {
  restore.point("slimify.org.dir")
  cat("\nSlimify", file.path(project_dir,"org"))

  all.files = list.files(file.path(project_dir,"org"),recursive=TRUE, full.names=TRUE)
  all.files = enc2utf8(all.files)
  fi = file.info(all.files)
  fi$mb = fi$size / 1e6
  fi$file = all.files
  fi$ext = tools::file_ext(fi$file)

  pre.mb = sum(fi$mb, na.rm=TRUE)
  org.fi = fi

  # Remove org folder
  if (!keep.org.code) {
    remove.dir(file.path(project_dir, "org"))
  } else {
    non.code.files = filter(org.fi,! tolower(ext) %in% c("do","r","py","m","rmd","ado","jl"))$file
    file.remove(non.code.files)
  }

  post.files = list.files(file.path(project_dir,"org"),recursive=TRUE, full.names=TRUE)
  post.files = enc2utf8(post.files)
  post.fi = filter(fi, file %in% post.files)
  post.mb = sum(post.fi$mb, na.rm=TRUE)
  cat(paste0(" from ", round(pre.mb,3), " MB to ", round(post.mb,3)," MB.\n"))
  return(TRUE)
}


example.size.info = function() {
  project.dirs = readLines("~/repbox/jobs/project_dirs_use.txt")
  sizes = bind_rows(lapply(project.dirs, project.size.info))
}


project.size.info = function(project_dir) {
  restore.point("project.size.info")
  all.files = list.files(project_dir,recursive=TRUE, full.names=TRUE)
  all.files = enc2utf8(all.files)
  fi = file.info(all.files)
  fi$mb = fi$size / 1e6
  fi$file = all.files
  fi$ext = tools::file_ext(fi$file)

  project_dir = normalizePath(project_dir, mustWork=FALSE)
  org.fi = filter(fi, startsWith(file, file.path(project_dir,"org")))
  mod.fi = filter(fi, startsWith(file, file.path(project_dir,"mod")))
  repbox.fi = filter(fi, startsWith(file, file.path(project_dir,"repbox")))
  log.fi = filter(repbox.fi, startsWith(file, file.path(project_dir,"repbox/stata/logs")))
  stata.res.fi = filter(repbox.fi, startsWith(file, file.path(project_dir,"repbox/stata/repbox_results.Rds")))
  matching.fi = filter(repbox.fi, startsWith(file, file.path(project_dir,"repbox/matched_tabs.Rds")))
  cmd.fi = filter(repbox.fi, startsWith(file, file.path(project_dir,"repbox/stata/cmd")))


  total.mb = sum(fi$mb, na.rm=TRUE)
  mod.mb = sum(mod.fi$mb, na.rm=TRUE)
  org.mb = sum(org.fi$mb, na.rm=TRUE)
  repbox.mb = sum(repbox.fi$mb, na.rm=TRUE)
  log.mb = sum(log.fi$mb, na.rm=TRUE)
  stata.res.mb = sum(stata.res.fi$mb, na.rm=TRUE)
  matching.mb = sum(matching.fi$mb, na.rm=TRUE)
  cmd.mb = sum(cmd.fi$mb, na.rm=TRUE)

  cat("\n", project_dir, " log.mb ", round(log.mb,2), " total.mb ", round(total.mb,2))
  info = tibble(project=basename(project_dir),total.mb,log.mb, cmd.mb,  mod.mb, org.mb, repbox.mb, stata.res.mb,matching.mb, project_dir=project_dir,timestamp = Sys.time())
  return(info)
}
