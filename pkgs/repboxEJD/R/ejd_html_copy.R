example = function() {
  copy_all_new_ejd_www()

  parent_dirs = "~/repbox/gha_ejd/projects_ejd_gha2"
  find_new_ejd_html_projects(parent_dirs)
}

repbox_ejd_www_dir = function() {
  normalizePath("~/web/ejd/repbox")
}

copy_all_shared_ejd_html = function(www_dir=repbox_ejd_www_dir()) {
  library(repboxHtml)
  upper_dirs = list.dirs(www_dir, recursive = FALSE)
  dir = upper_dirs[1]
  for (dir in upper_dirs) {
    repboxHtml::repbox_copy_shared_www(www_dir = dir)
  }
  rstudioapi::filesPaneNavigate(dir)

}

find_ejd_html_projects = function(parent_dirs = "~/repbox/gha_ejd/projects_ejd_gha2") {
  restore.point("find_ejd_html_projects")

  project_dirs = list.dirs(parent_dirs, recursive = FALSE)


  ejd_dirs = paste0(project_dirs, "/reports/report_ejd")
  use = dir.exists(ejd_dirs)
  project_dirs[use]

  #images_dirs = paste0(project_dirs,"/reports/images")
  #has_images = dir.exists(images_dirs)

}

find_ejd_www_dirs = function( www_dir = repbox_ejd_www_dir()) {
  upper_dirs = list.dirs(www_dir, recursive = FALSE, full.names = TRUE)
  list.dirs(upper_dirs, recursive=FALSE, full.names=TRUE)


}

find_new_ejd_html_projects = function(parent_dirs, www_dir =  repbox_ejd_www_dir()) {
  restore.point("find_new_ejd_html_projects")

  project_dirs = find_ejd_html_projects(parent_dirs)

  exist_dirs = find_ejd_www_dirs(www_dir)

  artids = basename(project_dirs)
  exist_artids = basename(exist_dirs)

  is_new = !artids %in% exist_artids

  project_dirs[is_new]
  #images_dirs = paste0(project_dirs,"/reports/images")
  #has_images = dir.exists(images_dirs)
}


copy_all_new_ejd_www = function(project_parent_dirs="~/repbox/gha_ejd/projects_ejd_gha2", www_dir =  repbox_ejd_www_dir(), overwrite=FALSE) {
  restore.point("copy_all_ejd_www")
  if (overwrite) {
    project_dirs = find_ejd_html_projects(project_parent_dirs)
  } else {
    project_dirs = find_new_ejd_html_projects(project_parent_dirs, www_dir)
  }
  i = 1
  res = lapply(seq_along(project_dirs), function(i) {
    project_dir = project_dirs[i]
    cat("\n",i,project_dir)
    copy_ejd_www(project_dir, www_dir, overwrite=overwrite)
  })

}


copy_ejd_www = function(project_dir, www_dir =  repbox_ejd_www_dir(), overwrite=TRUE) {
  restore.point("copy_ejd_www")
  #stop()
  source.dir = file.path(project_dir, "reports","report_ejd")
  if (!file.exists(source.dir)) return()
  artid = basename(project_dir)
  journ = str.left.of(artid,"_")
  #art = readRDS(file.path(project_dir,"meta","art.Rds"))

  dest.dir = file.path(www_dir,journ,artid)
  if (dir.exists(dest.dir)) {
    if (!overwrite) return()
    remove.dir(dest.dir,must.contain = www_dir)
  }
  try(dir.create(dest.dir,recursive = TRUE,showWarnings = FALSE))
  copy.dir(source.dir, dest.dir)

  image.dir = file.path(project_dir, "reports/images")
  if (dir.exists(image.dir)) {
    dest.image.dir = file.path(dest.dir,"images")
    try(dir.create(dest.image.dir, showWarnings = FALSE))
    copy.dir(image.dir, dest.image.dir)
  }

  dest.dir
  #rstudioapi::filesPaneNavigate(dest.dir)
}


remove.dir = function(dir.to.be.removed, recursive=TRUE, must.contain = "/projects/") {
  if (!has.substr(dir.to.be.removed,must.contain)) {
    stop(paste0("Sorry, for security reasons currently only directories that contain the path ", must.contain, " can be removed."))
  }
  if (!dir.exists(dir.to.be.removed)) return()
  unlink(dir.to.be.removed,recursive = recursive)
}




