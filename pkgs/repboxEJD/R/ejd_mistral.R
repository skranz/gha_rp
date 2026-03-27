# Copy mistral ocr file to doc/art_mocr/ocr.Rds

example = function() {
  library(restorepoint)
  project_dirs = list.dirs("~/repbox/projects_share", recursive = FALSE)
  project_dir = project_dirs[1]
  for (project_dir in project_dirs) {
    repbox_ejd_copy_mistral_ocr(project_dir,overwrite = TRUE)
    correct_no_pdf_mistral_ocr(project_dir)
  }
  rstudioapi::filesPaneNavigate(project_dir)

  # del_files = list.files("~/repbox/projects_share", glob2rx("ocr.Rds"),full.names = TRUE, recursive = TRUE)
  # file.remove(del_files)
  # dirs =  list.dirs("~/repbox/projects_share", recursive = TRUE)
  # mocr_dirs = dirs[has.substr(dirs, "/doc/art_mocr")]
  # del_dirs = union(mocr_dirs, dirname(mocr_dirs))
  # file.remove(del_dirs)
  # any(dir.exists(del_dirs))
  # del_dir = list.files("~/repbox/projects_share", glob2rx("doc/ar"),full.names = TRUE, recursive = TRUE)
}

correct_no_pdf_mistral_ocr = function(project_dir) {
  mocr_dir = file.path(project_dir, "doc","art_mocr")
  do_correct = dir.exists(mocr_dir) & !dir.exists(file.path(project_dir, "doc","art_pdf"))
  if (!do_correct) return()
  files = list.files(mocr_dir,glob2rx("*.*"), full.names = TRUE)
  file.remove(files)
  file.remove(mocr_dir)
}

repbox_ejd_copy_mistral_ocr = function(project_dir, source_dir = "~/ejd_files/mocr_art", overwrite=FALSE, need_art_pdf = TRUE) {
  restore.point("repbox_ejd_copy_mistral_ocr")
  artid = basename(project_dir)
  source_file = file.path(source_dir, paste0(artid, ".Rds"))
  if (!file.exists(source_file)) return(NULL)
  dest_dir = file.path(project_dir, "doc", "art_mocr")
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
  dest_file = file.path(dest_dir, "ocr.Rds")
  if (file.exists(dest_file) & !overwrite) return(NULL)
  if (need_art_pdf) {
    if (!dir.exists(file.path(project_dir, "doc", "art_pdf")))
      return(NULL)
  }
  cat("\nCopy to ", dest_file)
  file.copy(source_file, dest_file,overwrite = overwrite)
}
