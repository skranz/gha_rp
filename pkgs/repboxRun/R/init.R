#' Call this function to initialize a new repbox project
#'
#' It will generate the new project directory and fill it with the
#' neccessary files to call repbox_run_project.
#'
#' @param project_dir The directory of the new project
#' @param sup_zip The ZIP file of the supplement
#' @param pdf_files The PDF file(s) of the article. Currently only a single PDF file works but in the future also support for multiple PDF files, e.g. article plus online appendix, will be added.
#' @param html_files Alternatively, the HTML file(s) of the article.
#' @param remove_macosx_dirs If TRUE, the "__MACOSX" directories will be removed from the supplement.
#' @param just_extract_code If TRUE just the R and Stata code files of the supplement will be extracted. This can (only) make sense if you just want to perform static analysis of code (and the article). Then you can save space. Complete repbox analysis won't be feasible however.
#' @export
repbox_init_project = function(project_dir, sup_zip=NULL, pdf_files=NULL, html_files = NULL, remove_macosx_dirs=TRUE, overwrite_org = FALSE, just_extract_code = FALSE) {
  restore.point("repbox_init_project")

  project = basename(project_dir)
  org.dir = file.path(project_dir,"org")
  repbox.dir = file.path(project_dir, "repbox")

  if (!dir.exists(project_dir)) {
    dir.create(project_dir)
  }

  if (!dir.exists(file.path(project_dir,"meta"))) {
    dir.create(file.path(project_dir,"meta"))
  }

  # Set working directory.
  # Otherwise if working directory will be deleted we can get later an error:
  # sh: 0: getcwd() failed
  setwd(project_dir)

  # Copy supplement ZIP content into org.dir
  if (!is.null(sup_zip) & (overwrite_org | !(dir.exists(org.dir)))) {
    # Some reproduction packages consist of multiple zip
    # files
    for (zip_file in sup_zip) {
      repbox_sup_extract_zip(project_dir, zip_file, just_extract_code, remove_macosx_dirs=remove_macosx_dirs)
    }
  }

  if (!dir.exists(org.dir)) {
    stop("You neither provided a working ZIP file nor had your project has no existing directory with the original data and code supplement.")
  }

  # AEA supplements often are dupplicated
  # in a separate "__MACOSX" directory
  # We will remove those by default
  if (remove_macosx_dirs) {
    remove_macosx_dirs(org.dir)
  }

  # We get an error if current working directory does not exist
  repbox_copy_art_pdf(project_dir, pdf_files)
  repbox_copy_art_html(project_dir, html_files)
}

repbox_has_just_extracted_code = function(project_dir) {
  just_extract_code_file = file.path(project_dir, "steps", "NOTE_JUST_EXRACTED_CODE")
  file.exists(just_extract_code_file)
}

repbox_sup_extract_zip = function(project_dir, sup_zip, just_extract_code, remove_macosx_dirs=TRUE) {
  restore.point("repbox_sup_just_extract_code")

  org.dir = file.path(project_dir,"org")
  repbox.dir = file.path(project_dir, "repbox")
  steps.dir = file.path(project_dir, "steps")
  just_extract_code_file = file.path(steps.dir, "NOTE_JUST_EXRACTED_CODE")

  # 1. Get all files in ZIP
  file_df = unzip(sup_zip, list = TRUE)

  names(file_df) = c("file_path","mb","timestamp")
  file_df$mb = file_df$mb / 1e6
  file_df$file_type = tolower(tools::file_ext(file_df$file_path))

  if (remove_macosx_dirs) {
    ignore = has.substr(file_df$file_path, "__MACOSX")
    file_df = file_df[!ignore,]
  }

  meta_dir = file.path(project_dir, "meta")
  if (!dir.exists(meta_dir)) dir.create(meta_dir)
  saveRDS(file_df, file.path(meta_dir, "sup_files.Rds"))


  if (!just_extract_code) {
    robust_unzip(sup_zip, exdir = org.dir)
    if (file.exists(just_extract_code_file)) file.remove(just_extract_code_file)
    note_problem_if_no_sub_files(project_dir, file_df = file_df)
    return(TRUE)
  }
  # 2. Unzip R and stata scripts files
  code_files = file_df$file_path[file_df$file_type %in% c("r","do","rmd","ado")]
  if (length(code_files)>0) {
    # internal unzip more often claims that ZIP is
    # corrupted while command line unzip works
    #unzip(sup_zip,files = code_files,exdir = org.dir, unzip="unzip")
    robust_unzip(normalizePath(sup_zip), exdir=normalizePath(org.dir), files=code_files)
    note_problem_if_no_sub_files(project_dir, file_df = file_df)
  } else {
    dir.create(org.dir)
  }

  # 3. Store note that just code has been extracted
  if (!dir.exists(steps.dir)) dir.create(steps.dir, recursive = TRUE)
  writeLines("yes", just_extract_code_file)

  return(TRUE)

}


repbox_copy_art_pdf = function(project_dir, pdf_files = NULL) {
  restore.point("copy.repbox.art")
  pdf.dir = file.path(project_dir, "art", "pdf")
  if (!is.null(pdf_files)) {
    clear.and.create.dir(pdf.dir)
    file.copy(pdf_files, pdf.dir,recursive = TRUE)
  }
}

repbox_copy_art_html = function(project_dir, html_files = NULL) {
  restore.point("repbox_copy_art_html")
  html.dir = file.path(project_dir, "art", "html")
  if (!is.null(html_files)) {
    clear.and.create.dir(html.dir)
    file.copy(html_files, html.dir,recursive = TRUE)
  }
}


remove_macosx_dirs = function(parent.dir) {
  dirs = list.dirs(parent.dir)
  mac.dirs = dirs[has.substr(dirs, "__MACOSX")]
  for (mac.dir in mac.dirs) {
    remove.dir(mac.dir,recursive = TRUE)
  }
}

# R's internal unzip command too often thinks the ZIP file of a replication package
# is corrupted even though linux command line unzip is able to unzip it.
robust_unzip = function(zipfile, exdir, files=NULL, overwrite=TRUE, verbose=FALSE) {
  restore.point("robust_unzip")
  if (overwrite) {
    cmd = paste0("unzip -o ")
  } else {
    cmd = paste0("unzip -n ")
  }

  if (is.null(files)) {
    cmd = paste0(cmd, '"', normalizePath(zipfile), '" -d "', normalizePath(exdir,mustWork = FALSE),'"')
  } else {
    cmd = paste0(cmd,'"', normalizePath(zipfile), '" ', paste0('"',files,'"', collapse=" "), ' -d "', normalizePath(exdir,mustWork = FALSE),'"')

  }
  cat(cmd)
  system(cmd,ignore.stdout = !verbose)
  #rstudioapi::filesPaneNavigate(exdir)
}


note_problem_if_no_sub_files = function(project_dir, sup_dir=paste0(project_dir, "/org"),file_df) {
  restore.point("note_problem_if_no_sub_files")
  if (!missing(file_df)) {
    if (NROW(file_df)==0) return()
  }
  files = list.files(sup_dir)
  if (length(files)==0) {
    repboxUtils::repbox_problem(type="zip_no_files", msg="We could not extract any files from the ZIP of the reproduction package. Possibly corrupted.",fail_action = "msg",project_dir = project_dir)
  }
}


