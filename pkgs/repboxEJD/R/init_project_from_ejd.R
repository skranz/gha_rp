example = function() {
  library(repboxEJD)
  artid = "aejapp_1_2_7"
  repbox_init_ejd_project(artid=artid, projects.dir = "~/repbox/projects_test")
  project_dir = "/home/rstudio/repbox/projects_gha_new/aejapp_1_2_4"
  repbox_ejd_init_readme(project_dir)
}


get_ejd_project_art_and_sup_files = function(artid=NULL,art=NULL,...) {
  res = repbox_init_ejd_project(artid=artid,art=art, ..., just_get_art_sup_files=TRUE)
}

get_ejd_art = function(artid, art=NULL, arts=NULL) {
  library(EconJournalScrap)
  if (is.null(arts) & is.null(art)) {
      arts = ejs_load_agg("art")
  }
  if (is.null(art)) {
    art = arts[arts$artid==artid,]
  }
  art = as.list(art)
  art
}

repbox_init_ejd_project = function(art=NULL,artid=NULL, projects.dir = "~/repbox/projects_reg", ejd.db.dir = "~", overwrite_org=TRUE,just_extract_code=FALSE, just_copy_sup=FALSE, just_get_art_sup_files=FALSE,...) {
  library(EconJournalScrap)
  library(repboxRun)
  if (is.null(art) & !is.null(artid)) {
    arts = ejs_load_agg("art")
    art = arts[arts$artid==artid,]
  } else if (is.null(art)) {
    stop("Please provide artid or art.")
  }
  restore.point("repbox_init_ejd_project")
  if (is.na(art$file_zip)) {
    cat("\n No supplement ZIP file downloaded for", art$id, ".\n")
    return(FALSE)
  }

  if (!file.exists(art$file_zip)) {
    cat("\n", art$id, " ZIP file ", art$file_zip, " with supplement does not exist.\n")
    return(FALSE)
  }

  art = as.list(art)
  artid = art$id
  project_dir = paste0(projects.dir,"/",art$id)

  if (just_copy_sup) {
    repbox_sup_extract_zip(project_dir, art$file_zip,just_extract_code = FALSE)
    remove_macosx_dirs(file.path(project_dir,"org"))
    return(TRUE)
  }

  art$pdf_file = art$file_articles_pdf
  art$html_file = art$file_articles_html
  art$has.pdf = file.exists(art$pdf_file)
  if (!art$has.pdf) art$pdf_file = NULL
  art$has.html = file.exists(art$html_file)
  if (!art$has.html) art$html_file = NULL

  if (just_get_art_sup_files) {
    return(list(sup_file = art$file_zip, art_file = if(art$has.html) art$html_file else art$pdf_file, pdf_file=art$pdf_file, html_file=art$html_file))
  }

  cat("\n Init ", project_dir, "\n")
  repbox_init_project(project_dir = project_dir, sup_zip = art$file_zip,pdf_files = art$pdf_file, html_file=art$html_file, overwrite_org = overwrite_org, just_extract_code=just_extract_code)

  meta = ejd_art_to_meta(as_tibble(art))
  cat(paste0('\nrstudioapi::filesPaneNavigate("',project_dir,'")\n'))
  save_art_sup_meta(meta, project_dir)

  # create doc (articles and appendix)
  repbox_ejd_init_doc(project_dir)
  repbox_ejd_init_readme(project_dir)



  repboxReadme::find_readme_cand_in_zip()


}


repbox_ejd_init_readme = function(project_dir, art=NULL, overwrite=FALSE) {
  restore.point("repbox_ejd_init_readme")

  if (!overwrite) {
    if (dir.exists(file.path(project_dir,"readme"))) return()
  }


  # copy readme
  artid = basename(project_dir)
  library(EconJournalScrap)
  copy.ejs.file = function(ejs_type,ejs_ext=NULL, dest_path, dest_base=NULL) {
    restore.point("khdkshfk")
    source_files = ejs_list_files(ejs_type, ejs_ext, artid=artid)
    if (length(source_files)==0) {
      cat("\n", artid, " has no ", ejs_type, " files\n")
      return(invisible())
    }
    dest_dir = file.path(project_dir, dest_path)
    if (!dir.exists(dest_dir)) dir.create(dest_dir,recursive = TRUE)
    if (is.null(dest_base)) dest_base = basename(source_files)
    dest_files = file.path(dest_dir, dest_base)
    file.copy(source_files, dest_files)
    cat("\nCreated ", paste0(dest_files, collapse="\n"),"\n")
  }
  copy.ejs.file("readme_files_icpsr", NULL, "readme/org")
  copy.ejs.file("readme", NULL, "readme/org")

  readme_org_dir = file.path(project_dir, "readme/org")
  if (!dir.exists(readme_org_dir)) {
    art = get_ejd_art(artid)
    if (!is_empty(art$file_zip)) {
      if (file.exists(art$file_zip)) {
        cat("\nTry to extract readme files from ", art$file_zip)
        repboxReadme::repbox_extract_readme_cand_from_zip(project_dir, zip_file= art$file_zip)
      }
    }
  }
  library(repboxReadme)
  repboxReadme::repbox_readme_files_to_txt(project_dir)
}

repbox_ejd_init_doc = function(project_dir, overwrite=FALSE) {
  library(repboxDoc)
  library(EconJournalScrap)
  artid = basename(project_dir)

  #if (!overwrite) {
  #  if (dir.exists(file.path(project_dir,"doc"))) return()
  #}

  copy.doc.file = function(ejs_type, ejs_ext, doc_path, dest_base=NULL) {
    restore.point("khdkshfk")
    source_files = ejs_list_files(ejs_type, ejs_ext, artid=artid)
    if (length(source_files)==0) {
      cat("\n", artid, " has no ", ejs_type, " files for ", doc_path,"\n")
      return(invisible())
    }
    dest_dir = file.path(project_dir, "doc", doc_path)
    if (!dir.exists(dest_dir)) dir.create(dest_dir,recursive = TRUE)
    if (is.null(dest_base)) dest_base = basename(source_files)
    dest_files = file.path(dest_dir, dest_base)
    file.copy(source_files, dest_files, overwrite=overwrite)
    cat("\nCreated ", paste0(dest_files, collapse="\n"),"\n")
  }
  copy.doc.file("articles_html", "html", "art_html/html")
  copy.doc.file("articles_pdf", "pdf", "art_pdf/pdf")
  copy.doc.file("art_mocr", "Rds", "art_mocr", "ocr.Rds")

  # Copy appendices
  source_files = sort(c(
    ejs_list_files("appendix", "pdf", artid=artid),
    ejs_list_files("appendix_from_zip", "pdf", artid=artid)
  ))
  if (length(source_files)>0) {
    source_files = source_files[!duplicated(basename(source_files))]
    for (i in seq_along(source_files)) {
      dest_dir = file.path(project_dir, "doc", paste0("app",i,"_pdf/pdf"))
      if (!dir.exists(dest_dir)) dir.create(dest_dir,recursive = TRUE)
      dest_file = file.path(dest_dir, basename(source_files[i]))
      file.copy(source_files[i], dest_file, overwrite = overwrite)
      cat("\nCreated ", paste0(dest_file, collapse="\n"),"\n")
    }
  }

  source_files = sort(c(
    ejs_list_files("app_mocr", "Rds", artid=artid)
  ))
  if (length(source_files)>0) {
    source_files = source_files[!duplicated(basename(source_files))]
    for (i in seq_along(source_files)) {
      dest_dir = file.path(project_dir, "doc", paste0("app",i,"_mocr"))
      if (!dir.exists(dest_dir)) dir.create(dest_dir,recursive = TRUE)
      dest_file = file.path(dest_dir, "ocr.Rds")
      file.copy(source_files[i], dest_file, overwrite = overwrite)
      cat("\nCreated ", paste0(dest_file, collapse="\n"),"\n")
    }
  }



  #rstudioapi::filesPaneNavigate(project_dir)

}
