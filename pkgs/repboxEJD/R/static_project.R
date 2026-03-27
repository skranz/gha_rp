example = function() {
  library(repboxExplore)
  library(repboxEJD)
  id_li = read_explore_rds("artid_sets.Rds")
  id_li$zips_with_do = intersect(id_li$ejd_with_do, id_li$zips)
  missing_static = setdiff(id_li$zips_with_do, id_li$all_dir)
  artid = missing_static[1]
  repbox_init_ejd_static_project(artid)
}



repbox_init_ejd_static_project = function(artid, zip_file=NULL, projects.dir = "~/repbox/projects_static", overwrite_org=FALSE, zip_df=NULL, arts=NULL,ejd.db.dir = "~") {
  restore.point("repbox_init_ejd_static_project")
  library(EconJournalScrap)
  library(repboxRun)

  art = get_ejd_art_from_artid(artid, arts=arts)
  if (is.null(zip_df))
    zip_df = repboxExplore::read_explore_rds("ejd_zip_df.Rds")

  zip = zip_df[is.true(zip_df$artid == artid),]
  if (NROW(zip)==0) {
    cat("\n No supplement ZIP file downloaded for", artid, ".\n")
    return(FALSE)
  }

  art = as.list(art)
  project_dir = paste0(projects.dir,"/",art$id)
  if (!overwrite_org) {
    if (dir.exists(project_dir)) {
      cat("\nProject directory", project_dir, "already exists.")
    }
  }

  art$pdf_file = file.path("~/ejd_files/articles_pdf",art$journ,paste0(art$id,".pdf"))
  art$html_file = file.path("~/ejd_files/articles_html",art$journ,paste0(art$id,".html"))
  art$has.pdf = file.exists(art$pdf_file)
  if (!art$has.pdf) art$pdf_file = NULL
  art$has.html = file.exists(art$html_file)
  if (!art$has.html) art$html_file = NULL

  #stop()
  cat("\n Init static ", project_dir, "\n")
  zip_files = strsplit(zip$file,";")[[1]]
  repbox_init_project(project_dir = project_dir, sup_zip = zip_files,pdf_files = art$pdf_file, html_file=art$html_file, overwrite_org = TRUE, just_extract_code=TRUE)

  #rstudioapi::filesPaneNavigate(project_dir)
  db = get.articles.db(ejd.db.dir)

  meta = ejd_art_to_meta(as_tibble(art),db)
  save_art_sup_meta(meta, project_dir)
}

get_ejd_art_from_artid = function(artid, arts=NULL, ejd.db.dir = "~") {
  if (!is.null(arts)) {
    art = arts[arts$id==artid,]
  } else {
    db = get.articles.db(ejd.db.dir)
    art = dbGet(db, "article",list(id=artid))
  }
  art
}
