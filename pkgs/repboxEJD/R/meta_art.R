update_all_ejd_art_sup_basic_info = function() {
  library(repboxEJD)
  library(repboxArt)
  library(repboxRun)
  project_dirs = list.dirs("~/repbox/projects_gha", recursive = FALSE)
  db = get.articles.db(db.dir = "~")

  arts = dbGet(db, "article")
  artids = basename(project_dirs)

  i = 1
  for (i in seq_along(artids)) {
    cat("\n",i," of ", length(artids), " ", artid,"\n")
    artid = artids[i]
    art = arts[arts$id == artid,]
    try({
      meta = ejd_art_to_meta(as_tibble(art),db)
      project_dir = project_dirs[i]
      save_art_sup_meta(meta, project_dir)
      try(repboxArt::art_save_basic_info(project_dir))
      try(repboxRun::sup_save_basic_info(project_dir))
    })
  }
  rstudioapi::filesPaneNavigate(project_dir)
}


ejd_art_to_meta = function(art) {
  restore.point("ejd_art_to_art_meta")
  art = art %>%
    mutate(
      artid = id,
      appendix_pdf_url = NA,
      pdf_url = NA,
      html_url = NA,
      article_doi = NA
    )

  sup = art %>%
    transmute(
      artid = id,
      sup_url = data_url,
      sup_doi = NA,
      sup_repo = repo
    )

  if (art$journ %in% c("aer","jep","aejmac","aejapp","aejmic","aejpol","pandp")) {
    #https://www.aeaweb.org/articles?id=10.1257/app.3.2.34
    #https://www.aeaweb.org/articles/pdf/doi/10.1257/app.3.2.34
    art$pdf_url = stri_replace_first_fixed(art$article_url, "/articles?id=","/articles/pdf/doi/")
    # 10.1257/app.3.2.34
    art$article_doi = str.right.of(art$article_url, "/articles?id=")
    if (is.na(art$repo)) {
      # Old data URL don't work anymore
      sup$sup_url = NA
    }
  }
  if (isTRUE(art$repo == "oi")) {
    # Don't see a fixed rule to map data DOI with data URL
    # I would need to better parse the OpenICPSR page to extract the DOI
    sup$sup_doi = NA
  } else if (isTRUE(art$repo == "dv")) {
    # https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/YZ46FJ
    # https://doi.org/10.7910/DVN/ZWTW0M
    if (has.substr(art$data_url,"doi.org/")) {
      sup$sup_doi = str.right.of(art$data_url,"doi.org/")
    } else if (has.substr(art$data_url,"=doi:")) {
      sup$sup_doi = str.right.of(art$data_url,"=doi:")
    } else {
      stop("The article with repo = 'dv' has an unknown URL format, for which we cannot yet extract the DOI.")
    }
  } else if (isTRUE(art$repo == "ze")) {
    if (has.substr(art$data_url,"doi.org/")) {
      sup$sup_doi = str.right.of(art$data_url,"doi.org/")
    } else {
      stop("The article with repo = 'ze' has an unknown URL format, for which we cannot yet extract the DOI.")
    }
  } else if (!is.empty(art$repo)) {
    stop(paste0("We should add code that generates the supplement doi based on the URL for the repo ", art$repo))
  }

  spec = repdb_get_spec("art")
  cols = spec$fields$field

  art = art[,intersect(cols,names(art))]

  missing_cols = setdiff(cols, names(art))
  missing_cols

  list(art=art, sup=sup)
}

save_art_sup_meta = function(meta, project_dir) {
  meta_dir = file.path(project_dir, "meta")
  if (!dir.exists(meta_dir)) dir.create(meta_dir)
  saveRDS(meta$art, file.path(meta_dir, "art_meta.Rds"))
  saveRDS(meta$sup, file.path(meta_dir, "sup_meta.Rds"))
}
