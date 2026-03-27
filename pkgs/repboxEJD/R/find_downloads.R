example = function() {
  # Get articles
  library(EconJournalData)
  setwd("~")
  db = get.articles.db()
  arts = dbGet(db,"article",params = list(repo="oi"))
  arts = arts %>% filter(date >= "2023-06-01")

  ejd_find_downloaded_sup_zip(arts)

  res = ejd_list_downloaded_sup_zip()
}

ejd_list_downloaded_sup_zip = function(dirs = c(oi.download.dir="~/ejd_files/sup_zip_oi", ms.download.dir = "~/ejd_files/sup_zip_ms",  dv.download.dir = "~/ejd_files/sup_zip_dv", zip.download.dir = "~/ejd_files/sup_zip")) {
  files = list.files(dirs, glob2rx("*.zip"),ignore.case = TRUE, full.names = TRUE)

  df = data.frame(
    artid = tools::file_path_sans_ext(basename(files)),
    file = files
  )
  df = df[!duplicated(df$artid),,drop=FALSE]
  df
}


ejd_find_downloaded_sup_zip = function(arts=NULL, ..., zip_df=NULL, artid=arts$id) {
  restore.point("repbox_find_downloaded_sup_zip")
  if (is.null(zip_df))
    zip_df = repboxExplore::read_explore_rds("ejd_zip_df.Rds")

  zip_df$file[match(artid, zip_df$artid)]
}

