example = function() {
  ejd_make_zip_df()
}


# Collects information from all downloaded ZIP files of supplements
# and stores it in explore directory
ejd_make_zip_df = function() {
  zip_dirs = c("~/ejd_files/sup_zip_oi", "~/ejd_files/sup_zip_ms", "~/ejd_files/sup_zip_dv","~/ejd_files/sup_zip")
  zip_files = list.files(zip_dirs,full.names = TRUE, recursive = TRUE)

  zip_df = tibble(
    artid = NA_character_,
    zip_id = tools::file_path_sans_ext(basename(zip_files)),
    file = zip_files,
    dir = dirname(zip_files),
    mb = file.size(zip_files) / 1e6,
    from_oi = endsWith(dir, "_oi"),
    from_ms = endsWith(dir, "_ms"),
    from_direct = endsWith(dir, "downloads"),
    num_zips = 1
  )

  # All EJD articles
  library(EconJournalScrap)
  #init.journal.scrapper()
  db = get.articles.db(db.dir = "~")
  arts = dbGet(db, "article")

  rows = zip_df$zip_id %in% arts$id
  zip_df$artid[rows] = zip_df$zip_id[rows]

  rows = which(has.substr(zip_df$zip_id, "issue_"))
  ids = zip_df$zip_id[rows]
  head(ids)
  ids = ids %>%
    stringi::stri_replace_first_fixed("_vol_","_") %>%
    stringi::stri_replace_first_fixed("_issue_","_") %>%
    stringi::stri_replace_first_fixed("_article_","_")
  head(ids)
  zip_df$artid[rows] = ids


  artid_rx = "^[a-z]+_[0-9]+_[0-9]+_[0-9]+"
  multi_rx = paste0(artid_rx, "__[0-9]+$")
  zip_df$is_multi_zip = stringi::stri_detect_regex(zip_df$zip_id, multi_rx)
  mu_df = zip_df %>%
    filter(is_multi_zip) %>%
    mutate(artid = str.left.of(zip_id,"__")) %>%
    group_by(artid) %>%
    mutate(num_zips = n(), file = paste0(file, collapse=";"), mb=sum(mb)) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(is_multi_zip = num_zips > 1)

  zip_df = bind_rows(filter(zip_df, !is_multi_zip), mu_df)

  # Try to match zips from oi
  oi_arts = arts %>%
    filter(repo=="oi")
  str = oi_arts$data_url
  str = str.right.of(str,"/project/",not.found = rep(NA_character_, length(str)))
  str = str.left.of(str,"/version/",not.found = rep(NA_character_, length(str)))
  oi_arts$oi_id = str

  # oi zip ids have a form like
  # 109644-V1
  temp_id = str.left.of(zip_df$zip_id,"-")
  mr = match(temp_id,oi_arts$oi_id)
  rows = which(!is.na(mr))
  zip_df$artid[rows] = oi_arts$id[mr[rows]]

  # Try to match downloads from MS
  ms_arts = arts %>%
    filter(journ=="ms") %>%
    mutate(ms_id = str.right.of(data_url,"?doi=",not.found = NA_character_))
  mr = match(zip_df$zip_id,ms_arts$ms_id)
  rows = which(!is.na(mr))
  zip_df$artid[rows] = ms_arts$id[mr[rows]]

  sum(!is.na(zip_df$artid))

  dv_arts = arts %>%
    filter(repo=="dv" | has.substr(data_url, "/DVN/")) %>%
    mutate(dv_id = str.right.of(data_url, "/DVN/"))
  mr = match(zip_df$zip_id,dv_arts$dv_id)
  rows = which(!is.na(mr))
  zip_df$artid[rows] = dv_arts$id[mr[rows]]

  repboxExplore::save_explore_rds(zip_df, "ejd_zip_df.Rds")
  invisible(zip_df)


}
