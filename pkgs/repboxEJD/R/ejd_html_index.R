# TO DO: Copy directories

example = function() {
  add_new_ejd_project_infos(parent_dir = c("~/repbox/gha_ejd/projects_ejd_gha2","~/repbox/projects_gha","~/repbox/projects_gha_new","~/repbox/projects_gha_old2", "~/repbox/gha_ejd/projects_ejd_gha"))
  make_ejd_repbox_html_links_df()
  make_ejd_www_index_page()

}


ejd_copy_repbox_report = function() {

}




make_ejd_www_index_page = function(info.file = "~/ejd_files/agg/repbox_ejd_infos.Rds", www_dir = "~/web/ejd/repbox", project_dirs = NULL) {
  restore.point("make_ejd_www_index_page")

  library(EconJournalScrap)
  info_df = readRDS(info.file)
  arts = ejs_load_agg("art")

  rem_cols = setdiff(intersect(names(info_df), names(arts)),"artid")
  arts = remove_col(arts, rem_cols)
  info_df = left_join(info_df, arts, by="artid")

  upper_dirs = list.dirs(www_dir, full.names = TRUE, recursive=FALSE)
  www.projects = list.dirs(upper_dirs, full.names=FALSE, recursive = FALSE) %>% setdiff("shared")

  if (!is.null(project_dirs)) {
    projects = basename(project_dirs)
    www.projects = intersect(www.projects, projects)
  }

  info_df = info_df %>%
    filter( artid %in% www.projects) %>%
    mutate(
      num_runs_no_dat = na_val(num_runs_no_dat,0),
      num_runs_ok = coalesce(num_runs_ok, num_runs-num_runs_err-num_runs_no_dat),
      share_runs_ok = ifelse(num_runs > 0, num_runs_ok / num_runs, 0),
      mb = coalesce(mb_zip, mb, mb_unzip)
    ) %>%
    filter(is.true(num_runs>0)) %>%
    mutate(
      has_all_data = num_load_data_err==0,
      data_missing = num_load_data_err
    ) %>%
    arrange(desc(share_runs_ok), desc(has_all_data)) %>%
    mutate(html = paste0(
      '<p .ejd_link>',1:n(),". ",round(share_runs_ok*100),'%, ', data_missing, ' missing data loads, ',round(mb,2)," MB, ", num_runs,' runs, ', ceiling(runtime), ' seconds ', ifelse(timeout > 0, " TIMEOUT!",""),
      '<a href="',journ,'/',artid,'" target="_blank">',title,' (',journ,",",year,')</a>',
      '</p>'
    ))
  style = "<style> .ejd_link {font-family: Helvetia; padding: 5px;}</style>"


  html = paste0("<body>",style,"\n<ul class='ejd_link'>",
                "<h4>Repbox-EJD Webpages</h4>",
                paste0(info_df$html, collapse="\n"),
                "</ul></body>"
  )
  writeLines(html, file.path(www_dir,"index.html"))
}



make_ejd_repbox_html_links_df = function(info.file = "~/ejd_files/agg/repbox_ejd_infos.Rds", url.base = "https://econ.mathematik.uni-ulm.de/ejd/repbox", out.dir="~/ejd_files/agg", www_dir="~/web/ejd/repbox") {
  restore.point("make_ejd_repbox_html_links_df")
  library(EconJournalScrap)
  info_df = readRDS(info.file)
  names(info_df)

  test = info_df %>%
    transmute(
      num_runs_no_dat = na_val(num_runs_no_dat,0),
      num_runs_ok = coalesce(num_runs_ok, num_runs-num_runs_err-num_runs_no_dat),
      num_runs=num_runs,
      sum_runs = num_runs_err+num_runs_ok+num_runs_no_dat
    )

  html_df = info_df %>%
    filter(num_runs >0) %>%
    mutate(journ = EconJournalScrap::ejs_artid_to_journ(artid)) %>%
    mutate(
      num_runs_no_dat = na_val(num_runs_no_dat,0),
      num_runs_ok = coalesce(num_runs_ok, num_runs-num_runs_err-num_runs_no_dat),
      share_runs_ok = ifelse(num_runs > 0, num_runs_ok / num_runs, 0)
    ) %>%
    mutate(
      repbox.html = paste0('(<a href="', url.base,'/', journ,'/',artid,'" target="_blank">Stata reproduction ', round(share_runs_ok*100),"%</a>)"),
      stata.perc = share_runs_ok
    ) %>%
    mutate(
      report_file = file.path(www_dir,journ, artid, "index.html"),
      html_exists = file.exists(report_file)
    ) %>%
    select(artid, repbox.html, stata.perc, report_file, html_exists)

  which(is.na(html_df$artid))
  sum(html_df$html_exists)
  sum(!html_df$html_exists)

  html_df = html_df %>%
    filter(html_exists) %>%
    select(artid, repbox.html, stata.perc)

  if (!is.null(out.dir)) {
    saveRDS(html_df, file.path(out.dir,"ejd_repbox_links.Rds"))
  }
  invisible(html_df)
}
