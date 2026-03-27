download_zenodo_zip <- function(record_id, destdir) {

  #url = "https://doi.org/10.5281/zenodo.6210284"
  #record_id = str.right.of(url,"zenodo.")


  qry <- paste0("https://zenodo.org/api/records/", record_id)

  res  <- httr::GET(qry)
  record  <- jsonlite::fromJSON(httr::content(res, as = "text", encoding="UTF-8"))

  url <- record$files$links$download
  if (length(url)==0) {
    cat("\nWARNING: Zenodo repo contains no file.")
    return(NULL)
  }

  if (length(url)>1 | !all(tolower(tools::file_ext(url))=="zip")) {
    cat("\nWARNING: Zenodo repo contains more than one file. Currently we assume that there is just a single zip file.")
    #invisible(lapply(url, function(x)
      #download.file(x, norm_path(dest_folder, basename(x)))))
    return(NULL)
  }
  destfile = file.path(destdir, basename(url))
  download.file(url, destfile)

}
