rb_update_meta = function(rb, overwrite=FALSE, opts=rb_options()) {
  restore.point("rb_update_meta")
  if (!require(repboxEJD)) {
    cat("\nNo repboxEJD package, don't update meta information.")
    return(rb)
  }
  library(repboxEJD)
  if (!rb_shall_perform_step(rb,"meta")) return(rb)

  rb$has_meta = rb_has_meta(rb$project_dir)
  if (!overwrite & rb$has_meta) return(rb)

  project_dir = rb$project_dir
  artid = basename(project_dir)

  art = rb_load_ejd_art(artid, rb=rb)
  rb$art = art

  # Currently we can only implement from repboxEJD
  #library(repboxEJD)
  meta = repboxEJD::ejd_art_to_meta(art)
  repboxEJD::save_art_sup_meta(meta=meta, project_dir=project_dir)

  rb$meta = meta
  rb$has_meta = rb_has_meta(rb$project_dir)
  rb
}

rb_has_meta = function(project_dir) {
  meta_dir = file.path(project_dir, "meta")

  file.exists(file.path(meta_dir, "art_meta.Rds")) &
    file.exists(file.path(meta_dir, "sup_meta.Rds"))
}

rb_load_meta = function(project_dir=rb$project_dir, rb=NULL) {
  if (!is.null(rb$meta)) return(rb$meta)
  meta_dir = file.path(project_dir, "meta")

  meta = list(
    art = read_rds_or_null(file.path(meta_dir, "art_meta.Rds")),
    sup = read_rds_or_null(file.path(meta_dir, "sup_meta.Rds"))
  )
  meta
}

rb_load_ejd_art = function(artid,rb=NULL) {
  if (!is.null(rb[["art"]])) return(rb$art)
  arts = EconJournalScrap::ejs_load_agg("art.Rds")
  arts[arts$artid==artid, ]
}
