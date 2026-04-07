rb_update_docs = function(rb, overwrite=FALSE,init_docs=TRUE, process_docs=TRUE) {
  restore.point("rb_update_docs")
  library(repboxDoc)
  project_dir = rb$project_dir

  rb$has_docs = rb_has_docs(project_dir)
  if (!overwrite & rb$has_docs) {
    rb$are_all_docs_processed = rb_are_all_docs_processed(project_dir)
    if (!process_docs | rb$are_all_docs_processed) {
      return(rb)
    }
  }

  if (init_docs & require(repboxEJD,quietly = TRUE)) {
    #library(repboxEJD)
    repboxEJD::repbox_ejd_init_doc(project_dir = project_dir, overwrite=overwrite)
  }

  if (process_docs) {
    library(repboxDoc)
    repboxDoc::repbox_process_all_docs(project_dir, overwrite=overwrite)
  }
  rb$are_all_docs_processed = rb_are_all_docs_processed(project_dir)
  rb
}

rb_are_all_docs_processed = function(project_dir) {
  library(repboxDoc)
  doc_dirs  =repboxDoc::repbox_doc_dirs(project_dir)
  is_processed = sapply(doc_dirs, rdoc_is_processed)
  all(is.true(is_processed))
}

rb_has_docs = function(project_dir) {
  library(repboxDoc)
  doc_dirs  =repboxDoc::repbox_doc_dirs(project_dir)
  length(doc_dirs)>1
}
