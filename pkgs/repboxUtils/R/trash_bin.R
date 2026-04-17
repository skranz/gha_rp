# Call trash-restore in linux shell to restore trash

example = function() {
  trash_file = c("~/repbox/test_tab.html","~/repbox/test_table.html")
  trash_file = "~/repbox/temp/to_trash"

}

dir_to_trash = function(trashed_dir) {
  trash_put(trashed_dir)
}

trash_put = function(trashed_file) {
  trash_file = normalizePath(trashed_file)
  cmd = paste0('trash-put ', paste0('"', trashed_file,'"', collapse=" "))
  cat(cmd)
  system(cmd)
}

trash_list = function(dir = "~") {
  cmd = paste0('trash-list')
  system(cmd)
}

