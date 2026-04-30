# To install 7zip on Linux run
# sudo apt-get install -y p7zip-full

example = function() {
  setwd("~")
  to.7z("~/temp", zip="~/test.7z", password="MyPassword")
  extract.7z("~/temp2", zip="~/test.7z", password="MyPassword")

  setwd("~/repbox")
  to.7z("repboxMain",zip = "~/repbox/repboxMain.7z", password = PKG_KEY)
  to.7z("repboxStata",zip = "~/repbox/repboxStata.7z", password = PKG_KEY)

}

to.7z = function(path, zip=paste0(basename(path),".7z"), password = NULL) {
  restore.point("to.7z")
  switch = ""
  if (!is.null(password)) {
    switch = paste0(" -p",password," ")
  }
  cmd = paste0('7z a ', zip, ' ', path,' ',  switch)
  cat(cmd)
  system(cmd)
}

extract.7z = function(path, zip=paste0(basename(path),".7z"), password = NULL) {
  restore.point("extract.7z")
  setwd(path)
  switch = ""
  if (!is.null(password)) {
    switch = paste0(" -p",password," ")
  }
  cmd = paste0('7z x ', zip, ' -y ',  switch)
  cat(cmd)
  system(cmd,ignore.stdout = TRUE)
}
