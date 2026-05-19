repbox_load_libs = function() {
  libs = c("repboxRun", "repboxDB", "repboxUtils", "repboxRegmap", "repboxStata", "repboxStataReg","repboxCodeText", "repboxR",         "repboxDoc", "repboxDRF", "metaregBase", "repboxEJD")

  for (lib in libs) {
    suppressPackageStartupMessages(require(lib,character.only = TRUE,warn.conflicts = FALSE))
  }
}
