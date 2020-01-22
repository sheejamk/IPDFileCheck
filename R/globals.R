#' Required to avoid the note/warning with RMD check "no visible binding for global variable"
#' @import utils
utils::globalVariables(names=c("install.packages","installed.packages",
                         "libPaths", "contrib.url", "repos","type","method","INSTALL_opts"),package="IPDFileCheck",add=TRUE)
