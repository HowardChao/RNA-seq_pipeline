#' Choose the location where 'gene_data' and 'RNAseq_bin' will be installed
#' @export
SetPrefixPath <- function(path.prefix = home.path) {
  if (isTRUE(dir.exists(path.prefix))){
    pkg.global.path.prefix$data_path <- path.prefix
    if (isTRUE(CheckPrefixPath(pkg.global.path.prefix$data_path))){
      cat(paste0("The following files will be installed under '", pkg.global.path.prefix$data_path, "'\n\n"))
    }
  } else {
    cat(paste0("(X) :Prefix path '", path.prefix, "' is invalid. Please try another one.\n\n"))
  }
}

#' Checking the absolute path
#' @export
CheckPrefixPath <- function(path.prefix = pkg.global.path.prefix$data_path, print = TRUE) {
  if (pkg.global.path.prefix$data_path == "NOT_SET_YET") {
    cat("(X) :You haven't set the prefix directory for the following steps.\n     Please run 'SetPrefixPath()' first to set the prefix directory.\n\n")
    return(FALSE)
  } else {
    if (substr(path.prefix, nchar(path.prefix), nchar(path.prefix)) != '/') {
      pkg.global.path.prefix$data_path <- paste0(path.prefix, '/')
    }
    if (isTRUE(print)) {
      cat(paste0("(O) :Prefix directory: '", pkg.global.path.prefix$data_path, "'\n\n"))
    }
    return(TRUE)
  }
}
