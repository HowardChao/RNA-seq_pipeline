#' Choose the location where 'gene_data' and 'RNAseq_bin' will be installed
#' @export
SetPrefixPath <- function(path.prefix = "NOT_SET_YET") {
  # Check the prefix exist
  if (isTRUE(dir.exists(path.prefix))){
    if (substr(path.prefix, nchar(path.prefix), nchar(path.prefix)) != '/') {
      pkg.global.path.prefix$data_path <- paste0(path.prefix, '/')
    } else {
      pkg.global.path.prefix$data_path <- path.prefix
    }
    cat(c("************** Setting prefix path ************\n"))
    cat(paste0("The following files will be installed under '", pkg.global.path.prefix$data_path, "'\n\n"))
    return(TRUE)
  } else if (path.prefix == "NOT_SET_YET") {
    cat("(\u2718) :Please give value to prefix path.\n\n")
    return(FALSE)
  } else {
    cat(paste0("(\u2718) :Prefix path '", path.prefix, "' is invalid. Please try another one.\n\n"))
    return(FALSE)
  }
}

#' Checking the absolute path
#' @export
CheckPrefixPath <- function(path.prefix = pkg.global.path.prefix$data_path, print = TRUE) {
  if (print) {
    cat(c("************** Checking prefix path ************\n"))
  }
  if (path.prefix == "NOT_SET_YET") {
    cat("(\u2718) :You haven't set the prefix directory for the following steps.\n     Please run 'SetPrefixPath()' first to set the prefix directory.\n\n")
    return(FALSE)
  } else {
    if (print) {
      cat(paste0("(\u2714) :Prefix directory: '", pkg.global.path.prefix$data_path, "'\n\n"))
      # print(ls.str(pkg.global.path.prefix))
    }
    return(TRUE)
  }
}
