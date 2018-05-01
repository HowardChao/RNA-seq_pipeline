#' Display an informative message when the package loads.
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to start RNAseq R package!\n Use 'Mkdir'")
}
