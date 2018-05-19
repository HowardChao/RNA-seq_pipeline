#' install all the tools and create a binary library.
#' @export
InstallToolsCMD <- function(path.prefix = "NOT_SET_YET", input.path.prefix = "NOT_SET_YET", gene.name = "NO_DATA", sample.pattern = "NO_DATA") {
  if (path.prefix == "NOT_SET_YET" || input.path.prefix == "NOT_SET_YET" ||gene.name == "NO_DATA" || sample.pattern == "NO_DATA"){
    if (path.prefix == "NOT_SET_YET") {
      cat("(\u2718) : 'path.prefix' is missing.\n\n")
    }
    if (input.path.prefix == "NOT_SET_YET") {
      cat("(\u2718) : 'input.path.prefix' is missing.\n\n")
    }
    if (gene.name == "NO_DATA") {
      cat("(\u2718) : 'gene.name' is missing.\n\n")
    }
    if (sample.pattern == "NO_DATA") {
      cat("(\u2718) : 'sample.pattern' is missing.\n\n")
    }
  } else {
    if (isTRUE(SetPrefixPath(path.prefix, print = FALSE))) {
      if (isTRUE(CheckInputDirFiles(input.path.prefix = input.path.prefix, gene.name = gene.name, sample.pattern = sample.pattern, print = FALSE))) {
        # CheckInputDirFiles will determin whether 'gene.name' and 'sample.pattern' is valid !!
        MkdirAll()
        if (isTRUE(CheckDirAll(print = TRUE))){
          current.path <- getwd()
          setwd(pkg.global.path.prefix$data_path)
          r_script.dir <- dir.create(file.path(paste0(pkg.global.path.prefix$data_path, 'Rscript/')), showWarnings = FALSE) == 0
          r_script.out.dir <- dir.create(file.path(paste0(pkg.global.path.prefix$data_path, 'Rscript_out/')), showWarnings = FALSE) == 0
          fileConn<-file("Rscript/INSTALL_TOOLS.R")
          first <- "library(RNASeq)"
          second <- paste0("SetPrefixPath('", pkg.global.path.prefix$data_path, "')")
          third <- paste0('CheckInputDirFiles(input.path.prefix = "', input.path.prefix, '", gene.name = "', gene.name, '", sample.pattern = "', sample.pattern, '")')
          fourth <- paste0("CopyInputDir(input.path.prefix = '", pkg.global.path.prefix$input.files, "', gene.name = '", gene.name, "', sample.pattern = '", sample.pattern, "')")
          fifth <- "InstallAll()"
          sixth <-  "ExportPath()"
          seventh <- "CheckToolAll()"
          writeLines(c(first, second, third, fourth, fifth, sixth, seventh), fileConn)
          close(fileConn)
          cat(c(paste0(" Local : Run command 'R CMD BATCH ", getwd(), "/Rscript/INSTALL_TOOLS.R"),  paste0(getwd(), "/Rscript_out/INSTALL_TOOLS.Rout"), "&' \n"))
          cat(c(paste0("Server : Run command 'nohup R CMD BATCH ", getwd(), "/Rscript/INSTALL_TOOLS.R"),  paste0(getwd(), "/Rscript_out/INSTALL_TOOLS.Rout"), "&' \n\n"))
          on.exit(setwd(current.path))
        }
      }
    }
  }
}
