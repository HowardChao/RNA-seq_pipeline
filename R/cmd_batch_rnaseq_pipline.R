#' Create 'RNAseqPipline.R' that user can
RNAseqPipelineCMD <- function(path.prefix = "NOT_SET_YET", input.files.path = "NOT_SET_YET", gene_name = "NO_DATA", sample_prefix = "NO_DATA") {
  if (isTRUE(SetPrefixPath(path.prefix, print = FALSE))) {
    if (isTRUE(dir.exists(input.files.path))) {
      if (gene_name == "NO_DATA" || sample_prefix == "NO_DATA"){
        if (gene_name == "NO_DATA") {
          cat("(\u2718) :gene_name is missing.\n\n")
        }
        if (sample_prefix == "NO_DATA") {
          cat("(\u2718) :sample_prefix is missing.\n\n")
        }
      } else {
        current.path <- getwd()
        cat("Current directory : ", pkg.global.path.prefix$data_path, "\n\n")
        setwd(pkg.global.path.prefix$data_path)
        fileConn<-file("RNAseq_pipline.R")
        first <- "library(RNASeq)"
        cat(paste0('RNAseqPipeline(path.prefix = "', path.prefix, '", input.files.path = "', input.files.path, '", gene_name = "', gene_name, '", sample_prefix = "', sample_prefix, '")'))
        second <- paste0('RNAseqPipeline(path.prefix = "', path.prefix, '", input.files.path = "', input.files.path, '", gene_name = "', gene_name, '", sample_prefix = "', sample_prefix, '")')
        writeLines(c(first, second), fileConn)
        close(fileConn)
        on.exit(setwd(current.path))
      }
    } else {
      cat("(\u2718) :Please give value to prefix path of 'input_files/' \n\n")
    }
  }
}
