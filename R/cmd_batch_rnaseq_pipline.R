#' Create 'RNAseqPipline.R' that user can
RNAseqPipelineCMD <- function(path.prefix = "NOT_SET_YET", input.path.prefix = "NOT_SET_YET", gene.name = "NO_DATA", sample.pattern = "NO_DATA") {
  if (isTRUE(SetPrefixPath(path.prefix, print = FALSE))) {
    if (isTRUE(dir.exists(input.path.prefix))) {
      if (gene.name == "NO_DATA" || sample.pattern == "NO_DATA"){
        if (gene.name == "NO_DATA") {
          cat("(\u2718) :gene.name is missing.\n\n")
        }
        if (sample.pattern == "NO_DATA") {
          cat("(\u2718) :sample.pattern is missing.\n\n")
        }
      } else {
        current.path <- getwd()
        setwd(pkg.global.path.prefix$data_path)
        fileConn<-file("RNASEQ_PIPELINE.R")
        first <- "library(RNASeq)"
        second <- paste0('RNAseqPipeline(path.prefix = "', path.prefix, '", input.path.prefix = "', input.path.prefix, '", gene.name = "', gene.name, '", sample.pattern = "', sample.pattern, '")')
        writeLines(c(first, second), fileConn)
        close(fileConn)
        cat(" Local : Run command 'R CMD BATCH RNASEQ_PIPELINE.R &' \n")
        cat("Server : Run command 'nohup R CMD BATCH RNASEQ_PIPELINE.R &' \n\n")
        on.exit(setwd(current.path))
      }
    } else {
      cat("(\u2718) :Please give value to prefix path of 'input_files/' \n\n")
    }
  }
}
