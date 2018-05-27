#' Create 'RNAseqPipline.R' that user can
#'
RNAseqPipelineCMD <- function(path.prefix = "NOT_SET_YET", input.path.prefix = "NOT_SET_YET", gene.name = "NO_DATA", sample.pattern = "NO_DATA", num.parallel.threads = 8) {
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
    if (isTRUE(SetPrefixPath(path.prefix = path.prefix, print = FALSE))) {
      # not print but if the prefix is invalid, then 'Prefix path '", path.prefix, "' is invalid. Please try another one.' will be printed.
      if (isTRUE(CheckPrefixPath(path.prefix = pkg.global.path.prefix$data_path, print = TRUE))) {
        if (isTRUE(CheckDirAll(print = TRUE))) {
          results <- ProgressGenesFiles(gene.name = gene.name, sample.pattern = sample.pattern, print=TRUE)
          if (isTRUE(results$gtf.file.logic.df) && isTRUE(results$fa.file.logic.df) && results$fastq.gz.files.number.df != 0 && isTRUE(results$phenodata.file.df) && (results$phenodata.invalid.column.number.df == 0)) {
            # If precheck doesn't have .ht2 files is fine
            ExportPath()
            if (isTRUE(CheckToolAll(print=TRUE))) {
              cat("(\u2714) : Successful in RNAseq-pipeline precheck. \n\n")
              current.path <- getwd()
              setwd(pkg.global.path.prefix$data_path)
              fileConn<-file("Rscript/RNASEQ_PIPELINE.R")
              first <- "library(RNASeq)"
              second <- paste0('RNAseqPipeline(path.prefix = "', path.prefix, '", input.path.prefix = "', input.path.prefix, '", gene.name = "', gene.name, '", sample.pattern = "', sample.pattern, '", num.parallel.threads = "', num.parallel.threads, '")')
              writeLines(c(first, second), fileConn)
              close(fileConn)
              cat(c(paste0(" Local : Run command 'R CMD BATCH ", getwd(), "/Rscript/RNASEQ_PIPELINE.R"),  paste0(getwd(), "/Rscript_out/RNASEQ_PIPELINE.Rout"), "&' \n"))
              cat(c(paste0("Server : Run command 'nohup R CMD BATCH ", getwd(), "/Rscript/RNASEQ_PIPELINE.R"),  paste0(getwd(), "/Rscript_out/RNASEQ_PIPELINE.Rout"), "&' \n\n"))
              on.exit(setwd(current.path))
            }
          }
        }
      }
    }
  }
}

