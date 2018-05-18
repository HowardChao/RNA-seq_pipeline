#' Check everything before calling 'RNAseqPipelineCMD'
#' @export
CheckAllParameter <- function(path.prefix = "NOT_SET_YET", input.path.prefix = "NOT_SET_YET", gene.name = "NO_DATA", sample.pattern = "NO_DATA") {
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
          print(results$gtf.file.logic.df)
          print(results$fa.file.logic.df)
          print(results$fastq.gz.files.number.df)
          print(results$phenodata.file.df)
          if (isTRUE(results$gtf.file.logic.df) && isTRUE(results$fa.file.logic.df) && results$fastq.gz.files.number.df != 0 && isTRUE(results$phenodata.file.df)) {
            ExportPath()
            if (isTRUE(CheckToolAll(print=TRUE))) {
              cat("(\u2714) : Successful in RNAseq-pipeline precheck. \n\n")
              cat(paste0("Run 'RNAseqPipelineCMD(path.prefix = \"", pkg.global.path.prefix$data_path, "\", input.path.prefix = \"", input.path.prefix, "\",  gene.name = \"", gene.name, "\", sample.pattern = \"", sample.pattern, "\")'\n\n" ))
            }
          }
        }
      }
    }
  }
}
