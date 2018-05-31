#' Examine how the transcripts compare with the reference annotation
#' @export
GffcompareRefSample <- function(gene.name = "NO_DATA", sample.pattern = "NO_DATA") {
  if (isTRUE(CheckGffcompare(print=FALSE))){
    if (isTRUE(CheckDirAll(print = FALSE))){
      if (gene.name == "NO_DATA" || sample.pattern == "NO_DATA"){
        if (gene.name == "NO_DATA") {
          cat("(\u2718) :gene.name is missing.\n\n")
        }
        if (sample.pattern == "NO_DATA") {
          cat("(\u2718) :sample.pattern is missing.\n\n")
        }
      } else if (sample.pattern == "NO_DATA") {
        cat("(\u2718) :sample.pattern is missing.\n\n")
      } else{
        check.results <- ProgressGenesFiles(gene.name, sample.pattern, print=TRUE)
        cat(paste0("\n************** Gffcompare comparing transcripts between merged and reference **************\n"))
        if ( isTRUE(check.results$stringtie_merged.gtf.file.df) && isTRUE(check.results$gtf.file.logic.df)){
          current.path <- getwd()
          setwd(paste0(pkg.global.path.prefix$data_path, "gene_data/"))
          whole.command <- paste("-r", paste0("ref_genes/", gene.name, ".gtf"), "-G -o merged/merged merged/stringtie_merged.gtf")
          cat(c("Input command :", paste("gffcompare", whole.command), "\n"))
          system2(command = "gffcompare", args = whole.command)
          cat("\n")
          on.exit(setwd(current.path))
        } else {
          cat(c(paste0("(\u2718) :'", gene.name, ".gtf'"), "or", paste0("'stringtie_merged.gtf'"), "is missing.\n\n"))
        }
      }
    }
  }
}
