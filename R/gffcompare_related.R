#' Examine how the transcripts compare with the reference annotation
#' @export
GffcompareRefSample <- function(gene_name = "NO_DATA") {
  if (isTRUE(CheckPrefixPath(pkg.global.path.prefix$data_path, print = FALSE))) {
    if (isTRUE(CheckDirAll(print = FALSE))){
      if (gene_name == "NO_DATA"){
        cat("(X) :gene_name is missing.\n     Can't find the target sample files to merge\n\n")
      } else{
        check.results <- CheckSampleGenesFiles(gene_name)
        if ( isTRUE(check.results$stringtie_merged.gtf.file.df) && isTRUE(check.results$gtf.file.logic.df)){
          current.path <- getwd()
          setwd(paste0(pkg.global.path.prefix$data_path, "gene_data/"))
          whole.command <- paste("-r", paste0("ref_genes/", gene_name, ".gtf"), "-G -o merged_VS_ref/merged", "merged_VS_ref/stringtie_merged.gtf")
          cat(c("Input command :", paste("gffcompare", whole.command), "\n"))
          system2(command = "gffcompare", args = whole.command)
          on.exit(setwd(current.path))
        }
      }
    }
  }
}
