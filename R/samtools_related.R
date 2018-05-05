#' use 'samtools' to sort and convert the SAM files to BAM
#' @export
SamtoolsToBam <- function(gene_name = "NO_DATA") {
  if (isTRUE(CheckDirAll(print = FALSE))){
    if (gene_name == "NO_DATA"){
      cat("(X) :gene_name is missing.\n     Can't find the target sample files to align.\n\n")
    } else {
      check.results <- CheckSampleGenesFiles(gene_name)
      cat(paste0("\n************** Samtools converting '.sam' to '.bam' **************\n"))
      if (check.results$sam.files.number.df != 0){
        # Map reads to each alignment
        current.path <- getwd()
        setwd(paste0(pkg.global.path.prefix$data_path, "gene_data/"))
        sample.table <- table(gsub(paste0(".sam$"), replace = "", check.results$sam.files.df))
        iteration.num <- length(sample.table)
        sample.name <- names(sample.table)
        sample.value <- as.vector(sample.table)
        for( i in 1:iteration.num){
          whole.command <- paste("sort -@ 8 -o", paste0("raw_bam/", sample.name[i], ".bam"), paste0("raw_sam/", sample.name[i], ".sam"))
          cat(c("Input command :", paste("samtools", whole.command), "\n"))
          system2(command = "samtools", args = whole.command)
        }
        on.exit(setwd(current.path))
      } else {
        cat(c("(X) :'XXX.sam' is missing.\n\n"))
      }
    }
  }
}
