#' use 'samtools' to sort and convert the SAM files to BAM
#' @export
SamtoolsToBam <- function(gene.name = "NO_DATA", sample.pattern = "NO_DATA") {
  if (isTRUE(CheckSamtools(print=FALSE))) {
    if (isTRUE(CheckDirAll(print = FALSE))) {
      if (gene.name == "NO_DATA" || sample.pattern == "NO_DATA"){
        if (gene.name == "NO_DATA") {
          cat("(\u2718) :gene.name is missing.\n\n")
        }
        if (sample.pattern == "NO_DATA") {
          cat("(\u2718) :sample.pattern is missing.\n\n")
        }
      } else {
        check.results <- ProgressGenesFiles(gene.name, sample.pattern, print=TRUE)
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
            if (i != 1) cat("\n")
            cat(c("Input command :", paste("samtools", whole.command), "\n"))
            system2(command = "samtools", args = whole.command)
          }
          cat("\n")
          on.exit(setwd(current.path))
        } else {
          cat(c("(\u2718) :'XXX.sam' is missing.\n\n"))
        }
      }
    }
  }
}
