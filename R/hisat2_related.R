#' Creating Hisat2 index
#' Creat Hisat2 index for further use
#' @export
CreateHisat2Index <- function (gene_name = "NO_DATA", splice.site.info = TRUE, exon.info = TRUE) {
  ## need to change 'Let the user can choose where to put their files'
  ## need to learn how to get the filenames under a directory
  ## need to use 'real filename' rather than the 'chrX'
  if (isTRUE(CheckPrefixPath(pkg.global.path.prefix$data_path, print = FALSE))) {
    if (isTRUE(CheckDirAll(print = FALSE))){
      if (gene_name == "NO_DATA"){
        cat("(X) :gene_name is missing.\n     Can't find the target sample files to align.\n\n")
      } else {
        if (!is.logical(splice.site.info) || !is.logical(exon.info)) {
          cat("Please make sure the type of 'splice.site.info' and 'exon.info' are logical.\n")
        } else {
          current.path <- getwd()
          setwd(paste0(pkg.global.path.prefix$data_path, "gene_data/indexes/"))
          if (isTRUE(splice.site.info)) {
            system2(command = 'extract_splice_sites.py', args = c(paste0(pkg.global.path.prefix$data_path, 'gene_data/genes/', gene_name, '.gtf'), '>', paste0(gene_name, '.ss')))
          }
          if (isTRUE(exon.info)) {
            system2(command = 'extract_exons.py', args = c(paste0(pkg.global.path.prefix$data_path, 'gene_data/genes/', gene_name, '.gtf'), '>', paste0(gene_name, '.exon')))
          }

          if (isTRUE(splice.site.info) && isTRUE(exon.info)) {
            system2(command = 'hisat2-build', args = c('--ss', paste0(gene_name, '.ss'), '--exon', paste0(gene_name, '.exon'), paste0(pkg.global.path.prefix$data_path, 'gene_data/genome/', gene_name, '.fa'), paste0(gene_name, '_tran')))
          } else if (isTRUE(splice.site.info) && !isTRUE(exon.info)) {
            system2(command = 'hisat2-build', args = c('--ss', paste0(gene_name, '.ss'), paste0(pkg.global.path.prefix$data_path, 'gene_data/genome/', gene_name, '.fa'), paste0(gene_name, '_tran')))
          } else if (!isTRUE(splice.site.info) && isTRUE(exon.info)) {
            system2(command = 'hisat2-build', args = c('--exon', paste0(gene_name, '.exon'), paste0(pkg.global.path.prefix$data_path, 'gene_data/genome/', gene_name, '.fa'), paste0(gene_name, '_tran')))
          } else {
            system2(command = 'hisat2-build', args = c(paste0(pkg.global.path.prefix$data_path, 'gene_data/genome/', gene_name, '.fa'), paste0(gene_name, '_tran')))
          }
          on.exit(setwd(current.path))
          cat(paste0("'", pkg.global.path.prefix$data_path, "gene_data/indexes/", gene_name, "_tran.*.ht2' has been created.\n\n"))
        }
      }
    }
  }
}

#' hisat2 alignment default
#' @export
Hisat2AlignmentDefault <- function(gene_name = "NO_DATA") {
  if (isTRUE(CheckPrefixPath(pkg.global.path.prefix$data_path, print = FALSE))) {
    if (isTRUE(CheckDirAll(print = FALSE))){
      if (gene_name == "NO_DATA"){
        cat("(X) :gene_name is missing.\n     Can't find the target sample files to align.\n\n")
      } else {
        check.results <- CheckSampleGenesFiles(gene_name)
        if (check.results$ht2.files.number.df != 0 && check.results$fastq.gz.files.number.df != 0){
          # Map reads to each alignment
          current.path <- getwd()
          setwd(paste0(pkg.global.path.prefix$data_path, "gene_data/"))
          sample.table <- table(gsub(paste0(gene_name, "_[0-9]*.fastq.gz$"), replace = "", check.results$fastq.gz.files.df))
          iteration.num <- length(sample.table)
          sample.name <- names(sample.table)
          sample.value <- as.vector(sample.table)
          for( i in 1:iteration.num){
            current.sub.command <- ""
            total.sub.command <- ""
            for ( j in 1:sample.value[i]){
              current.sub.command <- paste(paste0("-", j),  paste0("samples_.fastq.gz/", sample.name[i], gene_name, "_", j, ".fastq.gz"))
              total.sub.command <- paste(total.sub.command, current.sub.command)
            }
            whole.command <- paste("-p 8 --dta -x", paste0("indexes/", gene_name, "_tran"), total.sub.command, "-S", paste0("samples_.sam/", sample.name[i], gene_name,".sam") )
            cat(c("Input command :", paste("hisat2", whole.command), "\n"))
            system2(command = "hisat2", args = whole.command)
          }
          on.exit(setwd(current.path))
        }
      }
    }
  }
}
