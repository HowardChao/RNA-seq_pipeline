#' check 'gene_data' and subdirectory files exit
#' @export
CheckSampleGenesFiles <- function(gene_name = "NO_DATA") {
  if (isTRUE(CheckPrefixPath(pkg.global.path.prefix$data_path, print = FALSE))) {
    if (isTRUE(CheckDirAll())){
      if (gene_name == "NO_DATA"){
        cat("gene_name is missing.\n\n")
        return(FALSE)
      } else {
        current.path <- getwd()
        setwd(paste0(pkg.global.path.prefix$data_path, "gene_data/"))
        cat(paste0("************** Checking validity of gene data in '", paste0(pkg.global.path.prefix$data_path, "gene_data/'"), " **************\n"))
        gtf.file <- file.exists(paste0(getwd(), '/genes/', gene_name, '.gtf'))
        if (isTRUE(gtf.file)) {
          cat(c("(O) :", paste0("'",getwd(), '/genes/', gene_name, '.gtf', "'"), "is exit\n\n"))
        } else {
          cat(c("(X) :", paste0("'",getwd(), '/genes/', gene_name, '.gtf', "'"), "is not exit\n"))
          cat(c("Put the", paste0("'",gene_name,".gtf", "'"), "file in", paste0("'",getwd(), '/genome/', "'"), "to fix the error.\n\n"))
        }
        fa.file <- file.exists(paste0(getwd(), '/genome/', gene_name, '.fa'))
        if (isTRUE(fa.file)) {
          cat(c("(O) :",paste0("'", getwd(), '/genome/', gene_name, '.fa', "'"), "is exit\n\n"))
        } else {
          cat(c("(X) :",paste0("'", getwd(), '/genome/', gene_name, '.fa', "'"), "is not exit\n"))
          cat(c("Put the", paste0("'",gene_name,".fa", "'"), "file in", paste0("'",getwd(), '/genes/', "'"), "to fix the error.\n\n"))
        }
        ht2.files <- list.files(path = paste0(getwd(), '/indexes/'), pattern = paste0("^", gene_name, "_tran.[0-9]*.ht2$"), all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE)
        ht2.files.number <- length(ht2.files)
        if (ht2.files.number != 0) {
          for (i in ht2.files){
            cat(c("(O) :",paste0("'",getwd(), '/indexes/', i, "'"), "is exit\n"))
          }
          cat(c("Total:", ht2.files.number, "files\n\n"))
        } else {
          cat(c("(X) :", paste0('\'',getwd(), '/indexes/', gene_name, '_tran.*.ht2\''), "is not exit\n"))
          cat("Run 'CreateHisat2Index()' to make '*_tran.*.ht2' files\n\n")
        }
        fastq.gz.files <- list.files(path = paste0(getwd(), '/samples/'), pattern = paste0( "^[A-Z]*", "[0-9]*", "_", gene_name, "_[0-9]*.fastq.gz$"), all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE)
        fastq.gz.files.number <- length(fastq.gz.files)
        if (fastq.gz.files.number != 0){
          for (i in fastq.gz.files){
            cat(c("(O) :", paste0("'", getwd(), '/samples/', i, "'"), "is exit\n"))
          }
          cat(c("Total:", fastq.gz.files.number, "files\n\n"))
        }else {
          cat(c("(X) :", paste0('\'',getwd(), '/samples/', gene_name, '_*.fastq.gz\''), "is not exit\n"))
          cat(c("Put the", paste0("'*_",gene_name,".*_tran.*.ht2", "'"), "file in", paste0("'",getwd(), '/samples/', "'"), "to fix the error.\n\n"))
        }
        on.exit(setwd(current.path))
        return(list(gtf.file.logic.df = gtf.file, fa.file.logic.df = fa.file, ht2.files.number.df = ht2.files.number, ht2.files.df = ht2.files, fastq.gz.files.number.df = fastq.gz.files.number, fasfastq.gz.files.df = fastq.gz.files))
      }
    }
  }
}


#' hisat2 alignment default
#' @export
Hisat2AlignmentDefault <- function(gene_name = "NO_DATA") {
  check.results <- CheckSampleGenesFiles(gene_name)
  if (check.results$ht2.files.number != 0 && check.results$fastq.gz.files.number != 0){
    # Map reads to each alignment
    print(typeof(check.results$fasfastq.gz.files.df))
    for ( i in check.results$fastq.gz.files){
      print(i)
    }
  }
}
