#' check 'gene_data' and subdirectory files exit
#' @export
CheckSampleGenesFiles <- function(gene_name = "NO_DATA") {
  if (isTRUE(CheckPrefixPath(pkg.global.path.prefix$data_path, print = FALSE))) {
    if (isTRUE(CheckDirAll(print = FALSE))){
      if (gene_name == "NO_DATA"){
        cat("(X) :gene_name is missing.\nSample files checking fails\n\n")
        return(FALSE)
      } else {
        current.path <- getwd()
        setwd(paste0(pkg.global.path.prefix$data_path, "gene_data/"))
        cat(paste0("************** Checking validity of gene data in '", paste0(pkg.global.path.prefix$data_path, "gene_data/'"), " **************\n"))
        gtf.file <- file.exists(paste0(getwd(), '/ref_genes/', gene_name, '.gtf'))
        if (isTRUE(gtf.file)) {
          cat(c("(O) :", paste0("'",getwd(), '/ref_genes/', gene_name, '.gtf', "'"), "is exit\n\n"))
        } else {
          cat(c("(X) :", paste0("'",getwd(), '/ref_genes/', gene_name, '.gtf', "'"), "is not exit\n"))
          cat(c("     Put the", paste0("'",gene_name,".gtf", "'"), "file in", paste0("'",getwd(), '/ref_genes/', "'"), "to fix the error.\n\n"))
        }
        fa.file <- file.exists(paste0(getwd(), '/ref_genome/', gene_name, '.fa'))
        if (isTRUE(fa.file)) {
          cat(c("(O) :",paste0("'", getwd(), '/ref_genome/', gene_name, '.fa', "'"), "is exit\n\n"))
        } else {
          cat(c("(X) :",paste0("'", getwd(), '/ref_genome/', gene_name, '.fa', "'"), "is not exit\n"))
          cat(c("     Put the", paste0("'",gene_name,".fa", "'"), "file in", paste0("'",getwd(), '/ref_genome/', "'"), "to fix the error.\n\n"))
        }
        fastq.gz.files <- list.files(path = paste0(getwd(), '/samples_.fastq.gz/'), pattern = paste0( "^[A-Z]*", "[0-9]*", "_", gene_name, "_[0-9]*.fastq.gz$"), all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE)
        fastq.gz.files.number <- length(fastq.gz.files)
        if (fastq.gz.files.number != 0){
          for (i in fastq.gz.files){
            cat(c("(O) :", paste0("'", getwd(), '/samples_.fastq.gz/', i, "'"), "is exit\n"))
          }
          cat(c("Total:", fastq.gz.files.number, "files\n\n"))
        }else {
          cat(c("(X) :", paste0('\'',getwd(), '/samples_.fastq.gz/*', gene_name, '_*.fastq.gz\''), "is not exit\n"))
          cat(c("     Put the", paste0("'*_",gene_name,"._*.fastq.gz", "'"), "file in", paste0("'",getwd(), '/samples_.fastq.gz/', "'"), "to fix the error.\n\n"))
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
          cat("     Run 'CreateHisat2Index()' to generate '*.ht2' files\n\n")
        }
        sam.files <- list.files(path = paste0(getwd(), '/samples_.sam/'), pattern = paste0( "^[A-Z]*", "[0-9]*", "_", gene_name, ".sam$"), all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE)
        sam.files.number <- length(sam.files)
        if (sam.files.number != 0){
          for (i in sam.files){
            cat(c("(O) :", paste0("'", getwd(), '/samples_.sam/', i, "'"), "is exit\n"))
          }
          cat(c("Total:", sam.files.number, "files\n\n"))
        }else {
          cat(c("(X) :", paste0('\'',getwd(), '/samples_.sam/*', gene_name, '.sam\''), "is not exit\n"))
          cat("     Run 'Hisat2AlignmentDefault()' to generate '*.sam' files\n\n")
        }
        bam.files <- list.files(path = paste0(getwd(), '/samples_.bam/'), pattern = paste0( "^[A-Z]*", "[0-9]*", "_", gene_name, ".bam$"), all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE)
        bam.files.number <- length(bam.files)
        if (bam.files.number != 0){
          for (i in bam.files){
            cat(c("(O) :", paste0("'", getwd(), '/samples_.bam/', i, "'"), "is exit\n"))
          }
          cat(c("Total:", bam.files.number, "files\n\n"))
        }else {
          cat(c("(X) :", paste0('\'',getwd(), '/samples_.bam/*', gene_name, '.bam\''), "is not exit\n"))
          cat("     Run 'SamtoolsToBam()' to generate '*.bam' files\n\n")
        }
        gtf.files <- list.files(path = paste0(getwd(), '/samples_.gtf/'), pattern = paste0( "^[A-Z]*", "[0-9]*", "_", gene_name, ".gtf$"), all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE)
        gtf.files.number <- length(gtf.files)
        if (gtf.files.number != 0){
          for (i in gtf.files){
            cat(c("(O) :", paste0("'", getwd(), '/samples_.gtf/', i, "'"), "is exit\n"))
          }
          cat(c("Total:", gtf.files.number, "files\n\n"))
        }else {
          cat(c("(X) :", paste0('\'',getwd(), '/samples_.gtf/*', gene_name, '.gtf\''), "is not exit\n"))
          cat("     Run 'StringTieAssemble()' to generate '*.gtf' files\n\n")
        }
        stringtie_merged.gtf.file <- file.exists(paste0(getwd(), '/merged_VS_ref/stringtie_merged.gtf'))
        if (isTRUE(stringtie_merged.gtf.file)) {
          cat(c("(O) :", paste0("'",getwd(), "/merged_VS_ref/stringtie_merged.gtf", "'"), "is exit\n\n"))
        } else {
          cat(c("(X) :", paste0("'",getwd(), "/merged_VS_ref/stringtie_merged.gtf", "'"), "is not exit\n"))
          cat("     Run 'StringTieMergeTrans()' to generate 'stringtie_merged.gtf' files\n\n")
        }
        fastq.gz.files <- list.files(path = paste0(getwd(), '/samples_.fastq.gz/'), pattern = paste0( "^[A-Z]*", "[0-9]*", "_", gene_name, "_[0-9]*.fastq.gz$"), all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE)

        ballgown.dirs <- list.files(path = paste0(getwd(), '/ballgown/'), pattern = paste0( "^[a-z,A-Z]*[0-9]*$"), all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE)
        ballgown.dirs.number <- length(ballgown.dirs)
        if (ballgown.dirs.number != 0){
          for (i in ballgown.dirs){
            cat(c("(O) :", paste0("'", getwd(), '/ballgown/', i, "'"), "is exit\n"))
          }
          cat(c("Total:", ballgown.dirs.number, "directories\n\n"))
        }else {
          cat(c("(X) :", paste0('\'',getwd(), '/ballgown/XXXX(gene)/'), "is not exit\n"))
          cat("     Run 'StringTieToBallgown()' to generate ballgown/XXXX(gene)/ directories\n\n")
        }

        on.exit(setwd(current.path))
        return(list(gtf.file.logic.df = gtf.file, fa.file.logic.df = fa.file,
                    fastq.gz.files.number.df = fastq.gz.files.number,
                    fastq.gz.files.df = fastq.gz.files,
                    ht2.files.number.df = ht2.files.number,
                    ht2.files.df = ht2.files,
                    sam.files.number.df = sam.files.number,
                    sam.files.df = sam.files,
                    bam.files.number.df = bam.files.number,
                    bam.files.df = bam.files,
                    gtf.files.number.df = gtf.files.number,
                    gtf.files.df = gtf.files,
                    stringtie_merged.gtf.file.df = stringtie_merged.gtf.file,
                    ballgown.dirs.number.df = ballgown.dirs.number,
                    ballgown.dirs.df = ballgown.dirs))
      }
    }
  }
}
