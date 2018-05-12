#' check 'gene_data' and subdirectory files exit
#' @export
ProgressGenesFiles <- function(gene_name = "NO_DATA", print = TRUE) {
  if (isTRUE(CheckDirAll(print = FALSE))){
    if (gene_name == "NO_DATA"){
      cat("(\u2718) :gene_name is missing.\nSample files checking fails\n\n")
      return(FALSE)
    } else {
      current.path <- getwd()
      setwd(paste0(pkg.global.path.prefix$data_path, "gene_data/"))
      cat(paste0("************** Current progress of RNA-seq files in '", paste0(pkg.global.path.prefix$data_path, "gene_data/'"), " **************\n"))
      gtf.file <- file.exists(paste0(getwd(), '/ref_genes/', gene_name, '.gtf'))
      if (isTRUE(gtf.file)) {
        if(isTRUE(print)){
          cat(c("(\u2714) :", paste0("'",getwd(), '/ref_genes/', gene_name, '.gtf', "'"), "is exit\n\n"))
        }
      } else {
        cat(c("(\u2718) :", paste0("'",getwd(), '/ref_genes/', gene_name, '.gtf', "'"), "is not exit\n"))
        cat(c("     Put the", paste0("'",gene_name,".gtf", "'"), "file in", paste0("'",getwd(), '/ref_genes/', "'"), "to fix the error.\n\n"))
      }
      fa.file <- file.exists(paste0(getwd(), '/ref_genome/', gene_name, '.fa'))
      if (isTRUE(fa.file)) {
        if(isTRUE(print)){
          cat(c("(\u2714) :",paste0("'", getwd(), '/ref_genome/', gene_name, '.fa', "'"), "is exit\n\n"))
        }
      } else {
        cat(c("(\u2718) :",paste0("'", getwd(), '/ref_genome/', gene_name, '.fa', "'"), "is not exit\n"))
        cat(c("     Put the", paste0("'",gene_name,".fa", "'"), "file in", paste0("'",getwd(), '/ref_genome/', "'"), "to fix the error.\n\n"))
      }
      fastq.gz.files <- list.files(path = paste0(getwd(), '/raw_fastq.gz/'), pattern = paste0( "^[A-Z, a-z]*", "[0-9]*", "[A-Z, a-z]*", "_", "[r]*[R]*","[1-2]*.fastq.gz$"), all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE)
      fastq.gz.files.number <- length(fastq.gz.files)
      if (fastq.gz.files.number != 0){
        if(isTRUE(print)){
          for (i in fastq.gz.files){
            cat(c("(\u2714) :", paste0("'", getwd(), '/raw_fastq.gz/', i, "'"), "is exit\n"))
          }
          cat(c("Total:", fastq.gz.files.number, "files\n\n"))
        }
      }else {
        cat(c("(\u2718) :", paste0('\'',getwd(), '/raw_fastq.gz/XXX_*.fastq.gz\''), "is not exit\n"))
        cat(c("     Put the", "'XXX_*.fastq.gz'", "file in", paste0("'",getwd(), '/raw_fastq.gz/', "'"), "to fix the error.\n\n"))
      }
      phenodata.file <- file.exists(paste0(getwd(), '/ref_genes/phenodata.csv'))
      if (isTRUE(gtf.file)) {
        if(isTRUE(print)){
          cat(c("(\u2714) :", paste0("'",getwd(), '/ref_genes/phenodata.csv', "'"), "is exit\n\n"))
        }
      } else {
        cat(c("(\u2718) :", paste0("'",getwd(), '/ref_genes/phenodata.csv', "'"), "is not exit\n"))
        cat(c("     Put the", paste0("'phenodata.csv'"), "file in", paste0("'",getwd(), '/ref_genes/', "'"), "to fix the error.\n\n"))
      }
      ht2.files <- list.files(path = paste0(getwd(), '/indexes/'), pattern = paste0("^", gene_name, "_tran.[0-9]*.ht2$"), all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE)
      ht2.files.number <- length(ht2.files)
      if (ht2.files.number != 0) {
        if (isTRUE(print)) {
          for (i in ht2.files){
            cat(c("(\u2714) :",paste0("'",getwd(), '/indexes/', i, "'"), "is exit\n"))
          }
          cat(c("Total:", ht2.files.number, "files\n\n"))
        }
      } else {
        cat(c("(\u231B) :", paste0('\'',getwd(), '/indexes/', gene_name, '_tran.*.ht2\''), "is not exit\n"))
        cat("     Files haven't created yet. Run 'CreateHisat2Index()' to generate '*.ht2' files\n\n")
      }
      sam.files <- list.files(path = paste0(getwd(), '/raw_sam/'), pattern = paste0( "^[A-Z, a-z]*", "[0-9]*", "[A-Z, a-z]*", ".sam$"), all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE)
      sam.files.number <- length(sam.files)
      if (sam.files.number != 0){
        if (isTRUE(print)) {
          for (i in sam.files){
            cat(c("(\u2714) :", paste0("'", getwd(), '/raw_sam/', i, "'"), "is exit\n"))
          }
          cat(c("Total:", sam.files.number, "files\n\n"))
        }
      }else {
        cat(c("(\u231B) :", paste0('\'',getwd(), '/raw_sam/XXX.sam\''), "is not exit\n"))
        cat("     Files haven't created yet. Run 'Hisat2AlignmentDefault()' to generate 'XXX.sam' files\n\n")
      }
      bam.files <- list.files(path = paste0(getwd(), '/raw_bam/'), pattern = paste0( "^[A-Z, a-z]*", "[0-9]*", "[A-Z, a-z]*", ".bam$"), all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE)
      bam.files.number <- length(bam.files)
      if (bam.files.number != 0){
        if (isTRUE(print)) {
          for (i in bam.files){
            cat(c("(\u2714) :", paste0("'", getwd(), '/raw_bam/', i, "'"), "is exit\n"))
          }
          cat(c("Total:", bam.files.number, "files\n\n"))
        }
      }else {
        cat(c("(\u231B) :", paste0('\'',getwd(), '/raw_sam/XXX.bam\''), "is not exit\n"))
        cat("     Files haven't created yet. Run 'SamtoolsToBam()' to generate 'XXX.bam' files\n\n")
      }
      gtf.files <- list.files(path = paste0(getwd(), '/raw_gtf/'), pattern = paste0("^[A-Z, a-z]*", "[0-9]*", "[A-Z, a-z]*", ".gtf$"), all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE)
      gtf.files.number <- length(gtf.files)
      if (gtf.files.number != 0){
        if (isTRUE(print)) {
          for (i in gtf.files){
            cat(c("(\u2714) :", paste0("'", getwd(), '/raw_gtf/', i, "'"), "is exit\n"))
          }
          cat(c("Total:", gtf.files.number, "files\n\n"))
        }
      }else {
        cat(c("(\u231B) :", paste0('\'',getwd(), '/raw_gtf/XXX.gtf\''), "is not exit\n"))
        cat("     Files haven't created yet. Run 'StringTieAssemble()' to generate 'XXX.gtf' files\n\n")
      }
      stringtie_merged.gtf.file <- file.exists(paste0(getwd(), '/merged/stringtie_merged.gtf'))
      if (isTRUE(stringtie_merged.gtf.file)) {
        if (isTRUE(print)) {
          cat(c("(\u2714) :", paste0("'",getwd(), "/merged/stringtie_merged.gtf", "'"), "is exit\n\n"))
        }
      } else {
        cat(c("(\u231B) :", paste0("'",getwd(), "/merged/stringtie_merged.gtf", "'"), "is not exit\n"))
        cat("     Files haven't created yet. Run 'StringTieMergeTrans()' to generate 'stringtie_merged.gtf' files\n\n")
      }
      ballgown.dirs <- list.files(path = paste0(getwd(), '/ballgown/'), pattern = paste0( "^[a-z,A-Z]*[0-9]*[a-z,A-Z]*$"), all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE)
      ballgown.dirs.number <- length(ballgown.dirs)
      if (ballgown.dirs.number != 0){
        if (isTRUE(print)) {
          for (i in ballgown.dirs){
            cat(c("(\u2714) :", paste0("'", getwd(), '/ballgown/', i, "'"), "is exit\n"))
          }
          cat(c("Total:", ballgown.dirs.number, "directories\n\n"))
        }
      }else {
        cat(c("(\u231B) :", paste0('\'',getwd(), '/ballgown/XXX/'), "is not exit\n"))
        cat("     Directories haven't created yet. Run 'StringTieToBallgown()' to generate ballgown/XXX/ directories\n\n")
      }

      on.exit(setwd(current.path))
      return(list(gtf.file.logic.df = gtf.file, fa.file.logic.df = fa.file,
                  fastq.gz.files.number.df = fastq.gz.files.number,
                  fastq.gz.files.df = fastq.gz.files,
                  phenodata.file.df = phenodata.file,
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
