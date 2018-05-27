#' check 'gene_data' and subdirectory files exit
#' @export
ProgressGenesFiles <- function(gene.name = "NO_DATA", sample.pattern = "NO_DATA", print = TRUE) {
  if (isTRUE(CheckDirAll(print = FALSE))){
    if (gene.name == "NO_DATA" || sample.pattern == "NO_DATA"){
      if (gene.name == "NO_DATA") {
        cat("(\u2718) :gene.name is missing.\n     Gene files checking fails\n\n")
      }
      if (sample.pattern == "NO_DATA"){
        cat("(\u2718) :sample.pattern is missing.\n     Gene files checking fails\n\n")
      }
      return(FALSE)
    } else {
      current.path <- getwd()
      setwd(paste0(pkg.global.path.prefix$data_path, "gene_data/"))
      if (print) {
        cat(paste0("************** Current progress of RNA-seq files in '", paste0(pkg.global.path.prefix$data_path, "gene_data/'"), " **************\n"))
      }
      gtf.file <- file.exists(paste0(pkg.global.path.prefix$data_path, "gene_data/", '/ref_genes/', gene.name, '.gtf'))
      if (isTRUE(gtf.file)) {
        if(print){
          cat(c("(\u2714) :", paste0("'",pkg.global.path.prefix$data_path, "gene_data/", '/ref_genes/', gene.name, '.gtf', "'"), "is exit\n\n"))
        }
      } else {
        cat(c("(\u2718) :", paste0("'",pkg.global.path.prefix$data_path, "gene_data/", '/ref_genes/', gene.name, '.gtf', "'"), "is not exit\n"))
        cat(c("     Put the", paste0("'",gene.name,".gtf", "'"), "file in", paste0("'",pkg.global.path.prefix$data_path, "gene_data/", '/ref_genes/', "'"), "to fix the error.\n\n"))
      }
      fa.file <- file.exists(paste0(pkg.global.path.prefix$data_path, "gene_data/", '/ref_genome/', gene.name, '.fa'))
      if (isTRUE(fa.file)) {
        if(print){
          cat(c("(\u2714) :",paste0("'", pkg.global.path.prefix$data_path, "gene_data/", '/ref_genome/', gene.name, '.fa', "'"), "is exit\n\n"))
        }
      } else {
        cat(c("(\u2718) :",paste0("'", pkg.global.path.prefix$data_path, "gene_data/", '/ref_genome/', gene.name, '.fa', "'"), "is not exit\n"))
        cat(c("     Put the", paste0("'",gene.name,".fa", "'"), "file in", paste0("'",pkg.global.path.prefix$data_path, "gene_data/", '/ref_genome/', "'"), "to fix the error.\n\n"))
      }
      fastq.gz.files <- list.files(path = paste0(pkg.global.path.prefix$data_path, "gene_data/", '/raw_fastq.gz/'), pattern = sample.pattern, all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE)
      fastq.gz.files.number <- length(fastq.gz.files)
      if (fastq.gz.files.number != 0){
        if(print){
          for (i in fastq.gz.files){
            cat(c("(\u2714) :", paste0("'", pkg.global.path.prefix$data_path, "gene_data/", '/raw_fastq.gz/', i, "'"), "is exit\n"))
          }
          cat(c("Total:", fastq.gz.files.number, "files\n\n"))
        }
      }else {
        cat(c("(\u2718) :", paste0('\'',pkg.global.path.prefix$data_path, "gene_data/", '/raw_fastq.gz/XXX_*.fastq.gz\''), "is not exit\n"))
        cat(c("     Put the", paste0('XXX_*.fastq.gz'), "file in", paste0("'",pkg.global.path.prefix$data_path, "gene_data/", '/raw_fastq.gz/', "'"), "to fix the error.\n\n"))
      }
      phenodata.file <- file.exists(paste0(pkg.global.path.prefix$data_path, "gene_data/", '/phenodata.csv'))
      invalid.column.number <- -1
      pheno_data <- read.csv(paste0(pkg.global.path.prefix$data_path, "gene_data/", "/phenodata.csv"))
      sample.table <- as.data.frame(table(pheno_data[1]))
      compare.sample.table <- as.data.frame(table(pheno_data[2]))
      extract.fastq.gz.sample.names <- unique(gsub("_[1-2]*.fastq.gz", "", fastq.gz.files))
      if(length(extract.fastq.gz.sample.names) == length(row.names(sample.table))){
        # check the number of sample(rows) in 'phenodata.csv' is same as the number of unique samples in 'fastq.gz.files'
        if(identical(sort(extract.fastq.gz.sample.names), sort(as.character(sample.table[1][,1])))){
          # check all the name in bath 'phenodata.csv' and 'fastq.gz.files' are same
          if (length(row.names(compare.sample.table)) == 2) {
            ## only two groups are allowed
            invalid.column.number <- 0
            adjustvars <- c()
            for( i in 1:length(pheno_data) ){
              if(length(pheno_data[i][is.na(pheno_data[i]) == TRUE]) != 0) {
                invalid.column.number <- invalid.column.number + 1
                cat(paste0("(\u2718) : There are missing values in column '", names(pheno_data[i]), "'\n" ))
              }
              if(i > 2){
                adjustvars <- append(adjustvars, names(pheno_data[i]))
              }
            }
            if (invalid.column.number != 0) {
              cat(paste0("(\u2718) : ", invalid.column.number, " columns are invalid. Please fix 'phenodata.csv'\n\n" ))
              adjustvars <- c()
            }
          }
        } else {
          cat(paste0("(\u2718) : Sample names in 'phenodata.csv' and 'XXX.fastq.gz' are different. Please check 'phenodata.csv' matches the 'XXX.fastq.gz'\n\n" ))
        }
      } else {
        cat(paste0("(\u2718) : The sample's numbers in 'phenodata.csv' and 'XXX.fastq.gz' are different. Please check 'phenodata.csv' matches the 'XXX.fastq.gz'\n\n" ))
      }
      if (isTRUE(gtf.file)) {
        if(print){
          cat(c("(\u2714) :", paste0("'",pkg.global.path.prefix$data_path, "gene_data/", '/phenodata.csv', "'"), "is exit\n\n"))
        }
      } else {
        cat(c("(\u2718) :", paste0("'",pkg.global.path.prefix$data_path, "gene_data/", '/phenodata.csv', "'"), "is not exit\n"))
        cat(c("     Put the", paste0("'phenodata.csv'"), "file in", paste0("'",pkg.global.path.prefix$data_path, "gene_data/", '/', "'"), "to fix the error.\n\n"))
      }
      ht2.files <- list.files(path = paste0(pkg.global.path.prefix$data_path, "gene_data/", '/indexes/'), pattern = paste0("^", gene.name, "_tran.[0-9]*.ht2$"), all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE)
      ht2.files.number <- length(ht2.files)
      if (ht2.files.number != 0) {
        if (print) {
          for (i in ht2.files){
            cat(c("(\u2714) :",paste0("'",pkg.global.path.prefix$data_path, "gene_data/", '/indexes/', i, "'"), "is exit\n"))
          }
          cat(c("Total:", ht2.files.number, "files\n\n"))
        }
      } else {
        cat(c("(\u231B) :", paste0('\'',pkg.global.path.prefix$data_path, "gene_data/", '/indexes/', gene.name, '_tran.*.ht2\''), "is not exit\n"))
        cat("       Files haven't created yet. Run 'CreateHisat2Index()' to generate '*.ht2' files or download from 'https://ccb.jhu.edu/software/hisat2/index.shtml'\n\n")
      }
      sam.files <- list.files(path = paste0(pkg.global.path.prefix$data_path, "gene_data/", '/raw_sam/'), pattern = paste0( "^[A-Z, a-z]*", "[0-9]*", "[A-Z, a-z]*", ".sam$"), all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE)
      sam.files.number <- length(sam.files)
      if (sam.files.number != 0){
        if (print) {
          for (i in sam.files){
            cat(c("(\u2714) :", paste0("'", pkg.global.path.prefix$data_path, "gene_data/", '/raw_sam/', i, "'"), "is exit\n"))
          }
          cat(c("Total:", sam.files.number, "files\n\n"))
        }
      }else {
        cat(c("(\u231B) :", paste0('\'', pkg.global.path.prefix$data_path, "gene_data/", '/raw_sam/XXX.sam\''), "is not exit\n"))
        cat("       Files haven't created yet. Run 'Hisat2AlignmentDefault()' to generate 'XXX.sam' files\n\n")
      }
      bam.files <- list.files(path = paste0(pkg.global.path.prefix$data_path, "gene_data/", '/raw_bam/'), pattern = paste0( "^[A-Z, a-z]*", "[0-9]*", "[A-Z, a-z]*", ".bam$"), all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE)
      bam.files.number <- length(bam.files)
      if (bam.files.number != 0){
        if (print) {
          for (i in bam.files){
            cat(c("(\u2714) :", paste0("'", pkg.global.path.prefix$data_path, "gene_data/", '/raw_bam/', i, "'"), "is exit\n"))
          }
          cat(c("Total:", bam.files.number, "files\n\n"))
        }
      }else {
        cat(c("(\u231B) :", paste0('\'', pkg.global.path.prefix$data_path, "gene_data/", '/raw_sam/XXX.bam\''), "is not exit\n"))
        cat("       Files haven't created yet. Run 'SamtoolsToBam()' to generate 'XXX.bam' files\n\n")
      }
      gtf.files <- list.files(path = paste0(pkg.global.path.prefix$data_path, "gene_data/", '/raw_gtf/'), pattern = paste0("^[A-Z, a-z]*", "[0-9]*", "[A-Z, a-z]*", ".gtf$"), all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE)
      gtf.files.number <- length(gtf.files)
      if (gtf.files.number != 0){
        if (print) {
          for (i in gtf.files){
            cat(c("(\u2714) :", paste0("'", pkg.global.path.prefix$data_path, "gene_data/", '/raw_gtf/', i, "'"), "is exit\n"))
          }
          cat(c("Total:", gtf.files.number, "files\n\n"))
        }
      }else {
        cat(c("(\u231B) :", paste0('\'',pkg.global.path.prefix$data_path, "gene_data/", '/raw_gtf/XXX.gtf\''), "is not exit\n"))
        cat("       Files haven't created yet. Run 'StringTieAssemble()' to generate 'XXX.gtf' files\n\n")
      }
      stringtie_merged.gtf.file <- file.exists(paste0(pkg.global.path.prefix$data_path, "gene_data/", '/merged/stringtie_merged.gtf'))
      if (isTRUE(stringtie_merged.gtf.file)) {
        if (print) {
          cat(c("(\u2714) :", paste0("'", pkg.global.path.prefix$data_path, "gene_data/", "/merged/stringtie_merged.gtf", "'"), "is exit\n\n"))
        }
      } else {
        cat(c("(\u231B) :", paste0("'", pkg.global.path.prefix$data_path, "gene_data/", "/merged/stringtie_merged.gtf", "'"), "is not exit\n"))
        cat("       Files haven't created yet. Run 'StringTieMergeTrans()' to generate 'stringtie_merged.gtf' files\n\n")
      }
      gffcompare.related.dirs <- list.files(path = paste0(pkg.global.path.prefix$data_path, "gene_data/", '/merged/'), pattern = "^merged.", all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE)
      gffcompare.related.dirs.number <- length(gffcompare.related.dirs)
      if (gffcompare.related.dirs.number != 0){
        if (print) {
          for (i in gffcompare.related.dirs){
            cat(c("(\u2714) :", paste0("'", pkg.global.path.prefix$data_path, "gene_data/", '/merged/', i, "'"), "is exit\n"))
          }
          cat(c("Total:", gffcompare.related.dirs.number, "files\n\n"))
        }
      }else {
        cat(c("(\u231B) :", paste0('\'', pkg.global.path.prefix$data_path, "gene_data/", '/merged/', "merged.", "XXX/"), "is not exit\n"))
        cat("       Directories haven't created yet. Run 'GffcompareRefSample()' to generate", paste0("merged/", "merged.XXX/"), "files\n\n")
      }
      ballgown.dirs <- list.files(path = paste0(pkg.global.path.prefix$data_path, "gene_data/", '/ballgown/'), pattern = sample.pattern, all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE)
      ballgown.dirs.number <- length(ballgown.dirs)
      if (ballgown.dirs.number != 0){
        if (print) {
          for (i in ballgown.dirs){
            cat(c("(\u2714) :", paste0("'", pkg.global.path.prefix$data_path, "gene_data/", '/ballgown/', i, "'"), "is exit\n"))
          }
          cat(c("Total:", ballgown.dirs.number, "directories\n\n"))
        }
      }else {
        cat(c("(\u231B) :", paste0('\'',pkg.global.path.prefix$data_path, "gene_data/", '/ballgown/', gsub(".fastq.gz", replace = "", sample.pattern), "/"), "is not exit\n"))
        cat("       Directories haven't created yet. Run 'StringTieToBallgown()' to generate", paste0("ballgown/", sample.pattern, "/"), "directories\n\n")
      }

      on.exit(setwd(current.path))
      return(list(gtf.file.logic.df = gtf.file, fa.file.logic.df = fa.file,
                  fastq.gz.files.number.df = fastq.gz.files.number,
                  fastq.gz.files.df = fastq.gz.files,
                  phenodata.file.df = phenodata.file,
                  phenodata.invalid.column.number.df = invalid.column.number,
                  phenodata.adjustvars.df = adjustvars,
                  ht2.files.number.df = ht2.files.number,
                  ht2.files.df = ht2.files,
                  sam.files.number.df = sam.files.number,
                  sam.files.df = sam.files,
                  bam.files.number.df = bam.files.number,
                  bam.files.df = bam.files,
                  gtf.files.number.df = gtf.files.number,
                  gtf.files.df = gtf.files,
                  stringtie_merged.gtf.file.df = stringtie_merged.gtf.file,
                  gffcompare.related.dirs.df = gffcompare.related.dirs,
                  gffcompare.related.dirs.number.df = gffcompare.related.dirs.number,
                  ballgown.dirs.number.df = ballgown.dirs.number,
                  ballgown.dirs.df = ballgown.dirs))
    }
  }
}
