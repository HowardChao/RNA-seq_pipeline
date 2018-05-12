#' stringtie assemble and quantify expressed genes and transcripts
#' @export
StringTieAssemble <- function(gene_name = "NO_DATA") {
  if (isTRUE(CheckStringTie(print=FALSE))) {
    if (isTRUE(CheckDirAll(print = FALSE))) {
      if (gene_name == "NO_DATA"){
        cat("(\u2718) :gene_name is missing.\n     Can't find the target sample files to assemble\n\n")
      } else {
        check.results <- ProgressGenesFiles(gene_name, print=TRUE)
        cat(paste0("\n************** Stringtie assembling **************\n"))
        if (check.results$bam.files.number.df != 0 && isTRUE(check.results$gtf.file.logic.df)){
          current.path <- getwd()
          setwd(paste0(pkg.global.path.prefix$data_path, "gene_data/"))
          sample.table <- table(gsub(paste0(".bam$"), replace = "", check.results$bam.files.df))
          iteration.num <- length(sample.table)
          sample.name <- names(sample.table)
          sample.value <- as.vector(sample.table)

          for( i in 1:iteration.num){
            whole.command <- paste("-p 8 -G",paste0("ref_genes/", gene_name, ".gtf"), "-o", paste0("raw_gtf/", sample.name[i], ".gtf"), "-l", sample.name[i], paste0("raw_bam/", sample.name[i], ".bam"))
            cat(c("Input command :", paste("stringtie", whole.command), "\n"))
            system2(command = "stringtie", args = whole.command)
          }
          on.exit(setwd(current.path))
        } else {
          cat(c(paste0("(\u2718) :'", gene_name, ".gtf'"), "or 'XXX.bam' is missing.\n\n"))
        }
      }
    }
  }
}

#' stringtie merge transcripts from all samples
#' @export
StringTieMergeTrans <- function(gene_name = "NO_DATA") {
  if (isTRUE(CheckStringTie(print=FALSE))) {
    if (isTRUE(CheckDirAll(print = FALSE))){
      if (gene_name == "NO_DATA"){
        cat("(\u2718) :gene_name is missing.\n     Can't find the target sample files to merge\n\n")
      } else{
        check.results <- ProgressGenesFiles(gene_name, print=TRUE)
        cat(paste0("\n************** Stringtie merging transcripts **************\n"))
        if ( isTRUE(check.results$gtf.file.logic.df) && check.results$gtf.files.number.df != 0){
          current.path <- getwd()
          setwd(paste0(pkg.global.path.prefix$data_path, "gene_data/"))
          dir.create(file.path(paste0(pkg.global.path.prefix$data_path, 'gene_data/merged/')), showWarnings = FALSE)
          sample.table <- table(check.results$gtf.files.df)
          iteration.num <- length(sample.table)
          sample.name <- names(sample.table)
          sample.value <- as.vector(sample.table)
          write.content <- paste0("raw_gtf/",sample.name[1])
          for (i in 2:iteration.num){
            write.content <- c(write.content, paste0("raw_gtf/" ,sample.name[i]))
          }
          write.file<-file("merged/mergelist.txt")
          writeLines(write.content, write.file)
          close(write.file)
          whole.command <- paste("--merge -p 8 -G", paste0("ref_genes/", gene_name, ".gtf"), "-o", "merged/stringtie_merged.gtf", "merged/mergelist.txt")
          cat(c("Input command :", paste("stringtie", whole.command), "\n"))
          system2(command = "stringtie", args = whole.command)
          on.exit(setwd(current.path))
        } else {
          cat(c(paste0("(\u2718) :'", gene_name, ".gtf'"), "or", "'XXX.gtf' is missing.\n\n"))
        }
      }
    }
  }
}

#' stringtie estimate transcript abundances and create table counts for Ballgown
#' @export
StringTieToBallgown <- function(gene_name = "NO_DATA") {
  if (isTRUE(CheckStringTie(print=FALSE))) {
    if (isTRUE(CheckDirAll(print = FALSE))){
      if (gene_name == "NO_DATA"){
        cat("(\u2718) :gene_name is missing.\n     Can't find the target sample files to merge\n\n")
      } else{
        check.results <- ProgressGenesFiles(gene_name, print=TRUE)
        cat(paste0("\n************** Stringtie creating table counts for Ballgown **************\n"))
        if ((check.results$bam.files.number.df != 0) && isTRUE(check.results$stringtie_merged.gtf.file.df)){
          current.path <- getwd()
          setwd(paste0(pkg.global.path.prefix$data_path, "gene_data/"))
          sample.table <- table(gsub(paste0(".bam$"), replace = "", check.results$bam.files.df))
          iteration.num <- length(sample.table)
          sample.name <- names(sample.table)
          sample.value <- as.vector(sample.table)
          for( i in 1:iteration.num){
            whole.command <- paste("-e -B -p 8 -G", "merged/stringtie_merged.gtf", "-o", paste0("ballgown/", sample.name[i],"/", sample.name[i], ".gtf"), paste0("raw_bam/", sample.name[i], ".bam"))
            cat(c("Input command :", paste("stringtie", whole.command), "\n"))
            system2(command = "stringtie", args = whole.command)
          }
          on.exit(setwd(current.path))
        } else {
          cat(c(paste0("(\u2718) :'stringtie_merged.gtf'"), "or", "'XXX.bam' is missing.\n\n"))
        }
      }
    }
  }
}
