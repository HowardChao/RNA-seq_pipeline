#' stringtie assemble and quantify expressed genes and transcripts
#' @export
StringTieAssemble <- function(gene_name = "NO_DATA") {
  if (isTRUE(CheckPrefixPath(pkg.global.path.prefix$data_path, print = FALSE))) {
    if (isTRUE(CheckDirAll(print = FALSE))){
      if (gene_name == "NO_DATA"){
        cat("(X) :gene_name is missing.\n     Can't find the target sample files to assemble\n\n")
      } else {
        check.results <- CheckSampleGenesFiles(gene_name)
        if (check.results$bam.files.number.df != 0){
          current.path <- getwd()
          setwd(paste0(pkg.global.path.prefix$data_path, "gene_data/"))
          sample.table <- table(gsub(paste0(gene_name, ".bam$"), replace = "", check.results$bam.files.df))
          iteration.num <- length(sample.table)
          sample.name <- names(sample.table)
          sample.value <- as.vector(sample.table)

          for( i in 1:iteration.num){
            whole.command <- paste("-p 8 -G",paste0("ref_genes/", gene_name, ".gtf"), "-o", paste0("samples_.gtf/", sample.name[i], gene_name, ".gtf"), "-l", gsub("_$", replace = "", sample.name[i]), paste0("samples_.bam/", sample.name[i], gene_name, ".bam"))
            cat(c("Input command :", paste("stringtie", whole.command), "\n"))
            system2(command = "stringtie", args = whole.command)
          }
          on.exit(setwd(current.path))
        }
      }
    }
  }
}

#' stringtie merge transcripts from all samples
#' @export
StringTieMergeTrans <- function(gene_name = "NO_DATA") {
  if (isTRUE(CheckPrefixPath(pkg.global.path.prefix$data_path, print = FALSE))) {
    if (isTRUE(CheckDirAll(print = FALSE))){
      if (gene_name == "NO_DATA"){
        cat("(X) :gene_name is missing.\n     Can't find the target sample files to merge\n\n")
      } else{
        check.results <- CheckSampleGenesFiles(gene_name)
        if (check.results$gtf.files.number.df != 0){
          current.path <- getwd()
          setwd(paste0(pkg.global.path.prefix$data_path, "gene_data/"))
          dir.create(file.path(paste0(pkg.global.path.prefix$data_path, 'gene_data/merged_VS_ref')), showWarnings = FALSE)
          sample.table <- table(check.results$gtf.files.df)
          iteration.num <- length(sample.table)
          sample.name <- names(sample.table)
          sample.value <- as.vector(sample.table)
          write.content <- paste0("samples_.gtf/",sample.name[1])
          for (i in 2:iteration.num){
            write.content <- c(write.content, paste0("samples_.gtf/" ,sample.name[i]))
          }
          write.file<-file("merged_VS_ref/mergelist.txt")
          writeLines(write.content, write.file)
          close(write.file)
          whole.command <- paste("--merge -p 8 -G", paste0("ref_genes/", gene_name, ".gtf"), "-o", "merged_VS_ref/stringtie_merged.gtf", "merged_VS_ref/mergelist.txt")
          cat(c("Input command :", paste("stringtie", whole.command), "\n"))
          system2(command = "stringtie", args = whole.command)
          on.exit(setwd(current.path))
        }
      }
    }
  }
}

#' stringtie estimate transcript abundances and create table counts for Ballgown
#' @export
StringTieToBallgown <- function(gene_name = "NO_DATA") {
  if (isTRUE(CheckPrefixPath(pkg.global.path.prefix$data_path, print = FALSE))) {
    if (isTRUE(CheckDirAll(print = FALSE))){
      if (gene_name == "NO_DATA"){
        cat("(X) :gene_name is missing.\n     Can't find the target sample files to merge\n\n")
      } else{
        check.results <- CheckSampleGenesFiles(gene_name)
        if ((check.results$bam.files.number.df != 0) && isTRUE(check.results$stringtie_merged.gtf.file.df)){
          current.path <- getwd()
          setwd(paste0(pkg.global.path.prefix$data_path, "gene_data/"))
          sample.table <- table(gsub(paste0("_",gene_name, ".bam$"), replace = "", check.results$bam.files.df))
          iteration.num <- length(sample.table)
          sample.name <- names(sample.table)
          sample.value <- as.vector(sample.table)
          for( i in 1:iteration.num){
            whole.command <- paste("-e -B -p 8 -G", "merged_VS_ref/stringtie_merged.gtf", "-o", paste0("ballgown/", sample.name[i],"/", sample.name[i], "_", gene_name, ".gtf"), paste0("samples_.bam/", sample.name[i], "_", gene_name, ".bam"))
            cat(c("Input command :", paste("stringtie", whole.command), "\n"))
            system2(command = "stringtie", args = whole.command)
          }
          on.exit(setwd(current.path))
        }
      }
    }
  }
}
