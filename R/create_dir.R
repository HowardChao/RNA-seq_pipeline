#' Create sample gene directory
#' @export
MkdirGeneDir <- function() {
  if (isTRUE(CheckPrefixPath(pkg.global.path.prefix$data_path, print = FALSE))){
    cat("************** Creating gene-data directory ************\n")
    gene_data.dir <- dir.create(file.path(paste0(pkg.global.path.prefix$data_path, 'gene_data/')), showWarnings = FALSE) == 0
    if (!isTRUE(gene_data.dir)) {
      cat(paste0("(O) :Create '", pkg.global.path.prefix$data_path, "gene_data/'.\n"))
    } else {
      cat(paste0("(X) :Fail to create '", pkg.global.path.prefix$data_path, "gene_data/'.\n     Please check whether the directory is already exit.\n"))
    }
    ref.genome.dir <- dir.create(file.path(paste0(pkg.global.path.prefix$data_path, 'gene_data/ref_genome/')), showWarnings = FALSE) == 0
    if (!isTRUE(ref.genome.dir)) {
      cat(paste0("(O) :Create '", pkg.global.path.prefix$data_path, "gene_data/ref_genome/'.\n"))
    } else {
      cat(paste0("(X) :Fail to create '", pkg.global.path.prefix$data_path, "gene_data/ref_genome/'.\n     Please check whether the directory is already exit.\n"))
    }
    ref.genes.dir <- dir.create(file.path(paste0(pkg.global.path.prefix$data_path, 'gene_data/ref_genes/')), showWarnings = FALSE) == 0
    if (!isTRUE(ref.genes.dir)) {
      cat(paste0("(O) :Create '", pkg.global.path.prefix$data_path, "gene_data/ref_genes/'.\n"))
    } else {
      cat(paste0("(X) :Fail to create '", pkg.global.path.prefix$data_path, "gene_data/ref_genes/'.\n     Please check whether the directory is already exit.\n"))
    }
    indexes.dir <- dir.create(file.path(paste0(pkg.global.path.prefix$data_path, 'gene_data/indexes/')), showWarnings = FALSE) == 0
    if (!isTRUE(indexes.dir)) {
      cat(paste0("(O) :Create '", pkg.global.path.prefix$data_path, "gene_data/indexes/'.\n"))
    } else {
      cat(paste0("(X) :Fail to create '", pkg.global.path.prefix$data_path, "gene_data/indexes/'.\n     Please check whether the directory is already exit.\n"))
    }
    samples.fastq.dir <- dir.create(file.path(paste0(pkg.global.path.prefix$data_path, 'gene_data/samples_.fastq.gz')), showWarnings = FALSE) == 0
    if (!isTRUE(samples.fastq.dir)) {
      cat(paste0("(O) :Create '", pkg.global.path.prefix$data_path, "gene_data/samples_.fastq.gz/'.\n"))
    } else {
      cat(paste0("(X) :Fail to create '", pkg.global.path.prefix$data_path, "gene_data/samples_.fastq.gz/'.\n     Please check whether the directory is already exit.\n"))
    }
    samples.sam.dir <- dir.create(file.path(paste0(pkg.global.path.prefix$data_path, 'gene_data/samples_.sam')), showWarnings = FALSE) == 0
    if (!isTRUE(samples.sam.dir)) {
      cat(paste0("(O) :Create '", pkg.global.path.prefix$data_path, "gene_data/samples_.sam/'.\n"))
    } else {
      cat(paste0("(X) :Fail to create '", pkg.global.path.prefix$data_path, "gene_data/samples_.sam/'.\n     Please check whether the directory is already exit.\n"))
    }
    samples.bam.dir <- dir.create(file.path(paste0(pkg.global.path.prefix$data_path, 'gene_data/samples_.bam')), showWarnings = FALSE) == 0
    if (!isTRUE(samples.bam.dir)) {
      cat(paste0("(O) :Create '", pkg.global.path.prefix$data_path, "gene_data/samples_.bam/'.\n"))
    } else {
      cat(paste0("(X) :Fail to create '", pkg.global.path.prefix$data_path, "gene_data/samples_.bam/'.\n     Please check whether the directory is already exit.\n"))
    }
    samples.gtf.dir <- dir.create(file.path(paste0(pkg.global.path.prefix$data_path, 'gene_data/samples_.gtf')), showWarnings = FALSE) == 0
    if (!isTRUE(samples.gtf.dir)) {
      cat(paste0("(O) :Create '", pkg.global.path.prefix$data_path, "gene_data/samples_.gtf/'.\n\n"))
    } else {
      cat(paste0("(X) :Fail to create '", pkg.global.path.prefix$data_path, "gene_data/samples_.gtf/'.\n     Please check whether the directory is already exit.\n\n"))
    }
  }
}

#' Make RNAseq_bin/ directory
#' @export
MkdirRNAseq_bin <- function() {
  if (isTRUE(CheckPrefixPath(pkg.global.path.prefix$data_path, print = FALSE))){
    cat("************** Creating binary directory ************\n")
    RNAseq_bin.dir <- dir.create(file.path(paste0(pkg.global.path.prefix$data_path, 'RNAseq_bin/')), showWarnings = FALSE) == 0
    if (!isTRUE(RNAseq_bin.dir)) {
      cat(paste0("(O) :Create '", pkg.global.path.prefix$data_path, "RNAseq_bin/'.\n"))
    } else {
      cat(paste0("(X) :Fail to create '", pkg.global.path.prefix$data_path, "RNAseq_bin/'.\n     Please check whether the directory is already exit.\n"))
    }
    download.dir <- dir.create(file.path(paste0(pkg.global.path.prefix$data_path, 'RNAseq_bin/Download/')), showWarnings = FALSE) == 0
    if (!isTRUE(download.dir)) {
      cat(paste0("(O) :Create '", pkg.global.path.prefix$data_path, "RNAseq_bin/Download/'.\n"))
    } else {
      cat(paste0("(X) :Fail to create '", pkg.global.path.prefix$data_path, "RNAseq_bin/Download/'.\n     Please check whether the directory is already exit.\n"))
    }
    unpacked.dir <- dir.create(file.path(paste0(pkg.global.path.prefix$data_path, 'RNAseq_bin/Unpacked/')), showWarnings = FALSE) == 0
    if (!isTRUE(unpacked.dir)) {
      cat(paste0("(O) :Create '", pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/'.\n\n"))
    } else {
      cat(paste0("(X) :Fail to create '", pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/'.\n     Please check whether the directory is already exit.\n\n"))
    }
  }
}

#' Check sample gene and binary directory
#' @export
CheckDirAll <- function(print = TRUE) {
  if (isTRUE(CheckPrefixPath(pkg.global.path.prefix$data_path, print = FALSE))) {
    gene.data.logic <- dir.exists(paste0(pkg.global.path.prefix$data_path, "gene_data/ref_genes/")) &&
      dir.exists(paste0(pkg.global.path.prefix$data_path, "gene_data/ref_genome/")) &&
      dir.exists(paste0(pkg.global.path.prefix$data_path, "gene_data/indexes/")) &&
      dir.exists(paste0(pkg.global.path.prefix$data_path, "gene_data/samples_.fastq.gz/")) &&
      dir.exists(paste0(pkg.global.path.prefix$data_path, "gene_data/samples_.sam/")) &&
      dir.exists(paste0(pkg.global.path.prefix$data_path, "gene_data/samples_.bam/")) &&
      dir.exists(paste0(pkg.global.path.prefix$data_path, "gene_data/samples_.gtf/"))
    rnaseq.bin.logic <- dir.exists(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Download/")) && dir.exists(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/"))

    if (!isTRUE(gene.data.logic)){
      cat(c("There are directories missing in", paste0(pkg.global.path.prefix$data_path, "gene_data/.")), "\n")
    }
    if (!isTRUE(rnaseq.bin.logic)){
      cat(c("There are directories missing in", paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/.")), "\n")
    }
    if (print){
      if (isTRUE(gene.data.logic) && isTRUE(rnaseq.bin.logic)){
        cat(c("(O) :Directories are all correct.\n\n"))
        return(TRUE)
      } else {
        cat(c("(X) :Please run 'MkdirAll()' to add the missing directories.\n\n"))
        return(FALSE)
      }
    } else{
      if (isTRUE(gene.data.logic) && isTRUE(rnaseq.bin.logic)){
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
  }
}

#' Create sample gene and binary directory
#' @export
MkdirAll <- function() {
  if (isTRUE(CheckPrefixPath(pkg.global.path.prefix$data_path, print = FALSE))) {
    MkdirGeneDir()
    MkdirRNAseq_bin()
    ExportPath()
  }
}
