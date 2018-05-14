#' Create sample gene directory
#' @export
MkdirGeneDir <- function() {
  if (isTRUE(CheckPrefixPath(pkg.global.path.prefix$data_path, print = FALSE))){
    cat("************** Creating 'gene-data/' directory ************\n")
    gene_data.dir <- dir.create(file.path(paste0(pkg.global.path.prefix$data_path, 'gene_data/')), showWarnings = FALSE) == 0
    if (!isTRUE(gene_data.dir)) {
      cat(paste0("(\u2714) :Create '", pkg.global.path.prefix$data_path, "gene_data/'.\n"))
    } else {
      cat(paste0("(\u2718) :Fail to create '", pkg.global.path.prefix$data_path, "gene_data/'.\n     Please check whether the directory is already exit.\n"))
    }
    ref.genome.dir <- dir.create(file.path(paste0(pkg.global.path.prefix$data_path, 'gene_data/ref_genome/')), showWarnings = FALSE) == 0
    if (!isTRUE(ref.genome.dir)) {
      cat(paste0("(\u2714) :Create '", pkg.global.path.prefix$data_path, "gene_data/ref_genome/'.\n"))
    } else {
      cat(paste0("(\u2718) :Fail to create '", pkg.global.path.prefix$data_path, "gene_data/ref_genome/'.\n     Please check whether the directory is already exit.\n"))
    }
    ref.genes.dir <- dir.create(file.path(paste0(pkg.global.path.prefix$data_path, 'gene_data/ref_genes/')), showWarnings = FALSE) == 0
    if (!isTRUE(ref.genes.dir)) {
      cat(paste0("(\u2714) :Create '", pkg.global.path.prefix$data_path, "gene_data/ref_genes/'.\n"))
    } else {
      cat(paste0("(\u2718) :Fail to create '", pkg.global.path.prefix$data_path, "gene_data/ref_genes/'.\n     Please check whether the directory is already exit.\n"))
    }
    indexes.dir <- dir.create(file.path(paste0(pkg.global.path.prefix$data_path, 'gene_data/indexes/')), showWarnings = FALSE) == 0
    if (!isTRUE(indexes.dir)) {
      cat(paste0("(\u2714) :Create '", pkg.global.path.prefix$data_path, "gene_data/indexes/'.\n"))
    } else {
      cat(paste0("(\u2718) :Fail to create '", pkg.global.path.prefix$data_path, "gene_data/indexes/'.\n     Please check whether the directory is already exit.\n"))
    }
    samples.fastq.dir <- dir.create(file.path(paste0(pkg.global.path.prefix$data_path, 'gene_data/raw_fastq.gz')), showWarnings = FALSE) == 0
    if (!isTRUE(samples.fastq.dir)) {
      cat(paste0("(\u2714) :Create '", pkg.global.path.prefix$data_path, "gene_data/raw_fastq.gz/'.\n"))
    } else {
      cat(paste0("(\u2718) :Fail to create '", pkg.global.path.prefix$data_path, "gene_data/raw_fastq.gz/'.\n     Please check whether the directory is already exit.\n"))
    }
    samples.sam.dir <- dir.create(file.path(paste0(pkg.global.path.prefix$data_path, 'gene_data/raw_sam')), showWarnings = FALSE) == 0
    if (!isTRUE(samples.sam.dir)) {
      cat(paste0("(\u2714) :Create '", pkg.global.path.prefix$data_path, "gene_data/raw_sam/'.\n"))
    } else {
      cat(paste0("(\u2718) :Fail to create '", pkg.global.path.prefix$data_path, "gene_data/raw_sam/'.\n     Please check whether the directory is already exit.\n"))
    }
    samples.bam.dir <- dir.create(file.path(paste0(pkg.global.path.prefix$data_path, 'gene_data/raw_bam')), showWarnings = FALSE) == 0
    if (!isTRUE(samples.bam.dir)) {
      cat(paste0("(\u2714) :Create '", pkg.global.path.prefix$data_path, "gene_data/raw_bam/'.\n"))
    } else {
      cat(paste0("(\u2718) :Fail to create '", pkg.global.path.prefix$data_path, "gene_data/raw_bam/'.\n     Please check whether the directory is already exit.\n"))
    }
    samples.gtf.dir <- dir.create(file.path(paste0(pkg.global.path.prefix$data_path, 'gene_data/raw_gtf')), showWarnings = FALSE) == 0
    if (!isTRUE(samples.gtf.dir)) {
      cat(paste0("(\u2714) :Create '", pkg.global.path.prefix$data_path, "gene_data/raw_gtf/'.\n\n"))
    } else {
      cat(paste0("(\u2718) :Fail to create '", pkg.global.path.prefix$data_path, "gene_data/raw_gtf/'.\n     Please check whether the directory is already exit.\n\n"))
    }
  }
}

#' Make RNAseq_bin/ directory
#' @export
MkdirRNAseq_bin <- function() {
  if (isTRUE(CheckPrefixPath(pkg.global.path.prefix$data_path, print = FALSE))){
    cat("************** Creating 'RNAseq_bin/' directory ************\n")
    RNAseq_bin.dir <- dir.create(file.path(paste0(pkg.global.path.prefix$data_path, 'RNAseq_bin/')), showWarnings = FALSE) == 0
    if (!isTRUE(RNAseq_bin.dir)) {
      cat(paste0("(\u2714) :Create '", pkg.global.path.prefix$data_path, "RNAseq_bin/'.\n"))
    } else {
      cat(paste0("(\u2718) :Fail to create '", pkg.global.path.prefix$data_path, "RNAseq_bin/'.\n     Please check whether the directory is already exit.\n"))
    }
    download.dir <- dir.create(file.path(paste0(pkg.global.path.prefix$data_path, 'RNAseq_bin/Download/')), showWarnings = FALSE) == 0
    if (!isTRUE(download.dir)) {
      cat(paste0("(\u2714) :Create '", pkg.global.path.prefix$data_path, "RNAseq_bin/Download/'.\n"))
    } else {
      cat(paste0("(\u2718) :Fail to create '", pkg.global.path.prefix$data_path, "RNAseq_bin/Download/'.\n     Please check whether the directory is already exit.\n"))
    }
    unpacked.dir <- dir.create(file.path(paste0(pkg.global.path.prefix$data_path, 'RNAseq_bin/Unpacked/')), showWarnings = FALSE) == 0
    if (!isTRUE(unpacked.dir)) {
      cat(paste0("(\u2714) :Create '", pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/'.\n\n"))
    } else {
      cat(paste0("(\u2718) :Fail to create '", pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/'.\n     Please check whether the directory is already exit.\n\n"))
    }
  }
}

#' Check sample gene and binary directory
#' @export
CheckDirAll <- function(print = TRUE) {
  if (print) {
    cat(c("************** Checking 'gene_data/' and 'RNAseq_bin' directories ************\n"))
  }
  if (isTRUE(CheckPrefixPath(pkg.global.path.prefix$data_path, print = FALSE))) {
    gene.data.logic <- dir.exists(paste0(pkg.global.path.prefix$data_path, "gene_data/ref_genes/")) &&
      dir.exists(paste0(pkg.global.path.prefix$data_path, "gene_data/ref_genome/")) &&
      dir.exists(paste0(pkg.global.path.prefix$data_path, "gene_data/indexes/")) &&
      dir.exists(paste0(pkg.global.path.prefix$data_path, "gene_data/raw_fastq.gz/")) &&
      dir.exists(paste0(pkg.global.path.prefix$data_path, "gene_data/raw_sam/")) &&
      dir.exists(paste0(pkg.global.path.prefix$data_path, "gene_data/raw_bam/")) &&
      dir.exists(paste0(pkg.global.path.prefix$data_path, "gene_data/raw_gtf/"))
    rnaseq.bin.logic <- dir.exists(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Download/")) && dir.exists(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/"))
    if (!isTRUE(gene.data.logic)){
      cat(c("There are directories missing in", paste0(pkg.global.path.prefix$data_path, "gene_data/.")), "\n")
    }
    if (!isTRUE(rnaseq.bin.logic)){
      cat(c("There are directories missing in", paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/.")), "\n")
    }
    if (isTRUE(gene.data.logic) && isTRUE(rnaseq.bin.logic)){
      if (print) {
        cat(c("(\u2714) :Directories are all correct.\n\n"))
      }
      return(TRUE)
    } else {
      cat(c("(\u2718) :Please run 'MkdirAll()' to add the missing directories.\n\n"))
      return(FALSE)
    }
  }
}

#' Create sample gene and binary directory
#' @export
MkdirAll <- function() {
  if (isTRUE(CheckPrefixPath(pkg.global.path.prefix$data_path, print = FALSE))) {
    MkdirGeneDir()
    MkdirRNAseq_bin()
  }
}
