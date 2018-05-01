#' Create sample gene directory
#' @export
MkdirSampleGeneDir <- function() {
  if (isTRUE(CheckPrefixPath(pkg.global.path.prefix$data_path, print = FALSE))){
    cat("************** Creating gene-data directory ************\n")
    gene_data.dir <- dir.create(file.path(paste0(pkg.global.path.prefix$data_path, 'gene_data/')), showWarnings = FALSE) == 0
    if (!isTRUE(gene_data.dir)) {
      cat(paste0("create '", pkg.global.path.prefix$data_path, "gene_data/'.\n"))
    } else {
      cat(paste0("fail to create '", pkg.global.path.prefix$data_path, "gene_data/'.\nPlease check whether the directory is exit or the 'path.prefix' is an absolute path.\n"))
    }
    genome.dir <- dir.create(file.path(paste0(pkg.global.path.prefix$data_path, 'gene_data/genome/')), showWarnings = FALSE) == 0
    if (!isTRUE(genome.dir)) {
      cat(paste0("create '", pkg.global.path.prefix$data_path, "gene_data/genome/'.\n"))
    } else {
      cat(paste0("fail to create '", pkg.global.path.prefix$data_path, "gene_data/genome/'.\nPlease check whether the directory is exit or the 'path.prefix' is an absolute path.\n"))
    }
    genes.dir <- dir.create(file.path(paste0(pkg.global.path.prefix$data_path, 'gene_data/genes/')), showWarnings = FALSE) == 0
    if (!isTRUE(genes.dir)) {
      cat(paste0("create '", pkg.global.path.prefix$data_path, "gene_data/genes/'.\n"))
    } else {
      cat(paste0("fail to create '", pkg.global.path.prefix$data_path, "gene_data/genes/'.\nPlease check whether the directory is exit or the 'path.prefix' is an absolute path.\n"))
    }
    indexes.dir <- dir.create(file.path(paste0(pkg.global.path.prefix$data_path, 'gene_data/indexes/')), showWarnings = FALSE) == 0
    if (!isTRUE(indexes.dir)) {
      cat(paste0("create '", pkg.global.path.prefix$data_path, "gene_data/indexes/'.\n"))
    } else {
      cat(paste0("fail to create '", pkg.global.path.prefix$data_path, "gene_data/indexes/'.\nPlease check whether the directory is exit or the 'path.prefix' is an absolute path.\n"))
    }
    samples.dir <- dir.create(file.path(paste0(pkg.global.path.prefix$data_path, 'gene_data/samples')), showWarnings = FALSE) == 0
    if (!isTRUE(samples.dir)) {
      cat(paste0("create '", pkg.global.path.prefix$data_path, "gene_data/samples/'.\n\n"))
    } else {
      cat(paste0("fail to create '", pkg.global.path.prefix$data_path, "gene_data/samples/'.\nPlease check whether the directory is exit or the 'path.prefix' is an absolute path.\n\n"))
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
      cat(paste0("create '", pkg.global.path.prefix$data_path, "RNAseq_bin/'.\n"))
    } else {
      cat(paste0("fail to create '", pkg.global.path.prefix$data_path, "RNAseq_bin/'.\nPlease check whether the directory is exit or the 'path.prefix' is an absolute path.\n"))
    }
    download.dir <- dir.create(file.path(paste0(pkg.global.path.prefix$data_path, 'RNAseq_bin/Download/')), showWarnings = FALSE) == 0
    if (!isTRUE(download.dir)) {
      cat(paste0("create '", pkg.global.path.prefix$data_path, "RNAseq_bin/Download/'.\n"))
    } else {
      cat(paste0("fail to create '", pkg.global.path.prefix$data_path, "RNAseq_bin/Download/'.\nPlease check whether the directory is exit or the 'path.prefix' is an absolute path.\n"))
    }
    unpacked.dir <- dir.create(file.path(paste0(pkg.global.path.prefix$data_path, 'RNAseq_bin/Unpacked/')), showWarnings = FALSE) == 0
    if (!isTRUE(unpacked.dir)) {
      cat(paste0("create '", pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/'.\n\n"))
    } else {
      cat(paste0("fail to create '", pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/'.\nPlease check whether the directory is exit or the 'path.prefix' is an absolute path.\n\n"))
    }
  }
}

#' Create sample gene and binary directory
#' @export
MkdirAll <- function() {
  MkdirSampleGeneDir()
  MkdirRNAseq_bin()
  ExportPath()
}
