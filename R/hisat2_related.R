#' Creating Hisat2 index
#' Creat Hisat2 index for further use
#' @export
CreateHisat2Index <- function (splice.site.info = TRUE, exon.info = TRUE) {
  ## need to change 'Let the user can choose where to put their files'
  ## need to learn how to get the filenames under a directory
  ## need to use 'real filename' rather than the 'chrX'
  if (isTRUE(CheckPrefixPath(pkg.global.path.prefix$data_path, print = FALSE))) {
    if (isTRUE(CheckDirAll())){
      if (!is.logical(splice.site.info) || !is.logical(exon.info)) {
        cat("Please make sure the type of 'splice.site.info' and 'exon.info' are logical.\n")
      } else {
        current.path <- getwd()
        setwd(paste0(pkg.global.path.prefix$data_path, "gene_data/indexes/"))
        if (isTRUE(splice.site.info)) {
          system2(command = 'extract_splice_sites.py', args = c(paste0(pkg.global.path.prefix$data_path, 'gene_data/genes/chrX.gtf'), '>', 'chrX.ss'))
        }
        if (isTRUE(exon.info)) {
          system2(command = 'extract_exons.py', args = c(paste0(pkg.global.path.prefix$data_path, 'gene_data/genes/chrX.gtf'), '>', 'chrX.exon'))
        }

        if (isTRUE(splice.site.info) && isTRUE(exon.info)) {
          system2(command = 'hisat2-build', args = c('--ss', 'chrX.ss', '--exon', 'chrX.exon', paste0(pkg.global.path.prefix$data_path, 'gene_data/genome/chrX.fa'), 'chrX_tran'))
        } else if (isTRUE(splice.site.info) && !isTRUE(exon.info)) {
          system2(command = 'hisat2-build', args = c('--ss', 'chrX.ss', paste0(pkg.global.path.prefix$data_path, 'gene_data/genome/chrX.fa'), 'chrX_tran'))
        } else if (!isTRUE(splice.site.info) && isTRUE(exon.info)) {
          system2(command = 'hisat2-build', args = c('--exon', 'chrX.exon', paste0(pkg.global.path.prefix$data_path, 'gene_data/genome/chrX.fa'), 'chrX_tran'))
        } else {
          system2(command = 'hisat2-build', args = c(paste0(pkg.global.path.prefix$data_path, 'gene_data/genome/chrX.fa'), 'chrX_tran'))
        }
        on.exit(setwd(current.path))
        cat(paste0("'", pkg.global.path.prefix$data_path, "gene_data/indexes/chrX_tran.*.ht2' has been created.\n\n"))
      }
    }
  }
}
