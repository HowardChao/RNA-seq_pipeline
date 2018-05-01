#' hisat2 alignment default
#' @export
Hisat2AlignmentDefault <- function(gene_name = "NO_DATA") {
  if (isTRUE(CheckPrefixPath(pkg.global.path.prefix$data_path, print = FALSE))) {
    if (gene_name == "NO_DATA"){
      cat("gene_name is missing.\n\n")
    } else {
      current.path <- getwd()
      setwd(paste0(pkg.global.path.prefix$data_path, "gene_data/"))
      cat(paste0("************** Checking validity of gene data in '", paste0(pkg.global.path.prefix$data_path, "gene_data/'"), " **************\n"))
      gtf.file <- file.exists(paste0(getwd(), '/genes/', gene_name, '.gtf'))
      if (isTRUE(gtf.file)) {
        cat(c("Valid :", paste0(getwd(), '/genes/', gene_name, '.gtf'), "is exit\n\n"))
      } else {
        cat(c("Invalid :", paste0(getwd(), '/genes/', gene_name, '.gtf'), "is not exit\n\n"))
      }
      fa.file <- file.exists(paste0(getwd(), '/genome/', gene_name, '.fa'))
      if (isTRUE(fa.file)) {
        cat(c("Valid :",paste0(getwd(), '/genome/', gene_name, '.fa'), "is exit\n\n"))
      } else {
        cat(c("Invalid :",paste0(getwd(), '/genome/', gene_name, '.fa'), "is not exit\n\n"))
      }
      ht2.files <- list.files(path =paste0(getwd(), '/indexes/'), pattern = paste0(gene_name, "_tran.*.ht2"), all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE)
      ht2.files.number <- length(ht2.files)
      if (ht2.files.number != 0) {
        for (i in ht2.files){
          cat(c("Valid :",paste0(getwd(), '/indexes/', i), "is exit\n"))
        }
        cat(c("Total:", ht2.files.number, "files\n\n"))
      } else {
        cat(c("InValid :", paste0('\'',getwd(), '/indexes/', gene_name, '.*.ht2\''), "is not exit\n\n"))
      }

      on.exit(setwd(current.path))
    }
  }
}

#' check 'gene_data' and subdirectory files exit
#' @export
CheckSampleData <- function() {

}
