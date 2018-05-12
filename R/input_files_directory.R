#' Copy input files directory
#' @export
CopyInputDir <- function(gene_name = "NO_DATA", abs_input_dir = "NOT_SET_YET") {
  if (isTRUE(CheckInputDir(gene_name, abs_input_dir,print=FALSE))) {
    if (isTRUE(CheckDirAll(print = TRUE))){
      current.path <- getwd()
      setwd(paste0(pkg.global.path.prefix$data_path, "gene_data/"))
      cat(c("************** Copying", paste0(pkg.global.path.prefix$input.files, "input_files/", gene_name, ".gtf"),  "************\n"))
      file.copy(paste0(pkg.global.path.prefix$input.files, "input_files/", gene_name, ".gtf"), paste0(getwd(), "/ref_genes/", gene_name, ".gtf"))
      cat(c("To :"), paste0(getwd(), "/ref_genes/\n"))
      cat(c("************** Copying", paste0(pkg.global.path.prefix$input.files, "input_files/", gene_name, ".fa"),  "************\n"))
      file.copy(paste0(pkg.global.path.prefix$input.files, "input_files/", gene_name, ".fa"), paste0(getwd(), "/ref_genome/", gene_name, ".fa"))
      cat(c("To :"), paste0(getwd(), "/ref_genome/\n"))
      cat(c("************** Copying", paste0(pkg.global.path.prefix$input.files, "input_files/", "raw_fastq.gz/"),  "************\n"))
      file.copy(paste0(pkg.global.path.prefix$input.files, "input_files/", "raw_fastq.gz/"), paste0(getwd(), "/"), overwrite = TRUE, recursive = TRUE)
      cat(c("To :", paste0(getwd()), "\n\n"))
      cat(c("************** Copying", paste0(pkg.global.path.prefix$input.files, "input_files/", gene_name, ".fa"),  "************\n"))
      file.copy(paste0(pkg.global.path.prefix$input.files, "input_files/phenodata.csv"), paste0(getwd(), "/phenodata.csv"))
      cat(c("To :"), paste0(getwd(), "/phenodata.csv\n"))
      on.exit(setwd(current.path))
    }
  }
}

#' Check input files directory
#' @export
CheckInputDir <- function(gene_name = "NO_DATA", abs_input_dir = "NOT_SET_YET", print=TRUE) {
  if (isTRUE(CheckPrefixPath(pkg.global.path.prefix$data_path, print = FALSE))){
    if (gene_name == "NO_DATA"){
      cat("(\u2718) :'gene_name' is missing.\n\n")
      return(FALSE)
    }
    if (abs_input_dir == "NOT_SET_YET") {
      cat("(\u2718) :Directory to 'input_files/' is missing.\n     Can't check input file directory.\n\n")
      return(FALSE)
    } else {
      if (substr(abs_input_dir, nchar(abs_input_dir), nchar(abs_input_dir)) != '/') {
        pkg.global.path.prefix$input.files <- paste0(abs_input_dir, '/')
      } else {
        pkg.global.path.prefix$input.files <- abs_input_dir
      }
      input.file.dir <- dir.exists(paste0(pkg.global.path.prefix$input.files, "input_files/"))
      if(!isTRUE(input.file.dir)){
        cat(c("(\u2718) :", paste0("'",pkg.global.path.prefix$input.files, "input_files/'"), "is not exit. Please check the prefix absolute path of input_files.\n\n"))
        return(FALSE)
      }
      cat(c("(\u2714) :", paste0("'",pkg.global.path.prefix$input.files, "'"), "is valid\n\n"))
      cat(c("************** Checking hierarchy of", paste0("'", pkg.global.path.prefix$input.files, 'input_files/\''), "************\n"))
      gtf.file <- file.exists(paste0(pkg.global.path.prefix$input.files, "input_files/",gene_name, ".gtf"))
      fa.file <- file.exists(paste0(pkg.global.path.prefix$input.files, "input_files/",gene_name, ".fa"))
      raw.fastq.dir <- dir.exists(paste0(pkg.global.path.prefix$input.files, "input_files/raw_fastq.gz/"))
      phenodata.file <- file.exists(paste0(pkg.global.path.prefix$input.files, "input_files/phenodata.csv"))
      if (isTRUE(raw.fastq.dir)) {
        raw.fastq <- list.files(path = paste0(pkg.global.path.prefix$input.files, 'input_files/raw_fastq.gz/'), pattern = paste0( "^[A-Z, a-z]*", "[0-9]*", "[A-Z, a-z]*_", "[1-2]*.fastq.gz$"), all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE)
        raw.fastq.number <- length(raw.fastq)
      }
      if (!isTRUE(gtf.file)) {
        cat(paste0("(\u2718) :'", gene_name, ".gtf'", " is missing.\n"))
      } else {
        cat(paste0("(\u2714) :'", gene_name, ".gtf'", " is in 'input_files'\n"))
      }
      if (!isTRUE(fa.file)) {
        cat(paste0("(\u2718) :'", gene_name, ".fa'", " is missing.\n"))
      } else {
        cat(paste0("(\u2714) :'", gene_name, ".fa'", " is in 'input_files'\n"))
      }
      if (!isTRUE(raw.fastq.dir)) {
        cat(c("(\u2718) :'raw_fastq.gz/' is missing.\n"))
      } else {
        cat(paste0("(\u2714) :'raw_fastq.gz/' is in 'input_files'\n"))
      }
      if (isTRUE(raw.fastq.dir) && raw.fastq.number == 0) {
        cat(c("(\u2718) :There are no samples in 'raw_fastq.gz/' or samples' names in 'raw_fastq.gz/' are incorrect.\n"))
      } else {
        for (i in raw.fastq) {
          cat(paste0("(\u2714) :'raw_fastq.gz/", i, "'"), "is in 'input_files'\n")
        }
      }
      if (!isTRUE(phenodata.file)) {
        cat(paste0("(\u2718) :'", "phenodata.csv is missing.\n"))
      } else {
        cat(paste0("(\u2714) :'", "phenodata.csv is in 'input_files'\n"))
      }
      if (isTRUE(gtf.file) && isTRUE(raw.fastq.dir) && isTRUE(raw.fastq.dir) && raw.fastq.number != 0 && isTRUE(phenodata.file)) {
        cat(c(paste0("\n(\u2714) :'", pkg.global.path.prefix$input.files,"input_files/", "'"), "is valid !\n\n"))
        return(TRUE)
      } else {
        cat("\n")
        return(FALSE)
      }
    }
  }
}
