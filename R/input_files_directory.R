pkg.global.ht2 <- new.env()
pkg.global.ht2$logic <- FALSE

#' Check input files directory
#' @export
CheckInputDirFiles <- function(input.path.prefix = "NOT_SET_YET", gene.name = "NO_DATA", sample.pattern = "NO_DATA", print=TRUE) {
  if (isTRUE(CheckPrefixPath(pkg.global.path.prefix$data_path, print = FALSE))){
    if (gene.name == "NO_DATA" || sample.pattern == "NO_DATA"){
      if (gene.name == "NO_DATA") {
        cat("(\u2718) :gene.name is missing.\n\n")
      }
      if (sample.pattern == "NO_DATA") {
        cat("(\u2718) :sample.pattern is missing.\n\n")
      }
      return(FALSE)
    }
    if (input.path.prefix == "NOT_SET_YET") {
      cat("(\u2718) : Directory to 'input_files/' is missing.\n     Can't check input file directory.\n\n")
      return(FALSE)
    } else {
      if (substr(input.path.prefix, nchar(input.path.prefix), nchar(input.path.prefix)) != '/') {
        pkg.global.path.prefix$input.files <- paste0(input.path.prefix, '/')
      } else {
        pkg.global.path.prefix$input.files <- input.path.prefix
      }
      input.file.dir <- dir.exists(paste0(pkg.global.path.prefix$input.files, "input_files/"))
      if(!isTRUE(input.file.dir)){
        cat(c("(\u2718) :", paste0("'",pkg.global.path.prefix$input.files, "input_files/'"), "is not exit. Please check the prefix absolute path of input_files.\n\n"))
        return(FALSE)
      }
      if (print) {
        cat(c("************** Checking hierarchy of", paste0("'", pkg.global.path.prefix$input.files, 'input_files/\''), "************\n"))
        cat(c("(\u2714) : Prefix of input_files", paste0("'",pkg.global.path.prefix$input.files, "'"), "is valid\n\n"))
      }
      gtf.file <- file.exists(paste0(pkg.global.path.prefix$input.files, "input_files/",gene.name, ".gtf"))
      fa.file <- file.exists(paste0(pkg.global.path.prefix$input.files, "input_files/",gene.name, ".fa"))
      raw.fastq.dir <- dir.exists(paste0(pkg.global.path.prefix$input.files, "input_files/raw_fastq.gz/"))
      phenodata.file <- file.exists(paste0(pkg.global.path.prefix$input.files, "input_files/phenodata.csv"))
      ht2.dir <- dir.exists(paste0(pkg.global.path.prefix$input.files, "input_files/indexes/"))
      if (isTRUE(raw.fastq.dir)) {
        raw.fastq <- list.files(path = paste0(pkg.global.path.prefix$input.files, 'input_files/raw_fastq.gz/'), pattern = paste0( sample.pattern, "[0-9]*", "_", "[1-2]*.fastq.gz$"), all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE)
        raw.fastq.number <- length(raw.fastq)
      }
      if (isTRUE(ht2.dir)) {
        ht2.files <- list.files(path = paste0(pkg.global.path.prefix$input.files, 'input_files/indexes/'), pattern = paste0("^", gene.name, "_tran.[0-9]*.ht2$"), all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE)
        ht2.files.number <- length(ht2.files)
      }
      if (!isTRUE(gtf.file)) {
        cat(paste0("(\u2718) : '", gene.name, ".gtf'", " is missing.\n"))
      } else {
        if (print) {
          cat(paste0("(\u2714) : '", gene.name, ".gtf'", " is in 'input_files'\n"))
        }
      }
      if (!isTRUE(fa.file)) {
        cat(paste0("(\u2718) : '", gene.name, ".fa'", " is missing.\n"))
      } else {
        if (print) {
          cat(paste0("(\u2714) : '", gene.name, ".fa'", " is in 'input_files'\n"))
        }
      }
      if (!isTRUE(raw.fastq.dir)) {
        cat(c("(\u2718) : 'raw_fastq.gz/' is missing.\n"))
      } else {
        if (print) {
          cat(paste0("(\u2714) : 'raw_fastq.gz/' is in 'input_files'\n"))
        }
      }
      if (isTRUE(raw.fastq.dir) && raw.fastq.number == 0) {
        cat(c("(\u2718) : There are no samples in 'raw_fastq.gz/' or samples' names in 'raw_fastq.gz/' are incorrect.\n"))
      } else if (isTRUE(raw.fastq.dir) && raw.fastq.number >= 0) {
        if (print){
          for (i in raw.fastq) {
            cat(paste0("(\u2714) : 'raw_fastq.gz/", i, "'"), "is in 'input_files/'\n")
          }
        }
      }
      if (!isTRUE(phenodata.file)) {
        cat(paste0("(\u2718) : '", "phenodata.csv is missing.\n"))
      } else {
        if (print) {
          cat(paste0("(\u2714) : '", "phenodata.csv is in 'input_files'\n"))
        }
      }


      if (!isTRUE(ht2.dir)) {
        ## not exist
        if (print) {
          cat(c("(\u26A0) : 'indexes/' is optional. You can download the corresponding 'XXX.ht2' from 'https://ccb.jhu.edu/software/hisat2/index.shtml' to speed up the process.\n"))
        }
      } else {
        if (print) {
          ## exist
          cat(paste0("(\u2714) : 'indexes/' is in 'input_files'\n"))
        }
      }
      if (isTRUE(ht2.dir) && ht2.files.number == 0) {
        cat(c("(\u26A0) : 'indexes/' directory has been created but there are no samples in 'indexes/' or files' names in 'indexes/' are incorrect.\n     No files will be copied.\n     (1). Make sure files name are", paste0("'", gene.name, "_tran.[0-9].ht2'"), "\n     (2). If you don't have", paste0("'", gene.name, "_tran.[0-9].ht2'"), "files, remove 'indexes' directory\n\n"))
        return(FALSE)
      } else if (isTRUE(ht2.dir) && ht2.files.number >= 0) {
        pkg.global.ht2$logic <- TRUE
        if (print){
          for (i in ht2.files) {
            cat(paste0("(\u2714) : 'indexes/", i, "'"), "is in 'input_files/'\n")
          }
        }
      }
      if (isTRUE(gtf.file) && isTRUE(raw.fastq.dir) && isTRUE(raw.fastq.dir) && raw.fastq.number != 0 && isTRUE(phenodata.file)) {
        if (print) {
          cat(c(paste0("\n(\u2714) : '", pkg.global.path.prefix$input.files,"input_files/", "'"), "is valid !\n"))
          if (isTRUE(ht2.dir) && ht2.files.number != 0) {
            cat(paste0("(\u2714) : optional directory 'indexes/' is valid !\n\n"))
          }
        }
        return(TRUE)
      } else {
        cat("\n")
        return(FALSE)
      }
    }
  }
}

#' Copy input files directory
#' @export
CopyInputDir <- function(input.path.prefix = "NOT_SET_YET", gene.name = "NO_DATA", sample.pattern = "NO_DATA", optional = pkg.global.ht2$logic) {
    if (isTRUE(CheckInputDirFiles(input.path.prefix = input.path.prefix, gene.name = gene.name, sample.pattern = sample.pattern, print=FALSE))) {
    if (isTRUE(CheckDirAll(print = FALSE))){
      current.path <- getwd()
      setwd(paste0(pkg.global.path.prefix$data_path, "gene_data/"))
      cat(c("************** Copying", paste0("'", pkg.global.path.prefix$input.files, "input_files/"), "************\n"))
      cat(c("Copying From :", paste0(pkg.global.path.prefix$input.files, "input_files/", gene.name, ".gtf"), "\n"))
      file.copy(paste0(pkg.global.path.prefix$input.files, "input_files/", gene.name, ".gtf"), paste0(getwd(), "/ref_genes/", gene.name, ".gtf"))
      cat(c("          To :"), paste0(getwd(), "/ref_genes/", gene.name, ".gtf", "\n"))
      cat(c("Copying From :", paste0(pkg.global.path.prefix$input.files, "input_files/", gene.name, ".fa"),  "\n"))
      file.copy(paste0(pkg.global.path.prefix$input.files, "input_files/", gene.name, ".fa"), paste0(getwd(), "/ref_genome/", gene.name, ".fa"))
      cat(c("          To :"), paste0(getwd(), "/ref_genome/", gene.name, ".fa", "\n"))
      cat(c("Copying From :", paste0(pkg.global.path.prefix$input.files, "input_files/", "raw_fastq.gz/"),  "\n"))
      file.copy(paste0(pkg.global.path.prefix$input.files, "input_files/", "raw_fastq.gz/"), paste0(getwd(), "/"), overwrite = TRUE, recursive = TRUE)
      cat(c("          To :", paste0(getwd(),"/raw_fastq.gz/"), "\n"))
      cat(c("Copying From :", paste0(pkg.global.path.prefix$input.files, "input_files/phenodata.csv"),  "\n"))
      file.copy(paste0(pkg.global.path.prefix$input.files, "input_files/phenodata.csv"), paste0(getwd(), "/phenodata.csv"))
      cat(c("          To :"), paste0(getwd(), "/phenodata.csv\n"))
      on.exit(setwd(current.path))
      if (isTRUE(optional)) {
        cat(c("Copying From :", paste0(pkg.global.path.prefix$input.files, "input_files/", "indexes/"),  "\n"))
        file.copy(paste0(pkg.global.path.prefix$input.files, "input_files/", "indexes/"), paste0(getwd(), "/"), overwrite = TRUE, recursive = TRUE)
        cat(c("          To :", paste0(getwd(),"/indexes/"), "\n\n"))
      } else {
        cat("\n")
      }
    }
  }
}


