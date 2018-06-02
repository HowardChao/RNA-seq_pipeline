pkg.global.ht2 <- new.env()
pkg.global.ht2$logic <- FALSE

#' Check input files directory
#' @export
CheckInputDirFiles <- function(input.path.prefix = "NOT_SET_YET", gene.name = "NO_DATA", sample.pattern = "NO_DATA", print=TRUE) {
  if (isTRUE(CheckPrefixPath(pkg.global.path.prefix$data_path, print = FALSE))){
    if (input.path.prefix == "NOT_SET_YET" ||gene.name == "NO_DATA" || sample.pattern == "NO_DATA"){
      if (input.path.prefix == "NOT_SET_YET") {
        cat("(\u2718) : 'input.path.prefix' is missing.\n     Can't check input file directory.\n\n")
      }
      if (gene.name == "NO_DATA") {
        cat("(\u2718) : 'gene.name' is missing.\n\n")
      }
      if (sample.pattern == "NO_DATA") {
        cat("(\u2718) : 'sample.pattern' is missing.\n\n")
      }
      return(FALSE)
    } else {
      if (substr(input.path.prefix, nchar(input.path.prefix), nchar(input.path.prefix)) != '/') {
        pkg.global.path.prefix$input.files <- paste0(input.path.prefix, '/')
      } else {
        pkg.global.path.prefix$input.files <- input.path.prefix
      }
      input.file.dir <- dir.exists(paste0(pkg.global.path.prefix$input.files, "input_files/"))
      if(!isTRUE(input.file.dir)){
        cat(c("(\u2718) : ", paste0("'",pkg.global.path.prefix$input.files, "input_files/'"), "is not exit. Please check whether prefix absolute path of 'input_files' is valid.\n\n"))
        return(FALSE)
      }
      if (print) {
        cat(c("************** Checking hierarchy of", paste0("'", pkg.global.path.prefix$input.files, 'input_files/\''), "************\n"))
        cat(c("(\u2714) : Prefix of input_files", paste0("'",pkg.global.path.prefix$input.files, "'"), "is valid\n\n"))
      }
      # only check whether exist
      gtf.file <- file.exists(paste0(pkg.global.path.prefix$input.files, "input_files/",gene.name, ".gtf"))
      # only check whether exist
      fa.file <- file.exists(paste0(pkg.global.path.prefix$input.files, "input_files/",gene.name, ".fa"))
      # check exist and rules
      raw.fastq.dir <- dir.exists(paste0(pkg.global.path.prefix$input.files, "input_files/raw_fastq.gz/"))
      # check exist and rules
      phenodata.file <- file.exists(paste0(pkg.global.path.prefix$input.files, "input_files/phenodata.csv"))
      # check exist and rules
      ht2.dir <- dir.exists(paste0(pkg.global.path.prefix$input.files, "input_files/indexes/"))
      if (isTRUE(raw.fastq.dir)) {
        raw.fastq <- list.files(path = paste0(pkg.global.path.prefix$input.files, 'input_files/raw_fastq.gz/'), pattern = sample.pattern, all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE)
        raw.fastq.number <- length(raw.fastq)
      }
      if (isTRUE(ht2.dir)) {
        ht2.files <- list.files(path = paste0(pkg.global.path.prefix$input.files, 'input_files/indexes/'), pattern = paste0("^", gene.name, "_tran.[0-9]*.ht2$"), all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE)
        ht2.files.number <- length(ht2.files)
      }
      if (!isTRUE(gtf.file)) {
        grab.gtf.file <- Sys.glob(file.path(path = paste0(pkg.global.path.prefix$input.files, 'input_files'), "*.gtf"))
        cat(paste0("(\u2718) : '", gene.name, ".gtf (user input)' and '", grab.gtf.file, " (find in directory)' ", " are mismatched.\n"))
      } else {
        if (print) {
          cat(paste0("(\u2714) : '", gene.name, ".gtf'", " is in 'input_files'\n"))
        }
      }
      if (!isTRUE(fa.file)) {
        grab.fa.file <- Sys.glob(file.path(path = paste0(pkg.global.path.prefix$input.files, 'input_files'), "*.fa"))
        cat(paste0("(\u2718) : '", gene.name, ".fa (user input)' and '", grab.fa.file, " (find in directory)' ", " are mismatched.\n"))
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
        cat(paste0("(\u2718) : Sample pattern \"", sample.pattern ,"\" is not found in 'raw_fastq.gz/'.\n"))
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
      invalid.column.number <- -1
      pheno_data <- read.csv(paste0(pkg.global.path.prefix$input.files, "/input_files/phenodata.csv"))
      sample.table <- as.data.frame(table(pheno_data[1]))
      compare.sample.table <- as.data.frame(table(pheno_data[2]))
      extract.fastq.gz.sample.names <- unique(gsub("_[1-2]*.fastq.gz", "", raw.fastq))
      if(length(extract.fastq.gz.sample.names) == length(row.names(sample.table))){
        # check the number of sample(rows) in 'phenodata.csv' is same as the number of unique samples in 'fastq.gz.files'
        if(identical(sort(extract.fastq.gz.sample.names), sort(as.character(sample.table[1][,1])))){
          # check all the name in bath 'phenodata.csv' and 'fastq.gz.files' are same
          if (length(row.names(compare.sample.table)) == 2) {
            ## only two groups are allowed
            invalid.column.number <- 0
            for( i in 1:length(pheno_data) ){
              if(length(pheno_data[i][is.na(pheno_data[i]) == TRUE]) != 0) {
                invalid.column.number <- invalid.column.number + 1
                cat(paste0("(\u2718) : There are missing values in column '", names(pheno_data[i]), "'\n" ))
              }
            }
            if (invalid.column.number != 0) {
              cat(paste0("(\u2718) : ", invalid.column.number, " columns are invalid. Please fix 'phenodata.csv'\n\n" ))
            }
          }
        } else {
          cat(paste0("(\u2718) : Sample names in 'phenodata.csv' and 'XXX.fastq.gz' are different. Please check 'phenodata.csv' matches the 'XXX.fastq.gz'\n\n" ))
        }
      } else {
        cat(paste0("(\u2718) : The sample's numbers in 'phenodata.csv' and 'XXX.fastq.gz' are different. Please check 'phenodata.csv' matches the 'XXX.fastq.gz'\n\n" ))
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
        cat(c("(\u26A0) : 'indexes/' directory has been created but there are no samples in 'indexes/' or files' names", paste0("\"^", gene.name, "_tran.[0-9]*.ht2$\""), "in 'indexes/' are not found.\n      No files will be copied.\n      (1). Check whether files name", paste0("'", gene.name, "_tran.[0-9]*.ht2'"), "matches the files in 'indexes' directory.\n      (2). If you don't have", paste0("'", gene.name, "_tran.[0-9].ht2'"), "files, remove 'indexes' directory\n\n"))
        return(FALSE)
      } else if (isTRUE(ht2.dir) && ht2.files.number >= 0) {
        pkg.global.ht2$logic <- TRUE
        if (print){
          for (i in ht2.files) {
            cat(paste0("(\u2714) : 'indexes/", i, "'"), "is in 'input_files/'\n")
          }
        }
      }
      if (isTRUE(gtf.file) && isTRUE(raw.fastq.dir) && isTRUE(raw.fastq.dir) && raw.fastq.number != 0 && isTRUE(phenodata.file) && invalid.column.number == 0 ) {
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


