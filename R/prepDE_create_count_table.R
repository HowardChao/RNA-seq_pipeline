#' converting stringtie ballogwn preprocessed data to count table
#' @export
PreDECountTable <- function(sample.pattern="NO_DATA", print=TRUE) {
  # ftp server : ftp://ftp.ccb.jhu.edu/pub/infphilo/hisat2/downloads/hisat2-2.1.0-source.zip
  if (sample.pattern == "NO_DATA") {
    cat("(\u2718) : sample.pattern is missing.\n\n")
  } else {
    if (isTRUE(CheckDirAll(print = FALSE))){
      if (!dir.exists(paste0(pkg.global.path.prefix$data_path, "gene_data/ballgown/raw_count/"))){
        dir.create(paste0(pkg.global.path.prefix$data_path, "gene_data/ballgown/raw_count/"))
      }
      cat("************** Installing prepDE.py ************\n")
      cat(paste0(pkg.global.path.prefix$data_path, "gene_data/ballgown/raw_count\n"))
      current.path <- getwd()
      setwd(paste0(pkg.global.path.prefix$data_path, "gene_data/ballgown/raw_count/"))
      system2(command = 'curl', args = c('https://ccb.jhu.edu/software/stringtie/dl/prepDE.py', '--output', paste0(pkg.global.path.prefix$data_path, "gene_data/ballgown/raw_count/prepDE.py")), stdout = "", wait = TRUE)
      cat(paste0("'", pkg.global.path.prefix$data_path, "gene_data/ballgown/raw_count/prepDE.py' has been installed.\n\n"))
      cat("************** Creating 'sample_lst.txt' file ************\n")
      sample.files <- list.files(paste0(pkg.global.path.prefix$data_path, "gene_data/ballgown/"), pattern = sample.pattern)
      write.content <- print(paste0(sample.files[1], " ", pkg.global.path.prefix$data_path, "gene_data/ballgown/", sample.files[1] ,"/", sample.files[1], ".gtf"))
      for(i in 2:length(sample.files)){
        #print(paste0(sample.files[i], " ", pkg.global.path.prefix$data_path, "gene_data/ballgown/", sample.files[i],"/", sample.files[i], ".gtf"))
        write.content <- c(write.content, paste0(sample.files[i], " ", pkg.global.path.prefix$data_path, "gene_data/ballgown/", sample.files[i],"/", sample.files[i], ".gtf"))
      }
      write.file<-file(paste0(pkg.global.path.prefix$data_path, "gene_data/ballgown/raw_count/sample_lst.txt"))
      writeLines(write.content, write.file)
      close(write.file)
      cat(paste0("'", pkg.global.path.prefix$data_path, "gene_data/ballgown/raw_count/sample_lst.txt' has been created\n\n"))
      cat("************** Creating gene and transcript raw count file ************\n")
      #print(paste0(pkg.global.path.prefix$data_path, "gene_data/ballgown/raw_count/prepDE.py -i ",  pkg.global.path.prefix$data_path, "gene_data/ballgown/raw_count/sample_lst.txt"))
      # have to check python !!!
      if(py_available(initialize = "TRUE")){
        cat("(\u2714) : Python is available on your device!\n")
        python.version <- as.numeric(py_config()$version)
        cat(paste0("       Python version : ", py_config()$version, "\n"))
        if(python.version >= 3) {
          cat("(\u270D) : Converting 'prepDE.py' from python2 to python3 \n\n")
          system2(command = '2to3', arg = paste0("-w ", pkg.global.path.prefix$data_path, "gene_data/ballgown/raw_count/prepDE.py"))
        } else if (python.version < 3 && python.version >= 2 ){
        }
        system2(command = 'python', args = paste0(pkg.global.path.prefix$data_path, "gene_data/ballgown/raw_count/prepDE.py -i ",  pkg.global.path.prefix$data_path, "gene_data/ballgown/raw_count/sample_lst.txt"))
        cat(paste0("'", pkg.global.path.prefix$data_path, "gene_data/ballgown/raw_count/gene_count_matrix.csv' has been created\n"))
        cat(paste0("'", pkg.global.path.prefix$data_path, "gene_data/ballgown/raw_count/transcript_count_matrix.csv' has been created\n\n"))
        on.exit(setwd(current.path))
        return(TRUE)
      } else {
        cat("(\u2718) : Python is not available on this device. Please install python to run python script 'prepDE.py'\n\n")
        on.exit(setwd(current.path))
        return(FALSE)
      }
    }
  }
}

#' DEG analysis with edgeR
#' @export
DEGedgeRPlot <- function() {
  # likelihood ratio test and quasi-likelihood F-test
  pheno_data <- read.csv(paste0(pkg.global.path.prefix$data_path, "gene_data/phenodata.csv"))
  x <- read.csv(paste0(pkg.global.path.prefix$data_path, "gene_data/ballgown/raw_count/gene_count_matrix.csv"))
  group <- pheno_data$sex
  y <- DGEList(counts=x[-1], group = group)
  keep <- rowSums(cpm(y)>1) >= 2
  y <- y[keep, , keep.lib.sizes=FALSE]
  y <- calcNormFactors(y)
  y$samples
}
