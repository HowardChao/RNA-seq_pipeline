#' converting stringtie ballogwn preprocessed data to count table
#' @export
PreDECountTable <- function(sample.pattern="NO_DATA", print=TRUE) {
  # ftp server : ftp://ftp.ccb.jhu.edu/pub/infphilo/hisat2/downloads/hisat2-2.1.0-source.zip
  if (isTRUE(CheckPrefixPath(print=FALSE))) {
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
}

#' DEG analysis with edgeR
#' @export
DEGedgeRPlot <- function() {
  # likelihood ratio test and quasi-likelihood F-test
  pheno_data <- read.csv(paste0(pkg.global.path.prefix$data_path, "gene_data/phenodata.csv"))
  x <- read.csv(paste0(pkg.global.path.prefix$data_path, "gene_data/ballgown/raw_count/gene_count_matrix.csv"))
  group <- pheno_data$sex

  genetable <- data.frame(gene.id=x$gene_id)

  # create DGEList object
  y <- DGEList(counts=x[-1], group = group, genes = genetable)
  countsPerMillion <- cpm(y)
  logcountsPerMillion <- cpm(y, log=TRUE)

  countCheck <- countsPerMillion > 1

  # genes that will express in all 12 samples
  table(rowSums(y$counts==0)==12)
  #y <- calcNormFactors(y, method = "TMM")

  plotMDS(y)


  sampleType<- as.character(sort(pheno_data$sex))
  sampleType[grep("female", sampleType)] <- "F"
  sampleType[grep("male", sampleType)] <- "M"
  # sampleReplicate <- paste("S", rep(1:2, each=6), sep="")
  designMat <- model.matrix(~sampleType)


  #design <- model.matrix( ,y$samples)


  dgList <- estimateGLMCommonDisp(y, design=designMat)
  dgList <- estimateGLMTrendedDisp(dgList, design=designMat)
  dgList <- estimateGLMTagwiseDisp(dgList, design=designMat)
  plotBCV(dgList)


  y <- calcNormFactors(y)
  y <- estimateDisp(y, designMat)
  fit <- glmFit(y, designMat)
  lrt <- glmLRT(fit, coef=ncol(designMat))
  tt <- topTags(lrt, n=nrow(y), p.value=0.1)
  tt10 <- topTags(lrt) # just the top 10 by default
  tt10
  tt.all <- topTags(lrt, n=nrow(y), sort.by="none")
  table(DESeq2=res$padj < 0.1, edgeR=tt.all$table$FDR < 0.1)

  plotSmear(lrt, de.tags=tt$table$gene.id)


  k <- c("16N", "16T", "18N", "18T", "19N", "19T")
  sampleType<- rep("N", 6)
  sampleType[grep("T", k)] <- "T"
  sampleReplicate <- paste("S", rep(1:3, each=2), sep="")
  designMat <- model.matrix(~sampleReplicate + sampleType)
  dgList <- estimateGLMCommonDisp(y, design=designMat)

  keep <- rowSums(cpm(y)>1) >= 2
  y <- y[keep, , keep.lib.sizes=FALSE]
  y <- calcNormFactors(y)
  y$samples
}
