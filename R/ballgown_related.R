pkg.ballgown.data <- new.env()
pkg.ballgown.data$bg_chrX <- ""
pkg.ballgown.data$bg_chrX_filt <- ""

#' Run ballgown analysis
#' @export
BallgownPreprocess <- function(gene.name = "NO_DATA", sample.pattern = "NO_DATA", covariate = "NO_DATA") {
  if (gene.name == "NO_DATA" || sample.pattern == "NO_DATA" || covariate == "NO_DATA") {
    if (sample.pattern == "NO_DATA") {
      cat("(\u2718) :sample.pattern is missing.\n\n")
    }
    if (covariate == "NO_DATA") {
      cat("(\u2718) :covariate is missing.\n\n")
    }
  } else {
    results <- ProgressGenesFiles(gene.name = gene.name, sample.pattern = sample.pattern, print = FALSE)
    if (isTRUE(results$phenodata.file.df) && (results$phenodata.invalid.column.number.df == 0) && results$ballgown.dirs.number.df != 0){
      adjustvars <- results$phenodata.adjustvars.df
      # sorting 'pheno_data'
      cat(paste0("************** Ballgown data preprocessing **************\n"))
      cat("\u25CF 1. Printing origin phenodata.csv : \n")
      pheno_data <- read.csv(paste0(pkg.global.path.prefix$data_path, "gene_data/phenodata.csv"))
      print(pheno_data)
      cat('\n')
      sample.table <- as.data.frame(table(pheno_data[covariate]))
      if (length(row.names(sample.table)) == 2) {
        dir.create(paste0(pkg.global.path.prefix$data_path, "DEG_results/"))
        cat("\u25CF 2. Sorting phenodata.csv : \n")
        pheno_data.arrange <- arrange(pheno_data, unlist(pheno_data[covariate]))
        print(pheno_data.arrange)
        cat('\n')
        # for adding FPKM column!
        sample.names <- as.character(pheno_data.arrange$ids)
        sample.names.with.covariate <- paste0(pheno_data.arrange$ids, ".", pheno_data.arrange[covariate][,1])
        sample.number <- length(sample.names)
        # make ballgown object

        cat("\u25CF 3. Making ballgown object : \n")
        pkg.ballgown.data$bg_chrX <- ballgown(dataDir = paste0(pkg.global.path.prefix$data_path, "gene_data/ballgown"), samplePattern = sample.pattern, pData = pheno_data, meas = 'all')
        bg <- pkg.ballgown.data$bg_chrX
        save(bg, file = paste0(pkg.global.path.prefix$data_path, "gene_data/ballgown/ballgown.rda"))
        cat('\n')
        pkg.ballgown.data$bg_chrX_filt <- ballgown::subset(pkg.ballgown.data$bg_chrX,"rowVars(ballgown::texpr(pkg.ballgown.data$bg_chrX)) >1",genomesubset=TRUE)
        #print(pkg.ballgown.data$bg_chrX_filt)

        # differential expression
        cat("\u25CF 4. Differential expression preprocessing : \n")
        cat("     \u25CF creating 'Differential Expression Gene FPKM data.frame ......'\n")
        cat(c("         \u25CF  covariate :", covariate, "\n"))
        if (length(adjustvars) != 0) {
          cat(c("         \u25CF adjustvars :", adjustvars, "\n"))
          results_transcripts <- stattest(pkg.ballgown.data$bg_chrX_filt, feature="transcript",covariate=covariate, adjustvars = adjustvars, getFC=TRUE, meas="FPKM")
        } else {
          results_transcripts <- stattest(pkg.ballgown.data$bg_chrX_filt, feature="transcript",covariate=covariate, getFC=TRUE, meas="FPKM")
        }
        results_transcripts$feature <- NULL
        results_transcripts$FC <- results_transcripts$fc
        results_transcripts$log2FC <- log2(results_transcripts$fc)
        results_transcripts$fc <- NULL
        colnames(results_transcripts)[1] <- "transcriptIDs"
        results_transcripts <- data.frame(geneNames=ballgown::geneNames(pkg.ballgown.data$bg_chrX_filt), geneIDs=ballgown::geneIDs(pkg.ballgown.data$bg_chrX_filt), transcriptNames=transcriptNames(pkg.ballgown.data$bg_chrX_filt), results_transcripts)
        # adding fpkm
        # cov : average per-base read coverage
        cat("     \u25CF merging each FPKM column and calculating average FPKM ......'\n")
        fpkm <- data.frame(texpr(pkg.ballgown.data$bg_chrX_filt,meas="FPKM"))
        all.mean <- c()
        for(i in 1:length(row.names(sample.table))) {
          columns.to.mean <- c()
          current.sum <- 0
          if (i-1 == 0 ) current.sum = 0
          else {
            for(z in 1:(i-1)) {
              current.sum <- current.sum + sample.table$Freq[z]
            }
          }
          for(j in 1:sample.table$Freq[i]){
            a <- paste0("FPKM.", sample.names[current.sum+j])
            results_transcripts[[sample.names.with.covariate[current.sum+j]]] <- fpkm[[a]]
            columns.to.mean <- append(columns.to.mean, sample.names.with.covariate[current.sum+j])
          }
          results_transcripts[[paste0(as.character(sample.table$Var1)[i], ".mean")]] <- rowMeans(results_transcripts[columns.to.mean])
          all.mean <- append(all.mean, paste0(as.character(sample.table$Var1)[i], ".mean"))
          columns.to.mean <- c()
        }
        results_transcripts[["FPKM.all.mean"]] <- rowMeans(results_transcripts[all.mean])
        cat("     \u25CF writing data.frame into 'FPKM_DEG_result.csv' ......'\n\n")
        write.csv(results_transcripts, paste0(pkg.global.path.prefix$data_path, "DEG_results/FPKM_DEG_result.csv"), row.names=FALSE)
        cat("\u25CF 5. Printing DEG dataset : \n")
        print(head(results_transcripts))
        cat("\n")
      } else {
        cat("(\u2718) : This pipline is only available for 2-group comparisons.\n")
        cat(paste0("      ",length(row.names(sample.table)), " is detected." ))
      }
    }
  }
}

#' check pkg.ballgown.data
#' @export
CheckBallgownGlobal <- function() {
  print(pkg.ballgown.data$bg_chrX)
  print(pkg.ballgown.data$bg_chrX_filt)
}
