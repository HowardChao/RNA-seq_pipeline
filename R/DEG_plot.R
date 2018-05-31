#' DEG volcanplot
#' @export
DEGVolcanoPlot <- function() {
  if(file.exists(paste0(pkg.global.path.prefix$data_path, "DEG_results/FPKM_DEG_result.csv"))){
    # load gene name for further usage
    if(!dir.exists(paste0(pkg.global.path.prefix$data_path, "DEG_results/images"))){
      dir.create(paste0(pkg.global.path.prefix$data_path, "DEG_results/images"))
    }
    cat(paste0("************** Plotting Volcano plot **************\n"))
    DEG_dataset <- read.csv(paste0(pkg.global.path.prefix$data_path, "DEG_results/FPKM_DEG_result.csv"))
    ## Volcano plot
    # Make a basic volcano plot
    png(paste0(pkg.global.path.prefix$data_path, "DEG_results/images/Volcano_plot.png"))
    par(mar=c(5,5,5,5), cex=1.0, cex.main=1.4, cex.axis=1.4, cex.lab=1.4)
    topT <- as.data.frame(DEG_dataset)
    with(topT, plot(log2FC, -log10(pval), pch=20, main="Volcano plot", cex=1.0, xlab=bquote(~Log[2]~fold~change), ylab=bquote(~-log[10]~Q~value), xlim=c(-15,15), ylim = c(0,12)))
    with(subset(topT, pval<0.05 & abs(log2FC)>3), points(log2FC, -log10(pval), pch=20, col="red"))
    with(subset(topT, pval<0.05 & log2FC< -3), points(log2FC, -log10(pval), pch=20, col="green"))
    abline(v=c(-3,3), h=2, col="black", lty='dashed')
    #abline(v=0, col="black", lty=3, lwd=1.0)
    #abline(v=-2, col="black", lty=4, lwd=2.0)
    #abline(v=2, col="black", lty=4, lwd=2.0)
    #abline(h=-log10(max(topT$pval[topT$pval<0.05], na.rm=TRUE)), col="black", lty=4, lwd=2.0)

    # this is to add the DEG name on the picture
    #library(calibrate)
    #with(subset(results_transcripts, pval<.05 & abs(log2FC)>2), textxy(log2FC, -log10(pval), labs=geneNames, cex=.8))
    dev.off()
  } else {
    cat("(\u2718) : 'FPKM_DEG_result.csv' haven't created yet.\n\n")
  }
}

#'
#' @export
DEGMAPlot <- function() {
  if(file.exists(paste0(pkg.global.path.prefix$data_path, "DEG_results/FPKM_DEG_result.csv"))){
    # load gene name for further usage
    if(!dir.exists(paste0(pkg.global.path.prefix$data_path, "DEG_results/images"))){
      dir.create(paste0(pkg.global.path.prefix$data_path, "DEG_results/images"))
    }
    cat(paste0("************** Plotting MA plot **************\n"))
    DEG_dataset <- read.csv(paste0(pkg.global.path.prefix$data_path, "DEG_results/FPKM_DEG_result.csv"))
    ## Ma plot
    png(paste0(pkg.global.path.prefix$data_path, "DEG_results/images/MA_plot.png"))
    p <- ggplot(DEG_dataset, aes(log2(FPKM.all.mean), log2FC, colour = qval<0.05)) +
      scale_color_manual(values=c("#999999", "#FF0000")) +
      geom_point() +
      geom_hline(yintercept=0, color="blue") +
      ylim(-6, 6)
    print(p)
    dev.off()
  } else {
    cat("(\u2718) : 'FPKM_DEG_result.csv' haven't created yet.\n\n")
  }
}

#' Frequency plot
#' @export
DEGFrequencyPlot <- function() {
  if(file.exists(paste0(pkg.global.path.prefix$data_path, "DEG_results/FPKM_DEG_result.csv"))){
    # load gene name for further usage
    if(!dir.exists(paste0(pkg.global.path.prefix$data_path, "DEG_results/images"))){
      dir.create(paste0(pkg.global.path.prefix$data_path, "DEG_results/images"))
    }
    cat(paste0("************** Plotting  Frequency plot **************\n"))
    DEG_dataset <- read.csv(paste0(pkg.global.path.prefix$data_path, "DEG_results/FPKM_DEG_result.csv"))
    # frequency plot
    png(paste0(pkg.global.path.prefix$data_path, "DEG_results/images/Frequency_plot.png"))
    pms <- DEG_dataset
    mypar(1, 1)
    pheno_data <- read.csv(paste0(pkg.global.path.prefix$data_path, "gene_data/phenodata.csv"))
    sample.table <- as.data.frame(table(pheno_data[2]))
    shist(log2(pms[, 5]), unit = 0.1, type = "n", xlab = "log (base 2) FPKM",
          main = "All samples", xlim = c(-5, 15))
    for(i in 1:length(row.names(sample.table))){
      current.sum <- 0
      if (i-1 == 0 ) current.sum = 0
      else {
        for(z in 1:(i-1)) {
          current.sum <- current.sum + sample.table$Freq[z]
        }
      }
      for(j in 1:sample.table$Freq[i]){
        plot.column.number <- 4+j+current.sum + i -1
        shist(log2(pms[, plot.column.number]), unit = 0.1, col = plot.column.number, add = TRUE, lwd = 2, lty = plot.column.number)
      }
    }
    dev.off()
  }
}


#'
#' @export
DEGTranscriptRelatedPlot <- function(){
  # draw for distribution of transcript count per gene
  if(file.exists(paste0(pkg.global.path.prefix$data_path, "DEG_results/FPKM_DEG_result.csv"))){
    # load gene name for further usage
    if(!dir.exists(paste0(pkg.global.path.prefix$data_path, "DEG_results/images"))){
      dir.create(paste0(pkg.global.path.prefix$data_path, "DEG_results/images"))
    }
    if (is.null(pkg.ballgown.data$bg_chrX)) {
      LoadBallgownObject()
    } else {
      cat(paste0("************** Plotting transcript related plot **************\n"))
      if(!dir.exists(paste0(pkg.global.path.prefix$data_path, "DEG_results/images/Transcript_Related/"))){
        dir.create(paste0(pkg.global.path.prefix$data_path, "DEG_results/images/Transcript_Related/"))
      }
      transcript_gene_table <- indexes(pkg.ballgown.data$bg_chrX)$t2g
      counts=table(transcript_gene_table[,"g_id"])
      c_one = length(which(counts == 1))
      c_more_than_one = length(which(counts > 1))
      c_max = max(counts)
      png(paste0(pkg.global.path.prefix$data_path, "DEG_results/images/Transcript_Related/Distribution_transcript_count_per_gene_plot.png"))
      hist(counts, breaks=50, col="bisque4", xlab="Transcripts per gene", main="Distribution of transcript count per gene")
      legend_text = c(paste("Genes with one transcript =", c_one), paste("Genes with more than one transcript =", c_more_than_one), paste("Max transcripts for single gene = ", c_max))
      legend("topright", legend_text, lty=NULL)
      dev.off()

      # draw the distribution of transcript length
      full_table <- texpr(pkg.ballgown.data$bg_chrX, 'all')
      t.mini.length = min(full_table$length[full_table$length > 0])
      t.max.length = max(full_table$length[full_table$length > 0])
      png(paste0(pkg.global.path.prefix$data_path, "DEG_results/images/Transcript_Related/Distribution_transcript_length_plot.png"))
      hist(full_table$length, breaks=50, xlab="Transcript length (bp)", main="Distribution of transcript lengths", col="steelblue")
      legend_text = c(paste("Minimum transcript length =", t.mini.length), paste("Maximum transcript length =", t.max.length))
      legend("topright", legend_text, lty=NULL)
      dev.off()
    }
  }
}

#'
#' @export
DEGFPKMBoxPlot <- function() {
  if(file.exists(paste0(pkg.global.path.prefix$data_path, "DEG_results/FPKM_DEG_result.csv"))){
    # load gene name for further usage
    if(!dir.exists(paste0(pkg.global.path.prefix$data_path, "DEG_results/images"))){
      dir.create(paste0(pkg.global.path.prefix$data_path, "DEG_results/images"))
    }
    if (is.null(pkg.ballgown.data$bg_chrX)) {
      LoadBallgownObject()
    } else {
      cat(paste0("************** Plotting FPKM Box plot **************\n"))
      pheno.data <- read.csv(paste0(pkg.global.path.prefix$data_path, "gene_data/phenodata.csv"))
      # frequency plot
      tropical <- c('darkorange', 'dodgerblue', 'hotpink', 'limegreen', 'yellow')
      palette(tropical)
      fpkm = data.frame(texpr(pkg.ballgown.data$bg_chrX,meas="FPKM"))
      fpkm = log2(fpkm+1)
      png(paste0(pkg.global.path.prefix$data_path, "DEG_results/images/FPKM_box_plot.png"))
      boxplot(fpkm, col=as.numeric(pheno.data$sex), las=2, ylab='log2(FPKM+1)')
      dev.off()
    }
  }
}

#'
#' @export
DEGPCAPlot <- function(){
  # http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/
  if(file.exists(paste0(pkg.global.path.prefix$data_path, "DEG_results/FPKM_DEG_result.csv"))){
    # load gene name for further usage
    if(!dir.exists(paste0(pkg.global.path.prefix$data_path, "DEG_results/images"))){
      dir.create(paste0(pkg.global.path.prefix$data_path, "DEG_results/images"))
    }
    if (is.null(pkg.ballgown.data$bg_chrX)) {
      LoadBallgownObject()
    } else {
      cat(paste0("************** Plotting PCA plot **************\n"))
      if(!dir.exists(paste0(pkg.global.path.prefix$data_path, "DEG_results/images/PCA/"))){
        dir.create(paste0(pkg.global.path.prefix$data_path, "DEG_results/images/PCA/"))
      }
      fpkm <- data.frame(texpr(pkg.ballgown.data$bg_chrX,meas="FPKM"))
      fpkm.trans <- data.frame(t(fpkm))
      fpkm.trans.row.names <- row.names(fpkm.trans)
      fpkm.trans.row.names.clean <- gsub("FPKM.", "", fpkm.trans.row.names)
      row.names(fpkm.trans) <- fpkm.trans.row.names.clean

      fpkm.trans.sort <- fpkm.trans[ order(row.names(fpkm.trans)), ]
      pheno_data <- read.csv(paste0(pkg.global.path.prefix$data_path, "gene_data/phenodata.csv"))
      pheno_data.sort <- pheno_data[order(pheno_data$id),]
      fpkm.trans.sort$attribute <- as.character(pheno_data.sort[,2])
      # scale.unit = TRUE ==> the data are scaled to unit variance before the analysis.
      fpkm.pca <- PCA(fpkm.trans.sort[,-ncol(fpkm.trans.sort)], scale.unit = TRUE, ncp = 2, graph = FALSE)
      eig.val <- get_eigenvalue(fpkm.pca)
      png(paste0(pkg.global.path.prefix$data_path, "DEG_results/images/PCA/Dimension_pca_plot.png"))
      p1 <- fviz_eig(fpkm.pca, addlabels = TRUE, ylim = c(0, 50), title = "PCA Dimensions")
      print(p1)
      dev.off()
      #var$coord: coordinates of variables to create a scatter plot
      #var$cos2: represents the quality of representation for variables on the factor map. It’s calculated as the squared coordinates: var.cos2 = var.coord * var.coord.
      #var$contrib: contains the contributions (in percentage) of the variables to the principal components. The contribution of a variable (var) to a given principal component is (in percentage) : (var.cos2 * 100) / (total cos2 of the component).
      #var <- get_pca_var(res.pca)
      png(paste0(pkg.global.path.prefix$data_path, "DEG_results/images/PCA/PCA_plot.png"))
      p2 <- fviz_pca_ind(fpkm.pca,
                   title = "Principal Component Analysis",
                   xlab = paste0("PC1(", round(data.frame(eig.val)$variance.percent[1], 2), "%)"), ylab = paste0("PC2(", round(data.frame(eig.val)$variance.percent[2],2), "%)"),
                   legend.title = "Treatment variable", legend.position = "top",
                   pointshape = 21,
                   pointsize = 2.5,
                   geom.ind = "point", # show points only (nbut not "text")
                   fill.ind = fpkm.trans.sort$attribute,
                   col.ind = fpkm.trans.sort$attribute, # color by groups
                   #palette = c("#00AFBB", "#E7B800"),
                   #                 addEllipses = TRUE, # Concentration ellipses
      )
      print(p2)
      dev.off()
    }
  }
}

#'
#' @export
DEGCorrelationPlot <- function(){
  if(file.exists(paste0(pkg.global.path.prefix$data_path, "DEG_results/FPKM_DEG_result.csv"))){
    # load gene name for further usage
    if(!dir.exists(paste0(pkg.global.path.prefix$data_path, "DEG_results/images"))){
      dir.create(paste0(pkg.global.path.prefix$data_path, "DEG_results/images"))
    }
    cat(paste0("************** Plotting Correlation plot **************\n"))
    if(!dir.exists(paste0(pkg.global.path.prefix$data_path, "DEG_results/images/Correlation/"))){
      dir.create(paste0(pkg.global.path.prefix$data_path, "DEG_results/images/Correlation/"))
    }
    DEG_dataset <- read.csv(paste0(pkg.global.path.prefix$data_path, "DEG_results/FPKM_DEG_result.csv"))
    pheno.data <- read.csv(paste0(pkg.global.path.prefix$data_path, "gene_data/phenodata.csv"))
    pheno.data.table <- as.data.frame(table(pheno.data[2]))
    count <- 0
    select.column <- c()
    for(i in 1:length(row.names(pheno.data.table))){
      for(j in 1:pheno.data.table$Freq[i]){
        a <- j + 4 + count + (i-1)
        select.column <- c(select.column, a)
        #count <- count+j
        #print(count)
      }
      count <- count + pheno.data.table$Freq[i]
    }
    res <- cor(head(DEG_dataset[select.column]), method = c("pearson", "kendall", "spearman"))
    png(paste0(pkg.global.path.prefix$data_path, "DEG_results/images/Correlation/Correlation_dot_plot.png"))
    p1 <- corrplot(res, type = "upper", order = "hclust",
             tl.col = "black", tl.srt = 45)
    print(p1)
    dev.off()
    png(paste0(pkg.global.path.prefix$data_path, "DEG_results/images/Correlation/Correlation_plot.png"))
    p2 <- chart.Correlation(res, histogram=TRUE, pch=19)
    print(p2)
    dev.off()

    png(paste0(pkg.global.path.prefix$data_path, "DEG_results/images/Correlation/Correlation_heat_plot.png"))
    col<- colorRampPalette(c("blue", "white", "red"))(20)
    p3 <- heatmap(x = res, col = col, symm = TRUE)
    print(p3)
    dev.off()
  }
}

#'
#' @export
DEGPlotAll <- function() {
  DEGVolcanoPlot()
  DEGMAPlot()
  DEGFrequencyPlot()
  DEGTranscriptRelatedPlot()
  DEGFPKMBoxPlot()
  DEGPCAPlot()
  DEGCorrelationPlot()
}
