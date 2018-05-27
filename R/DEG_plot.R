#' DEG volcanplot
#' @export
DEGVolcanoPlot <- function() {
  if(file.exists(paste0(pkg.global.path.prefix$data_path, "DEG_results/FPKM_DEG_result.csv"))){
    # load gene name for further usage
    if(!dir.exists(paste0(pkg.global.path.prefix$data_path, "DEG_results/images"))){
      dir.create(paste0(pkg.global.path.prefix$data_path, "DEG_results/images"))
    }
    DEG_dataset <- read.csv(paste0(pkg.global.path.prefix$data_path, "DEG_results/FPKM_DEG_result.csv"))
    ## Volcano plot
    # Make a basic volcano plot
    png(paste0(pkg.global.path.prefix$data_path, "DEG_results/images/volcano_plot.png"))
    par(mar=c(5,5,5,5), cex=1.0, cex.main=1.4, cex.axis=1.4, cex.lab=1.4)
    topT <- as.data.frame(DEG_dataset)
    with(topT, plot(log2FC, -log10(pval), pch=20, main="Volcano plot", cex=1.0, xlab=bquote(~Log[2]~fold~change), ylab=bquote(~-log[10]~Q~value)))
    with(subset(topT, pval<0.05 & abs(log2FC)>2), points(log2FC, -log10(pval), pch=20, col="red"))
    with(subset(topT, pval<0.05 & log2FC< -2), points(log2FC, -log10(pval), pch=20, col="green"))
    abline(v=0, col="black", lty=3, lwd=1.0)
    abline(v=-2, col="black", lty=4, lwd=2.0)
    abline(v=2, col="black", lty=4, lwd=2.0)
    abline(h=-log10(max(topT$pval[topT$pval<0.05], na.rm=TRUE)), col="black", lty=4, lwd=2.0)
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
    DEG_dataset <- read.csv(paste0(pkg.global.path.prefix$data_path, "DEG_results/FPKM_DEG_result.csv"))
    ## Ma plot
    png(paste0(pkg.global.path.prefix$data_path, "DEG_results/images/MA_plot.png"))
    ggplot(DEG_dataset, aes(log2(FPKM.all.mean), log2FC, colour = qval<0.05)) +
      scale_color_manual(values=c("#999999", "#FF0000")) +
      geom_point() +
      geom_hline(yintercept=0, color="blue") +
      ylim(6,-6)
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
    DEG_dataset <- read.csv(paste0(pkg.global.path.prefix$data_path, "DEG_results/FPKM_DEG_result.csv"))
    # frequency plot
    png(paste0(pkg.global.path.prefix$data_path, "DEG_results/images/Frequency_plot.png"))
    pms <- DEG_dataset
    mypar(1, 1)
    pheno_data <- read.csv(paste0(pkg.global.path.prefix$data_path, "gene_data/phenodata.csv"))
    sample.table <- as.data.frame(table(pheno_data[2]))
    shist(log2(pms[, 9]), unit = 0.1, type = "n", xlab = "log (base 2) FPKM",
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
        plot.column.number <- 8+j+current.sum + i -1
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
  load(paste0(pkg.global.path.prefix$data_path, "gene_data/ballgown/ballgown.rda"))
  transcript_gene_table <- indexes(bg)$t2g
  counts=table(transcript_gene_table[,"g_id"])
  c_one = length(which(counts == 1))
  c_more_than_one = length(which(counts > 1))
  c_max = max(counts)
  png(paste0(pkg.global.path.prefix$data_path, "DEG_results/images/Distribution_transcript_count_per_gene_plot.png"))
  hist(counts, breaks=50, col="bisque4", xlab="Transcripts per gene", main="Distribution of transcript count per gene")
  legend_text = c(paste("Genes with one transcript =", c_one), paste("Genes with more than one transcript =", c_more_than_one), paste("Max transcripts for single gene = ", c_max))
  legend("topright", legend_text, lty=NULL)
  dev.off()

  # draw the distribution of gene length
  png(paste0(pkg.global.path.prefix$data_path, "DEG_results/images/Distribution_transcript_length_plot.png"))
  full_table <- texpr(bg, 'all')
  hist(full_table$length, breaks=50, xlab="Transcript length (bp)", main="Distribution of transcript lengths", col="steelblue")
  dev.off()
}

