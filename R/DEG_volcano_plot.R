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
    png(paste0(pkg.global.path.prefix$data_path, "DEG_results/images/MA.png"))
    print(paste0(pkg.global.path.prefix$data_path, "DEG_results/images/MA.png"))
    ggplot(DEG_dataset, aes(log2(all.mean), log2FC, colour = qval<0.05)) +
      scale_color_manual(values=c("#999999", "#FF0000")) +
      geom_point() +
      geom_hline(yintercept=0)
    dev.off()
  } else {



    # gene_id vs gene_name
    bg_gene_names = unique(bg_table[, 9:10])

    # FPKM for all samples in the ballgown object
    gene_expression = as.data.frame(gexpr(pkg.ballgown.data$bg_chrX_filt))

    # draw for distribution of transcript count per gene
    counts=table(transcript_gene_table[,"g_id"])
    c_one = length(which(counts == 1))
    c_more_than_one = length(which(counts > 1))
    c_max = max(counts)
    hist(counts, breaks=50, col="bisque4", xlab="Transcripts per gene", main="Distribution of transcript count per gene")
    legend_text = c(paste("Genes with one transcript =", c_one), paste("Genes with more than one transcript =", c_more_than_one), paste("Max transcripts for single gene = ", c_max))
    legend("topright", legend_text, lty=NULL)

    # draw the distribution of gene length
    full_table <- texpr(pkg.ballgown.data$bg_chrX , 'all')
    hist(full_table$length, breaks=50, xlab="Transcript length (bp)", main="Distribution of transcript lengths", col="steelblue")

    # Set the minimum non-zero FPKM values for use later. Do this by grabbing a copy of all data values,
    # coverting 0’s to NA, and calculating the minimum or all non NA values
    min_nonzero=1

    # Set the columns for finding FPKM and create shorter names for figures
    data_columns=c(1:sample.number)







    # frequency plot
    png(paste0(pkg.global.path.prefix$data_path, "gene_data/04.png"))
    pms <- results_transcripts
    mypar(1, 1)
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
      print(current.sum)
      for(j in 1:sample.table$Freq[i]){
        plot.column.number <- 8+j+current.sum + i -1
        print(plot.column.number)
        shist(log2(pms[, plot.column.number]), unit = 0.1, col = plot.column.number, add = TRUE, lwd = 2, lty = plot.column.number)
      }
    }
    dev.off()


    #results_genes <- stattest(gown=pkg.ballgown.data$bg_chrX_filt, feature="gene", covariate="sex", adjustvars = c("population"), getFC=TRUE, meas="FPKM")
    #results_genes <- data.frame(results_genes, transcriptNames=transcriptNames(pkg.ballgown.data$bg_chrX_filt), transcriptIDs=transcriptIDs(pkg.ballgown.data$bg_chrX_filt))
    #results_genes <- arrange(results_genes,pval)
    #table(results_genes$qval<0.05)
    #table(results_genes$pval<0.05)





    results_transcripts <- arrange(results_transcripts,pval)
    results_transcripts %>% filter(qval < 0.05)
    table(results_transcripts$qval < 0.05)
    table(results_transcripts$pval < 0.05)

    typeof(fpkm)

    results_transcripts$fpkm_mean <- fpkm_mean
    results_transcripts$log2_fpkm_plus_mean <- log2_fpkm_plus_mean

    # relationship between exon and transcript
    exon_id_list <- as.data.frame(eexpr(pkg.ballgown.data$bg_chrX_filt, 'all')$e_id,  col.names = "e_id")
    exon_id_list
    exon_read_count <- as.data.frame(eexpr(pkg.ballgown.data$bg_chrX_filt, 'rcount'))
    exon_read_count
    exon_read_count$e_id <-unlist (exon_id_list)
    exon_read_count
    exon_to_trans <- indexes(pkg.ballgown.data$bg_chrX_filt)$e2t


    # merge to one dataset
    # merged <- merge(exon_to_trans, exon_read_count, by = interaction(exon_to_trans$e_id, exon_read_count$e_id))




    ## volcano plot
    # Make a basic volcano plot
    with(results_genes, plot(log2FoldChange, -log10(pvalue), pch=20, main="Volcano plot", xlim=c(-2.5,2)))

    # Add colored points: red if padj<0.05, orange of log2FC>1, green if both)
    with(subset(res, padj<.05 ), points(fc, -log10(pval), pch=20, col="red"))
    with(subset(res, abs(log2FoldChange)>1), points(log2FoldChange, -log10(pvalue), pch=20, col="orange"))
    with(subset(res, padj<.05 & abs(log2FoldChange)>1), points(log2FoldChange, -log10(pvalue), pch=20, col="green"))

    # Label points with the textxy function from the calibrate plot
    library(calibrate)
    with(subset(res, padj<.05 & abs(log2FoldChange)>1), textxy(log2FoldChange, -log10(pvalue), labs=Gene, cex=.8))

    #results_transcripts.tpm <- stattest(pkg.ballgown.data$bg_chrX_filt, feature="transcript",covariate="sex",adjustvars = c("population"), getFC=TRUE, meas="TPM")

    cat(c("************** Writing .csv ************\n"))
    dir.create(file.path(paste0(pkg.global.path.prefix$data_path, "gene_data/ballgown/results/")), showWarnings = FALSE)

    print(subset(results_transcripts,results_transcripts$qval<0.05))
    print(subset(results_genes,results_genes$qval<0.05))
    tropical <- c('darkorange', 'dodgerblue', 'hotpink', 'limegreen', 'yellow')
    palette(tropical)

    data_columns=c(1:12)
    fpkm_gene <- as.data.frame(gexpr(pkg.ballgown.data$bg_chrX))
    fpkm_gene <- log2(fpkm[,data_columns]+1)

    #cat(c("************** draw box plot ************\n"))
    #png(paste0(pkg.global.path.prefix$data_path, "gene_data/ballgown/results/sample_vs_FPKM.png"))
    boxplot(fpkm,col=as.numeric(pheno_data$sex),las=2,ylab='log2(FPKM+1)')
    #dev.off()

    # gene_expression draw
    boxplot(log2(gene_expression[,data_columns]+min_nonzero), col=as.numeric(pheno_data$sex), las=2, ylab="log2(FPKM)", main="Distribution of FPKMs for all 12 libraries")

    # Plot #4 - plot a pair of replicates to assess reproducibility of technical replicates Tranform the data
    # by converting to log2 scale after adding an arbitrary small value to avoid log2(0)
    x = gene_expression[, "FPKM.ERR188044"]
    y = gene_expression[, "FPKM.ERR188104"]

    plot(x=log2(x+min_nonzero), y=log2(y+min_nonzero), pch=16, col="blue", cex=0.25, xlab="FPKM (IS20351_DS, Replicate 1)", ylab="FPKM (IS20351_DS, Replicate 2)", main="Comparison of expression values for a pair of replicates")
    abline(a=0,b=1)
    rs=cor(x,y)^2
    legend("topleft", paste("R squared = ", round(rs, digits=3), sep=""), lwd=1, col="black")


    fpkm_summary <- texpr(pkg.ballgown.data$bg_chrX,meas="all")
    write.csv(fpkm_summary, paste0(pkg.global.path.prefix$data_path, "gene_data/ballgown/results/FPKM_summary.csv"))
    #print(ballgown::transcriptNames(pkg.ballgown.data$bg_chrX)[11])
    #print(ballgown::geneNames(pkg.ballgown.data$bg_chrX)[11])
    plot(fpkm[11,] ~ pheno_data$sex, border=c(1,2), main=paste(ballgown::geneNames(pkg.ballgown.data$bg_chrX)[12],' : ', ballgown::transcriptNames(pkg.ballgown.data$bg_chrX)[12]),pch=19, xlab="Sex", ylab='log2(FPKM+1)')
    print(fpkm)
    points(fpkm[11,] ~ jitter(as.numeric(pheno_data$sex)), col=as.numeric(pheno_data$sex))
    plotTranscripts(ballgown::geneIDs(pkg.ballgown.data$bg_chrX)[1721], pkg.ballgown.data$bg_chrX, main=c('Gene XIST in sample ERR188234'), sample=c('ERR188234'))
    plotMeans('MSTRG.56', pkg.ballgown.data$bg_chrX_filt, groupvar="sex",legend=FALSE)
  }
}
