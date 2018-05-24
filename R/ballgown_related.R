pkg.ballgown.data <- new.env()
pkg.ballgown.data$bg_chrX <- ""
pkg.ballgown.data$bg_chrX_filt <- ""

#' Run ballgown analysis
#' @export
BallgownDraw <- function(sample.pattern = "NO_DATA", covariate = "NO_DATA") {
  if (sample.pattern == "NO_DATA" || covariate == "NO_DATA") {
    if (sample.pattern == "NO_DATA") {
      cat("(\u2718) :sample.pattern is missing.\n\n")
    }
    if (covariate == "NO_DATA") {
      cat("(\u2718) :covariate is missing.\n\n")
    }
  } else {
    print(paste0(pkg.global.path.prefix$data_path, "gene_data/"))
    cat(paste0("************** Differential analysis (Ballgown) **************\n"))
    pheno_data <- read.csv(paste0(pkg.global.path.prefix$data_path, "gene_data/phenodata.csv"))
    cat("\u25CF 1. printing phenodata.csv : ")
    print(pheno_data)
    cat("\n")
    # make ballgown object

    cat("\u25CF 2. making ballgown object : ")
    # 'cov' ==> average covergae values ;  'rcounts' read counts
    pkg.ballgown.data$bg_chrX <- ballgown(dataDir = paste0(pkg.global.path.prefix$data_path, "gene_data/ballgown"), samplePattern = "ERR", pData = pheno_data, meas = 'all')
    # set the condition to filter the ballgown object
    #save a file
    pkg.ballgown.data$bg_chrX_filt <- subset(pkg.ballgown.data$bg_chrX,"rowVars(ballgown::texpr(pkg.ballgown.data$bg_chrX)) > 1",genomesubset=TRUE)
    #print(pkg.ballgown.data$bg_chrX_filt)

    # load gene name for further usage
    bg_table = texpr(pkg.ballgown.data$bg_chrX_filt, 'all')
    bg_gene_names = unique(bg_table[, 9:10])

    # Pull the gene_expression data frame from the ballgown object
    gene_expression = as.data.frame(gexpr(pkg.ballgown.data$bg_chrX_filt))

    colnames(gene_expression)
    row.names(gene_expression)

    dim(gene_expression)

    # the way to see certain genes
    i = row.names(gene_expression) == "MSTRG.1"
    gene_expression[i,]

    # load transcript to gene index from ballgown object
    transcript_gene_table = indexes(pkg.ballgown.data$bg_chrX)$t2g
    transcript_gene_table

    # check how many rows
    length(row.names(transcript_gene_table))

    # check unique gene id
    length(unique(transcript_gene_table[,"g_id"]))

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
    data_columns=c(1:12)

    results_transcripts_cov <- stattest(pkg.ballgown.data$bg_chrX_filt, feature="transcript",covariate="sex",adjustvars = c("population"), getFC=TRUE, meas="cov")

    # stattest : Test each transcript, gene, exon, or intron in a ballgown object for differential expression, using comparisons of linear models.
    # the expression measurement to use for statistical tests. Must be one of "cov", "FPKM", "rcount", "ucount", "mrcount", or "mcov". Not all expression measurements are available for all features. Leave as default if gowntable is provided.

    # differential expression
    results_transcripts <- stattest(pkg.ballgown.data$bg_chrX_filt, feature="transcript",covariate="sex",adjustvars = c("population"), getFC=TRUE, meas="FPKM")
    results_transcripts <- data.frame(geneNames=ballgown::geneNames(pkg.ballgown.data$bg_chrX_filt), geneIDs=ballgown::geneIDs(pkg.ballgown.data$bg_chrX_filt), results_transcripts)
    results_transcripts <- arrange(results_transcripts,pval)
    results_transcripts %>% filter(qval < 0.05)
    table(results_transcripts$qval < 0.05)

    results_genes <- stattest(pkg.ballgown.data$bg_chrX_filt, feature="gene", covariate="sex", adjustvars = c("population"), getFC=TRUE, meas="FPKM")
    results_genes <- arrange(results_genes,pval)
    table(results_genes$qval<0.05)

    ## Ma plot
    results_transcripts$mean <- rowMeans(texpr(pkg.ballgown.data$bg_chrX_filt))

    ggplot(results_transcripts, aes(log2(mean), log2(fc), colour = qval<0.05)) +
      scale_color_manual(values=c("#999999", "#FF0000")) +
      geom_point() +
      geom_hline(yintercept=0)

    #results_transcripts.tpm <- stattest(pkg.ballgown.data$bg_chrX_filt, feature="transcript",covariate="sex",adjustvars = c("population"), getFC=TRUE, meas="TPM")

    cat(c("************** Writing .csv ************\n"))
    dir.create(file.path(paste0(pkg.global.path.prefix$data_path, "gene_data/ballgown/results/")), showWarnings = FALSE)

    print(subset(results_transcripts,results_transcripts$qval<0.05))
    print(subset(results_genes,results_genes$qval<0.05))
    tropical <- c('darkorange', 'dodgerblue', 'hotpink', 'limegreen', 'yellow')
    palette(tropical)
    fpkm <- texpr(pkg.ballgown.data$bg_chrX,meas="FPKM")
    fpkm <- log2(fpkm+1)
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

#' check pkg.ballgown.data
#' @export
CheckBallgownGlobal <- function() {
  print(pkg.ballgown.data$bg_chrX)
  print(pkg.ballgown.data$bg_chrX_filt)
}
