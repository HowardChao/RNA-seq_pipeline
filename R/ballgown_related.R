pkg.ballgown.data <- new.env()
pkg.ballgown.data$bg_chrX <- ""
pkg.ballgown.data$bg_chrX_filt <- ""

#' Run ballgown analysis
#' @export
BallgownDraw <- function() {
  current.path <- getwd()
  print(paste0(pkg.global.path.prefix$data_path, "gene_data/"))
  setwd(paste0(pkg.global.path.prefix$data_path, "gene_data/"))
  pheno_data <- read.csv("phenodata.csv")
  print(pheno_data)
  pkg.ballgown.data$bg_chrX <- ballgown(dataDir = "ballgown", samplePattern = "ERR", pData = pheno_data)
  print(pkg.ballgown.data$bg_chrX)
  pkg.ballgown.data$bg_chrX_filt <- ballgown::subset(pkg.ballgown.data$bg_chrX,"rowVars(ballgown::texpr(pkg.ballgown.data$bg_chrX)) >1",genomesubset=TRUE)
  print(pkg.ballgown.data$bg_chrX_filt)
  results_transcripts <- stattest(pkg.ballgown.data$bg_chrX_filt, feature="transcript",covariate="sex",adjustvars = c("population"), getFC=TRUE, meas="FPKM")
  results_genes <- stattest(pkg.ballgown.data$bg_chrX_filt, feature="gene", covariate="sex", adjustvars = c("population"), getFC=TRUE, meas="FPKM")
  results_transcripts <- data.frame(geneNames=ballgown::geneNames(pkg.ballgown.data$bg_chrX_filt), geneIDs=ballgown::geneIDs(pkg.ballgown.data$bg_chrX_filt), results_transcripts)
  results_transcripts <- arrange(results_transcripts,pval)
  results_genes <- arrange(results_genes,pval)
  cat(c("************** Writing .csv ************\n"))
  write.csv(results_transcripts, "chrX_transcript_results.csv", row.names=FALSE)
  write.csv(results_genes, "chrX_gene_results.csv", row.names=FALSE)
  print(subset(results_transcripts,results_transcripts$qval<0.05))
  print(subset(results_genes,results_genes$qval<0.05))
  cat(c("************** Setting tropical ************\n"))
  tropical <- c('darkorange', 'dodgerblue', 'hotpink', 'limegreen', 'yellow')
  palette(tropical)
  cat(c("************** Showing distribution of gene ************\n"))
  fpkm <- texpr(pkg.ballgown.data$bg_chrX,meas="FPKM")
  fpkm <- log2(fpkm+1)
  cat(c("************** draw box plot ************\n"))
  boxplot(fpkm,col=as.numeric(pheno_data$sex),las=2,ylab='log2(FPKM+1)')
  print(ballgown::transcriptNames(pkg.ballgown.data$bg_chrX)[11])
  print(ballgown::geneNames(pkg.ballgown.data$bg_chrX)[11])
  plot(fpkm[11,] ~ pheno_data$sex, border=c(1,2), main=paste(ballgown::geneNames(pkg.ballgown.data$bg_chrX)[12],' : ', ballgown::transcriptNames(pkg.ballgown.data$bg_chrX)[12]),pch=19, xlab="Sex", ylab='log2(FPKM+1)')
  points(fpkm[11,] ~ jitter(as.numeric(pheno_data$sex)), col=as.numeric(pheno_data$sex))
  plotTranscripts(ballgown::geneIDs(pkg.ballgown.data$bg_chrX)[1721], pkg.ballgown.data$bg_chrX, main=c('Gene XIST in sample ERR188234'), sample=c('ERR188234'))
  plotMeans('MSTRG.56', pkg.ballgown.data$bg_chrX_filt, groupvar="sex",legend=FALSE)
  on.exit(setwd(current.path))
}

#' check pkg.ballgown.data
#' @export
CheckBallgownGlobal <- function() {
  print(pkg.ballgown.data$bg_chrX)
  print(pkg.ballgown.data$bg_chrX_filt)
}
