#' functions about ballgown
#' @export
#' @import ballgown RSkittleBrewer dplyr devtools
#' @importFrom genefilter rowVars
BallgownDraw <- function() {
  print("Hello, ballgoen!")
  current.path <- getwd()
  print(paste0(pkg.global.path.prefix$data_path, "gene_data/"))
  setwd(paste0(pkg.global.path.prefix$data_path, "gene_data/"))
  pheno_data = read.csv("geuvadis_phenodata.csv")
  bg_chrX = ballgown(dataDir = "ballgown", samplePattern = "ERR", pData = pheno_data)
  #bg_chrX_filt = subset(bg_chrX,"rowVars(texpr(bg_chrX)) >1",genomesubset=TRUE)
  results_transcripts = stattest(bg_chrX, feature="transcript",covariate="sex",adjustvars = c("population"), getFC=TRUE, meas="FPKM")
  results_genes = stattest(bg_chrX, feature="gene", covariate="sex", adjustvars = c("population"), getFC=TRUE, meas="FPKM")
  results_transcripts = data.frame(geneNames=ballgown::geneNames(bg_chrX), geneIDs=ballgown::geneIDs(bg_chrX), results_transcripts)
  results_transcripts = arrange(results_transcripts,pval)
  results_genes = arrange(results_genes,pval)
  cat(c("************** Writing .csv ************\n"))
  write.csv(results_transcripts, "chrX_transcript_results.csv", row.names=FALSE)
  write.csv(results_genes, "chrX_gene_results.csv", row.names=FALSE)
  print(subset(results_transcripts,results_transcripts$qval<0.05))
  print(subset(results_genes,results_genes$qval<0.05))
  cat(c("************** Setting tropical ************\n"))
  tropical= c('darkorange', 'dodgerblue', 'hotpink', 'limegreen', 'yellow')
  palette(tropical)
  cat(c("************** Showing distribution of gene ************\n"))
  fpkm = texpr(bg_chrX,meas="FPKM")
  fpkm = log2(fpkm+1)
  cat(c("************** draw box plot ************\n"))
  boxplot(fpkm,col=as.numeric(pheno_data$sex),las=2,ylab='log2(FPKM+1)')
  ballgown::transcriptNames(bg_chrX)[12]
  ballgown::geneNames(bg_chrX)[12]
  plot(fpkm[12,] ~ pheno_data$sex, border=c(1,2), main=paste(ballgown::geneNames(bg_chrX)[12],' : ', ballgown::transcriptNames(bg_chrX)[12]),pch=19, xlab="Sex", ylab='log2(FPKM+1)')
  points(fpkm[12,] ~ jitter(as.numeric(pheno_data$sex)), col=as.numeric(pheno_data$sex))
  plotTranscripts(ballgown::geneIDs(bg_chrX)[1729], bg_chrX, main=c('Gene XIST in sample ERR188234'), sample=c('ERR188234'))
  plotMeans('MSTRG.56', bg_chrX, groupvar="sex",legend=FALSE)
  on.exit(setwd(current.path))
}
