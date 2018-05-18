#' rna seq pipline
#' @export
RNAseqPipeline <- function(path.prefix = "NOT_SET_YET", input.path.prefix = "NOT_SET_YET", gene.name = "NO_DATA", sample.pattern = "NO_DATA") {
  if (isTRUE(SetPrefixPath(path.prefix))){
    if (isTRUE(CheckDirAll(print = TRUE))){
      if (gene.name == "NO_DATA" || sample.pattern == "NO_DATA"){
        if (gene.name == "NO_DATA") {
          cat("(\u2718) :gene.name is missing.\n\n")
        }
        if (sample.pattern == "NO_DATA") {
          cat("(\u2718) :sample.pattern is missing.\n\n")
        }
      } else {
        if (isTRUE(CheckInputDirFiles(input.path.prefix = input.path.prefix, gene.name = gene.name, sample.pattern = sample.pattern, print=TRUE))) {
          ExportPath()
          check.results <- ProgressGenesFiles(gene.name = gene.name, sample.pattern = sample.pattern, print=FALSE)
          if (isTRUE(check.results$gtf.file.logic.df) && isTRUE(check.results$fa.file.logic.df) && (check.results$fastq.gz.files.number.df != 0)){
            CreateHisat2Index(gene.name, sample.pattern)
            Hisat2AlignmentDefault(gene.name, sample.pattern)
            SamtoolsToBam(gene.name, sample.pattern)
            StringTieAssemble(gene.name, sample.pattern)
            StringTieMergeTrans(gene.name, sample.pattern)
            GffcompareRefSample(gene.name, sample.pattern)
            StringTieToBallgown(gene.name, sample.pattern)
            finals <- ProgressGenesFiles(gene.name, sample.pattern, print=TRUE)

            if (isTRUE(finals$gtf.file.logic.df) && isTRUE(finals$fa.file.logic.df) &&
                finals$fastq.gz.files.number.df != 0 &&
                finals$ht2.files.number.df != 0 &&
                finals$sam.files.number.df != 0 &&
                finals$bam.files.number.df != 0 &&
                finals$gtf.files.number.df != 0 &&
                isTRUE(finals$stringtie_merged.gtf.file.df) && finals$ballgown.dirs.number.df != 0) {
              cat(paste0("\n**************************************\n"))
              cat(paste0("************** Success! **************\n"))
              cat(paste0("**************************************\n"))
            }
          }
          else{
            cat(paste0("(\u2718) :Necessary files are lost.\n     Please check 'ref_genes/", gene.name, ".gtf' , 'ref_genome/", gene.name, ".fa' , 'samples_.fastq.gz/XXX_", gene.name, "_*.fastq.gz' are exit.\n\n" ))
          }
        }
      }
    }
  }
}
