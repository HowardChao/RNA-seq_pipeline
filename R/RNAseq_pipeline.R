#' rna seq pipline
#' @export
RNAseqPipeline <- function(path.prefix = "NOT_SET_YET", input.files.path = "NOT_SET_YET", gene_name = "NO_DATA", sample_prefix = "NO_DATA") {
  if (isTRUE(SetPrefixPath(path.prefix))){
    MkdirAll()
    if (isTRUE(CheckDirAll(print = TRUE))){
      if (gene_name == "NO_DATA" || sample_prefix == "NO_DATA"){
        if (gene_name == "NO_DATA") {
          cat("(\u2718) :gene_name is missing.\n\n")
        }
        if (sample_prefix == "NO_DATA") {
          cat("(\u2718) :sample_prefix is missing.\n\n")
        }
      } else {
        if (isTRUE(CheckInputDirFiles(gene_name = gene_name, sample_prefix = sample_prefix, abs.input.dir = input.files.path, print=TRUE))) {
          CopyInputDir(gene_name, sample_prefix ,input.files.path)
          ExportPath()
          if (isTRUE(InstallAll())){
            check.results <- ProgressGenesFiles(gene_name = gene_name, sample_prefix = sample_prefix, print=FALSE)
            if (isTRUE(check.results$gtf.file.logic.df) && isTRUE(check.results$fa.file.logic.df) && (check.results$fastq.gz.files.number.df != 0)){
              CreateHisat2Index(gene_name, sample_prefix)
              Hisat2AlignmentDefault(gene_name, sample_prefix)
              SamtoolsToBam(gene_name, sample_prefix)
              StringTieAssemble(gene_name, sample_prefix)
              StringTieMergeTrans(gene_name, sample_prefix)
              GffcompareRefSample(gene_name, sample_prefix)
              StringTieToBallgown(gene_name, sample_prefix)
              finals <- ProgressGenesFiles(gene_name, sample_prefix, print=TRUE)

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
              cat(paste0("(\u2718) :Necessary files are lost.\n     Please check 'ref_genes/", gene_name, ".gtf' , 'ref_genome/", gene_name, ".fa' , 'samples_.fastq.gz/XXX_", gene_name, "_*.fastq.gz' are exit.\n\n" ))
            }
          }
        }
      }
    }
  }
}
