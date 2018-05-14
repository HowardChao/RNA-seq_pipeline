#' rna seq pipline
#' @export
RNAseqPipeline <- function(path.prefix = "NOT_SET_YET", input.files.path = "NOT_SET_YET", gene_name = "NO_DATA") {
  if (isTRUE(SetPrefixPath(path.prefix))){
    MkdirAll()
    if (isTRUE(CheckDirAll(print = TRUE))){
      if (gene_name == "NO_DATA"){
        cat("(\u2718) :gene_name is missing.\n     Can't find the target sample files.\n\n")
      } else {
        if (isTRUE(CheckInputDirFiles(gene_name = gene_name, abs.input.dir = input.files.path, print=TRUE))) {
          CopyInputDir(gene_name, input.files.path)
          ExportPath()
          if (isTRUE(InstallAll())){
            check.results <- ProgressGenesFiles(gene_name = gene_name, print=FALSE)
            if (isTRUE(check.results$gtf.file.logic.df) && isTRUE(check.results$fa.file.logic.df) && (check.results$fastq.gz.files.number.df != 0)){
              CreateHisat2Index(gene_name)
              Hisat2AlignmentDefault(gene_name)
              SamtoolsToBam(gene_name)
              StringTieAssemble(gene_name)
              StringTieMergeTrans(gene_name)
              GffcompareRefSample(gene_name)
              StringTieToBallgown(gene_name)
              finals <- ProgressGenesFiles(gene_name, print=TRUE)

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
