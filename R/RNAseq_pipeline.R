#' rna seq pipline
#' @export
RNAseqPipeline <- function(path.prefix = home.path, gene_name = "NO_DATA") {
  if (isTRUE(SetPrefixPath(path.prefix))){
    MkdirAll()
    if (isTRUE(CheckDirAll(print = FALSE))){
      if (gene_name == "NO_DATA"){
        cat("(X) :gene_name is missing.\n     Can't find the target sample files.\n\n")
      } else {
        if (isTRUE(InstallAll())){
          check.results <- CheckSampleGenesFiles(gene_name = gene_name)
          if (isTRUE(check.results$gtf.file.logic.df) && isTRUE(check.results$fa.file.logic.df) && (check.results$fastq.gz.files.number.df != 0)){
            CreateHisat2Index(gene_name)
            Hisat2AlignmentDefault(gene_name)
            SamtoolsToBam(gene_name)
            StringTieAssemble(gene_name)
            StringTieMergeTrans(gene_name)
            GffcompareRefSample(gene_name)
            StringTieToBallgown(gene_name)
            CheckSampleGenesFiles(gene_name)
          }
          else{
            cat(paste0("(X) :Necessary files are lost.\n     Please check 'ref_genes/", gene_name, ".gtf' , 'ref_genome/", gene_name, ".fa' , 'samples_.fastq.gz/XXX_", gene_name, "_*.fastq.gz' are exit.\n\n" ))
          }
        }
      }
    }
  }
}
