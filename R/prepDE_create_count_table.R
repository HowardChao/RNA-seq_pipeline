#' converting stringtie ballogwn preprocessed data to count table
#' @export
PreDECountTable <- function(sample.pattern="NO_DATA", print=TRUE) {
  # ftp server : ftp://ftp.ccb.jhu.edu/pub/infphilo/hisat2/downloads/hisat2-2.1.0-source.zip
  if (sample.pattern == "NO_DATA") {
    cat("(\u2718) : sample.pattern is missing.\n\n")
  } else {
    if (isTRUE(CheckDirAll(print = FALSE))){
      cat("************** Installing prepDE.py ************\n")
      cat(paste0(pkg.global.path.prefix$data_path, "gene_data/ballgown/\n"))
      current.path <- getwd()
      setwd(paste0(pkg.global.path.prefix$data_path, "gene_data/ballgown/"))
      system2(command = 'curl', args = c('https://ccb.jhu.edu/software/stringtie/dl/prepDE.py', '--output', paste0(pkg.global.path.prefix$data_path, "gene_data/ballgown/prepDE.py")), stdout = "", wait = TRUE)
      cat(paste0("'", pkg.global.path.prefix$data_path, "gene_data/ballgown/prepDE.py' has been installed.\n\n"))
      sample.files <- list.files(paste0(pkg.global.path.prefix$data_path, "gene_data/ballgown/"), pattern = sample.pattern)
      write.content <- print(paste0(sample.files[1], " ", pkg.global.path.prefix$data_path, "gene_data/ballgown/", sample.files[1] ,"/", sample.files[1], ".gtf"))
      for(i in 2:length(sample.files)){
        print(paste0(sample.files[i], " ", pkg.global.path.prefix$data_path, "gene_data/ballgown/", sample.files[i],"/", sample.files[i], ".gtf"))
        write.content <- c(write.content, paste0(sample.files[i], " ", pkg.global.path.prefix$data_path, "gene_data/ballgown/", sample.files[i],"/", sample.files[i], ".gtf"))
      }
      print(write.content)
      write.file<-file(paste0(pkg.global.path.prefix$data_path, "gene_data/ballgown/sample_lst.txt"))
      writeLines(write.content, write.file)
      close(write.file)
      print(paste0(pkg.global.path.prefix$data_path, "gene_data/ballgown/prepDE.py -i ",  pkg.global.path.prefix$data_path, "gene_data/ballgown/sample_lst.txt"))
      system2(command = 'python2', args = paste0(pkg.global.path.prefix$data_path, "gene_data/ballgown/prepDE.py -i ",  pkg.global.path.prefix$data_path, "gene_data/ballgown/sample_lst.txt"))
      on.exit(setwd(current.path))
    }
  }
}

#' DEG analysis with edgeR
#' @export
DEGedgeRPlot <- function() {

}
