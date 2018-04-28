#' Check hisat2
#' Check whether hisat2 is installed on the workstation
#' @export
CheckHisat2 <- function(){
  hisat2.installed <- system2( command = 'hisat2', args = '-v', stdout = FALSE, stderr = FALSE)==0
  if(hisat2.installed == FALSE){
    cat("\'hisat2\' command is not found on the device. Please install it and add it to your PATH.")
  }
  else{
    print("True")
  }
}

#' Make RNAseq_bin/ directory
#' @export
MkdirRNAseq_bin <- function() {
  system2(command = 'mkdir', args = '~/RNAseq_bin/')
  cat("************** Creating binary directory ************\n")
  cat("'~/RNAseq_bin/' has been created.\n\n")
}

#' Install Hisat2
#' @export
InstallHisat2 <- function() {
  # ftp server : ftp://ftp.ccb.jhu.edu/pub/infphilo/hisat2/downloads/hisat2-2.1.0-Linux_x86_64.zip
  cat("************** Installing Hisat2 (hisat2-2.1.0-Linux_x86_64.zip) ************\n")
  #cat("Installing Hisat2 (hisat2-2.1.0-Linux_x86_64.zip) ...\n")
  current.path <- getwd()
  setwd("~/RNAseq_bin/")
  system2(command = 'curl', args = c('ftp.ccb.jhu.edu/pub/infphilo/hisat2/downloads/hisat2-2.1.0-Linux_x86_64.zip', '--output', 'hisat2-2.1.0-Linux_x86_64.zip'), stdout = "", wait = TRUE)
  on.exit(setwd(current.path))
  cat("'~/RNAseq_bin/hisat2-2.1.0-Linux_x86_64.zip' has been installed.\n\n")
}

#' Install StringTie
#' @export
InstallStringTie <- function() {
  # http://ccb.jhu.edu/software/stringtie/dl/stringtie-1.3.4d.tar.gz
  cat("************** Installing StringTie (stringtie-1.3.4d.tar.gz) ************\n")
  #cat("Installing StringTie (stringtie-1.3.4d.tar.gz) ...\n")
  current.path <- getwd()
  setwd("~/RNAseq_bin/")
  system2(command = 'curl', args = c('ccb.jhu.edu/software/stringtie/dl/stringtie-1.3.4d.tar.gz', '--output', 'stringtie-1.3.4d.tar.gz'), stdout = "", wait = TRUE)
  on.exit(setwd(current.path))
  cat("'~/RNAseq_bin/stringtie-1.3.4d.tar.gz' has been installed.\n\n")
}

#' Install gffcompare
#' @export
InstallGffcompare <- function() {
  # http://ccb.jhu.edu/software/stringtie/dl/gffread-0.9.4.tar.gz
  cat("************** Installing gffcompare (gffcompare-0.10.4.tar.gz) ************\n")
  #cat("Installing gffcompare (gffcompare-0.10.4.tar.gz) ...\n")
  current.path <- getwd()
  setwd("~/RNAseq_bin/")
  system2(command = 'curl', args = c('ccb.jhu.edu/software/stringtie/dl/gffcompare-0.10.4.tar.gz', '--output', 'gffcompare-0.10.4.tar.gz'), stdout = "", wait = TRUE)
  on.exit(setwd(current.path))
  cat("'~/RNAseq_bin/gffcompare-0.10.4.tar.gz' has been installed.\n\n")
}

#' Install samtools
#' @export
InstallSamtools <- function() {
  # https://github.com/samtools/samtools/releases/download/1.8/samtools-1.8.tar.bz2
  cat("************** Installing samtools (samtools-1.8.tar.bz2) ************\n")
  #cat("Installing samtools (samtools-1.8.tar.bz2) ...\n")
  current.path <- getwd()
  setwd("~/RNAseq_bin/")
  system2(command = 'curl', args = c('-L', 'https://github.com/samtools/samtools/releases/download/1.8/samtools-1.8.tar.bz2', '>', 'samtools-1.8.tar.bz2'), stdout = "", wait = TRUE)
  on.exit(setwd(current.path))
  cat("'~/RNAseq_bin/samtools-1.8.tar.bz2' has been installed.\n\n")
}

#' Install Hisat2, StringTie, gffcompare, samtools
#' @export
InstallAll <- function() {
  MkdirRNAseq_bin()
  InstallHisat2()
  InstallStringTie()
  InstallGffcompare()
  InstallSamtools()
}
