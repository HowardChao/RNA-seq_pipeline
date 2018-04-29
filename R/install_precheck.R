home.path <- system2(command = 'echo', args = "$HOME", stdout = TRUE)
#' Check Hisat2
#' Check whether hisat2 is installed on the workstation
#' @export
CheckHisat2 <- function(){
  cat("************** Checking hisat2 command ************\n")
  hisat2.installed <- system('hisat2 --version')==0
  if( isTRUE(hisat2.installed)){
    cat("'hisat2' is installed\n\n")
  }
  else{
    cat("\'hisat2\' command is not found on this device. Please run 'InstallAll()' to install the necessary programs\n\n")
  }
}

#' Check StringTie
#' Check whether StringTie is installed on the workstation
#' @export
CheckStringTie <- function(){
  cat("************** Checking stringtie command ************\n")
  stringtie.installed <- system( 'stringtie --version')==0
  if( isTRUE(stringtie.installed)){
    cat("'stringtie' is installed\n\n")
  }
  else{
    cat("\'stringtie\' command is not found on this device. Please run 'InstallAll()' to install the necessary programs\n\n")
  }
}

#' Check Gffcompare
#' Check whether Gffcompare is installed on the workstation
#' @export
CheckGffcompare <- function() {
  cat("************** Checking gffcompare command ************\n")
  gffcompare.old <- system( 'gffcompare --version')==0
  if( isTRUE(gffcompare.old)){
    cat("'gffcompare' is installed\n\n")
  }
  else{
    cat("\'gffcompare\' command is not found on this device. Please run 'InstallAll()' to install the necessary programs\n\n")
  }
}

#' Check Samtools
#' Check whether Samtools is installed on the workstation
#' @export
CheckSamtools <- function(){
  cat("************** Checking samtools command ************\n")
  samtools.old <- system( 'samtools --version')==0
  if( isTRUE(samtools.old)){
    cat("'samtools' is installed\n\n")
  }
  else{
    cat("\'samtools\' command is not found on this device. Please run 'InstallAll()' to install the necessary programs\n\n")
  }
}

#' Check whether programs are installed
#' @export
CheckAll <- function() {
  CheckHisat2()
  CheckStringTie()
  CheckGffcompare()
  CheckSamtools()
}

#' Make RNAseq_bin/ directory
#' @export
MkdirRNAseq_bin <- function() {
  system2(command = 'mkdir', args = '~/RNAseq_bin/')
  system2(command = 'mkdir', args = '~/RNAseq_bin/Download/')
  system2(command = 'mkdir', args = '~/RNAseq_bin/Unpacked/')
  cat("************** Creating binary directory ************\n")
  cat(paste0("'", home.path, "/RNAseq_bin/' has been created.\n"))
  cat(paste0("'", home.path, "/RNAseq_bin/Download/' has been created.\n"))
  cat(paste0("'", home.path, "/RNAseq_bin/Unpacked/' has been created.\n\n"))
}

#' Add '~/RNAseq_bin/ to R environment "PATH"
#' @export
ExportPath <- function() {
  cat("************** Adding PATH to R environment ************\n")
  old.path <- Sys.getenv("PATH")
  Sys.setenv(
    PATH = paste(old.path, paste0(home.path, "/RNAseq_bin"), sep = ":")
  )
  cat("R environment 'PATH': ", Sys.getenv("PATH"), "\n\n")
}

#' Install Hisat2
#' @export
InstallHisat2 <- function() {
  # ftp server : ftp://ftp.ccb.jhu.edu/pub/infphilo/hisat2/downloads/hisat2-2.1.0-Linux_x86_64.zip
  cat("************** Installing Hisat2 (hisat2-2.1.0-Linux_x86_64.zip) ************\n")
  current.path <- getwd()
  setwd("~/RNAseq_bin/Download/")
  system2(command = 'curl', args = c('ftp.ccb.jhu.edu/pub/infphilo/hisat2/downloads/hisat2-2.1.0-Linux_x86_64.zip', '--output', 'hisat2-2.1.0-Linux_x86_64.zip'), stdout = "", wait = TRUE)
  on.exit(setwd(current.path))
  cat(paste0("'", home.path, "/RNAseq_bin/Download/hisat2-2.1.0-Linux_x86_64.zip' has been installed.\n\n"))
}

#' Install StringTie
#' @export
InstallStringTie <- function() {
  # http://ccb.jhu.edu/software/stringtie/dl/stringtie-1.3.4d.tar.gz
  cat("************** Installing StringTie (stringtie-1.3.4d.tar.gz) ************\n")
  current.path <- getwd()
  setwd("~/RNAseq_bin/Download/")
  system2(command = 'curl', args = c('ccb.jhu.edu/software/stringtie/dl/stringtie-1.3.4d.tar.gz', '--output', 'stringtie-1.3.4d.tar.gz'), stdout = "", wait = TRUE)
  on.exit(setwd(current.path))
  cat(paste0("'", home.path, "/RNAseq_bin/Download/stringtie-1.3.4d.tar.gz' has been installed.\n\n"))
}

#' Install Gffcompare
#' @export
InstallGffcompare <- function() {
  # http://ccb.jhu.edu/software/stringtie/dl/gffcompare-0.10.4.tar.gz
  cat("************** Installing Gffcompare (gffcompare-0.10.4.tar.gz) ************\n")
  current.path <- getwd()
  setwd("~/RNAseq_bin/Download/")
  system2(command = 'curl', args = c('ccb.jhu.edu/software/stringtie/dl/gffcompare-0.10.4.tar.gz', '--output', 'gffcompare-0.10.4.tar.gz'), stdout = "", wait = TRUE)
  on.exit(setwd(current.path))
  cat(paste0("'", home.path, "/RNAseq_bin/Download/gffcompare-0.10.4.tar.gz' has been installed.\n\n"))
}

#' Install Samtools
#' @export
InstallSamtools <- function() {
  # https://github.com/samtools/samtools/releases/download/1.8/samtools-1.8.tar.bz2
  cat("************** Installing Samtools (samtools-1.8.tar.bz2) ************\n")
  current.path <- getwd()
  setwd("~/RNAseq_bin/Download/")
  system2(command = 'curl', args = c('-L', 'https://github.com/samtools/samtools/releases/download/1.8/samtools-1.8.tar.bz2', '>', 'samtools-1.8.tar.bz2'), stdout = "", wait = TRUE)
  on.exit(setwd(current.path))
  cat(paste0("'", home.path, "/RNAseq_bin/Download/samtools-1.8.tar.bz2' has been installed.\n\n"))
}

#' Unpack Hisat2
#' @export
UnpackHisat2 <- function() {
  cat("************** Unpacking Hisat2 (hisat2-2.1.0-Linux_x86_64.zip) ************\n")
  current.path <- getwd()
  setwd("~/RNAseq_bin/Unpacked/")
  system2(command = 'unzip', args = "~/RNAseq_bin/Download/hisat2-2.1.0-Linux_x86_64.zip")
  on.exit(setwd(current.path))
  cat(paste0("Hisat2 has been unpacked. ('", home.path, "/RNAseq_bin/Unpacked/hisat2-2.1.0/')"), "\n\n")
}

#' Unpack StringTie
#' @export
UnpackStringTie <- function() {
  cat("************** Unpacking StringTie (stringtie-1.3.4d.tar.gz) ************\n")
  current.path <- getwd()
  setwd("~/RNAseq_bin/Unpacked")
  system2(command = 'tar', args = c("xvzf", "~/RNAseq_bin/Download/stringtie-1.3.4d.tar.gz"))
  on.exit(setwd(current.path))
  cat(paste0("StringTie has been unpacked. ('", home.path, "/RNAseq_bin/Unpacked/stringtie-1.3.4d.tar.gz/')"), "\n\n")
}

#' Unpack Gffcompare
#' @export
UnpackGffcompare <- function() {
  cat("************** Unpacking Gffcompare (gffcompare-0.10.4.tar.gz) ************\n")
  current.path <- getwd()
  setwd("~/RNAseq_bin/Unpacked/")
  system2(command = 'tar', args = c("xvzf", "~/RNAseq_bin/Download/gffcompare-0.10.4.tar.gz"))
  on.exit(setwd(current.path))
  cat(paste0("Gffcompare has been unpacked. ('", home.path, "/RNAseq_bin/Unpacked/gffcompare-0.10.4/')"), "\n\n")
}

#' Unpack Samtools
#' @export
UnpackSamtools <- function() {
  cat("************** Unpacking Samtools (samtools-1.8.tar.bz2) ************\n")
  current.path <- getwd()
  setwd("~/RNAseq_bin/Unpacked/")
  system2(command = 'tar', args = c("jxvf", "~/RNAseq_bin/Download/samtools-1.8.tar.bz2"))
  on.exit(setwd(current.path))
  cat(paste0("Samtools has been unpacked. ('", home.path, "/RNAseq_bin/Unpacked/samtools-1.8/')"), "\n\n")
}

#' Making Hisat2 binary
#' @export
BinaryHisat2 <- function() {
  cat("************** Moving Hisat2 Binary ************\n")
  current.path <- getwd()
  setwd("~/RNAseq_bin/Unpacked/hisat2-2.1.0/")
  system2(command = 'cp', args = c("hisat2*", "*.py", paste0(home.path, "/RNAseq_bin/")), stderr = FALSE)
  cat("hisat2 binaries are in: ", paste0("'", home.path, "/RNAseq_bin/'\n\n"))
  on.exit(setwd(current.path))
}

#' Making StringTie binary
#' @export
BinaryStringTie <- function() {
  cat("************** Making StringTie Binary ************\n")
  current.path <- getwd()
  setwd("~/RNAseq_bin/Unpacked/stringtie-1.3.4d/")
  system2(command = 'make', args = c("clean", "release"), stderr = FALSE)
  system2(command = 'cp', args = c("stringtie", paste0(home.path, "/RNAseq_bin/")), stderr = FALSE)
  cat("stringtie binaries are in: ", paste0("'", home.path, "/RNAseq_bin/'\n\n"))
  on.exit(setwd(current.path))
}

#' Making Gffcompare binary
#' @export
BinaryGffcompare <- function() {
  cat("************** Making Gffcompare Binary ************\n")
  current.path <- getwd()
  setwd("~/RNAseq_bin/Unpacked/gffcompare-0.10.4/")
  system2(command = 'make', stderr = FALSE)
  system2(command = 'cp', args = c("gffcompare", paste0(home.path, "/RNAseq_bin/")), stderr = FALSE)
  cat("gffcompare binaries are in: ", paste0("'", home.path, "/RNAseq_bin/'\n\n"))
  on.exit(setwd(current.path))
}


#' Making Samtools binary
#' @export
BinarySamtools <- function() {
  cat("************** Making Samtools Binary ************\n")
  current.path <- getwd()
  setwd("~/RNAseq_bin/Unpacked/samtools-1.8/")
  system2(command = 'make', args = "clean", stderr = FALSE)
  system2(command = 'make')
  system2(command = 'cp', args = c("samtools", paste0(home.path, "/RNAseq_bin/")), stdout = FALSE, stderr = FALSE)
  cat("samtools binaries are in: ", paste0("'", home.path, "/RNAseq_bin/'\n\n"))
  on.exit(setwd(current.path))
}


#' Install Hisat2, StringTie, Gffcompare, Samtools
#' @export
InstallAll <- function() {
  sink("RNAseq_program_install_report.txt")
  MkdirRNAseq_bin()
  ExportPath()
  InstallHisat2()
  InstallStringTie()
  InstallGffcompare()
  InstallSamtools()
  UnpackHisat2()
  UnpackStringTie()
  UnpackGffcompare()
  UnpackSamtools()
  BinaryHisat2()
  BinaryStringTie()
  BinaryGffcompare()
  BinarySamtools()
  CheckAll()
  sink()
}
