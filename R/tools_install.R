home.path <- system2(command = 'echo', args = "$HOME", stdout = TRUE)
pkg.global.path.prefix <- new.env()
pkg.global.path.prefix$data_path <- "NOT_SET_YET"

#' Install Hisat2
#' @export
InstallHisat2 <- function() {
  # ftp server : ftp://ftp.ccb.jhu.edu/pub/infphilo/hisat2/downloads/hisat2-2.1.0-Linux_x86_64.zip
  if (isTRUE(CheckPrefixPath(pkg.global.path.prefix$data_path, print = FALSE))){
    cat("************** Installing Hisat2 (hisat2-2.1.0-Linux_x86_64.zip) ************\n")
    current.path <- getwd()
    cat(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Download/"))
    setwd(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Download/"))
    system2(command = 'curl', args = c('ftp.ccb.jhu.edu/pub/infphilo/hisat2/downloads/hisat2-2.1.0-Linux_x86_64.zip', '--output', 'hisat2-2.1.0-Linux_x86_64.zip'), stdout = "", wait = TRUE)
    on.exit(setwd(current.path))
    cat(paste0("'", pkg.global.path.prefix$data_path, "RNAseq_bin/Download/hisat2-2.1.0-Linux_x86_64.zip' has been installed.\n\n"))
  }
}

#' Install StringTie
#' @export
InstallStringTie <- function() {
  # http://ccb.jhu.edu/software/stringtie/dl/stringtie-1.3.4d.tar.gz
  if (isTRUE(CheckPrefixPath(pkg.global.path.prefix$data_path, print = FALSE))) {
    cat("************** Installing StringTie (stringtie-1.3.4d.tar.gz) ************\n")
    current.path <- getwd()
    setwd(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Download/"))
    system2(command = 'curl', args = c('ccb.jhu.edu/software/stringtie/dl/stringtie-1.3.4d.tar.gz', '--output', 'stringtie-1.3.4d.tar.gz'), stdout = "", wait = TRUE)
    on.exit(setwd(current.path))
    cat(paste0("'", pkg.global.path.prefix$data_path, "RNAseq_bin/Download/stringtie-1.3.4d.tar.gz' has been installed.\n\n"))
  }
}

#' Install Gffcompare
#' @export
InstallGffcompare <- function() {
  # http://ccb.jhu.edu/software/stringtie/dl/gffcompare-0.10.4.tar.gz
  if (isTRUE(CheckPrefixPath(pkg.global.path.prefix$data_path, print = FALSE))) {
    cat("************** Installing Gffcompare (gffcompare-0.10.4.tar.gz) ************\n")
    current.path <- getwd()
    setwd(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Download/"))
    system2(command = 'curl', args = c('ccb.jhu.edu/software/stringtie/dl/gffcompare-0.10.4.tar.gz', '--output', 'gffcompare-0.10.4.tar.gz'), stdout = "", wait = TRUE)
    on.exit(setwd(current.path))
    cat(paste0("'", pkg.global.path.prefix$data_path, "RNAseq_bin/Download/gffcompare-0.10.4.tar.gz' has been installed.\n\n"))
  }
}

#' Install Samtools
#' @export
InstallSamtools <- function() {
  # https://github.com/samtools/samtools/releases/download/1.8/samtools-1.8.tar.bz2
  if (isTRUE(CheckPrefixPath(pkg.global.path.prefix$data_path, print = FALSE))) {
    cat("************** Installing Samtools (samtools-1.8.tar.bz2) ************\n")
    current.path <- getwd()
    setwd(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Download/"))
    system2(command = 'curl', args = c('-L', 'https://github.com/samtools/samtools/releases/download/1.8/samtools-1.8.tar.bz2', '>', 'samtools-1.8.tar.bz2'), stdout = "", wait = TRUE)
    on.exit(setwd(current.path))
    cat(paste0("'", pkg.global.path.prefix$data_path, "RNAseq_bin/Download/samtools-1.8.tar.bz2' has been installed.\n\n"))
  }
}

#' Unpack Hisat2
#' @export
UnpackHisat2 <- function() {
  if (isTRUE(CheckPrefixPath(pkg.global.path.prefix$data_path, print = FALSE))) {
    cat("************** Unpacking Hisat2 (hisat2-2.1.0-Linux_x86_64.zip) ************\n")
    current.path <- getwd()
    setwd(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/"))
    system2(command = 'unzip', args = paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Download/hisat2-2.1.0-Linux_x86_64.zip"))
    on.exit(setwd(current.path))
    cat(paste0("Hisat2 has been unpacked. ('", pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/hisat2-2.1.0/')"), "\n\n")
  }
}

#' Unpack StringTie
#' @export
UnpackStringTie <- function() {
  if (isTRUE(CheckPrefixPath(pkg.global.path.prefix$data_path, print = FALSE))) {
    cat("************** Unpacking StringTie (stringtie-1.3.4d.tar.gz) ************\n")
    current.path <- getwd()
    setwd(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/"))
    system2(command = 'tar', args = c("xvzf", paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Download/stringtie-1.3.4d.tar.gz")))
    on.exit(setwd(current.path))
    cat(paste0("StringTie has been unpacked. ('", pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/stringtie-1.3.4d.tar.gz/')"), "\n\n")
  }
}

#' Unpack Gffcompare
#' @export
UnpackGffcompare <- function() {
  if (isTRUE(CheckPrefixPath(pkg.global.path.prefix$data_path, print = FALSE))) {
    cat("************** Unpacking Gffcompare (gffcompare-0.10.4.tar.gz) ************\n")
    current.path <- getwd()
    setwd(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/"))
    system2(command = 'tar', args = c("xvzf", paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Download/gffcompare-0.10.4.tar.gz")))
    on.exit(setwd(current.path))
    cat(paste0("Gffcompare has been unpacked. ('", pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/gffcompare-0.10.4/')"), "\n\n")
  }
}

#' Unpack Samtools
#' @export
UnpackSamtools <- function() {
  if (isTRUE(CheckPrefixPath(pkg.global.path.prefix$data_path, print = FALSE))) {
    cat("************** Unpacking Samtools (samtools-1.8.tar.bz2) ************\n")
    current.path <- getwd()
    setwd(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/"))
    system2(command = 'tar', args = c("jxvf",  paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Download/samtools-1.8.tar.bz2")))
    on.exit(setwd(current.path))
    cat(paste0("Samtools has been unpacked. ('", pkg.global.path.prefix$data_path, "/RNAseq_bin/Unpacked/samtools-1.8/')"), "\n\n")
  }
}

#' Making Hisat2 binary
#' @export
BinaryHisat2 <- function() {
  if (isTRUE(CheckPrefixPath(pkg.global.path.prefix$data_path, print = FALSE))) {
    cat("************** Moving Hisat2 Binary ************\n")
    current.path <- getwd()
    setwd(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/hisat2-2.1.0/"))
    system2(command = 'cp', args = c("hisat2*", "*.py", paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/")), stderr = FALSE)
    cat("hisat2 binaries are in: ", paste0("'", pkg.global.path.prefix$data_path, "RNAseq_bin/'\n\n"))
    on.exit(setwd(current.path))
  }
}

#' Making StringTie binary
#' @export
BinaryStringTie <- function() {
  if (isTRUE(CheckPrefixPath(pkg.global.path.prefix$data_path, print = FALSE))) {
    cat("************** Making StringTie Binary ************\n")
    current.path <- getwd()
    setwd(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/stringtie-1.3.4d/"))
    system2(command = 'make', args = c("clean", "release"), stderr = FALSE)
    system2(command = 'cp', args = c("stringtie", paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/")), stderr = FALSE)
    cat("stringtie binaries are in: ", paste0("'", pkg.global.path.prefix$data_path, "RNAseq_bin/'\n\n"))
    on.exit(setwd(current.path))
  }
}

#' Making Gffcompare binary
#' @export
BinaryGffcompare <- function() {
  if (isTRUE(CheckPrefixPath(pkg.global.path.prefix$data_path, print = FALSE))) {
    cat("************** Making Gffcompare Binary ************\n")
    current.path <- getwd()
    setwd(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/gffcompare-0.10.4/"))
    system2(command = 'make', stderr = FALSE)
    system2(command = 'cp', args = c("gffcompare", paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/")), stderr = FALSE)
    cat("gffcompare binaries are in: ", paste0("'", pkg.global.path.prefix$data_path, "RNAseq_bin/'\n\n"))
    on.exit(setwd(current.path))
  }
}


#' Making Samtools binary
#' @export
BinarySamtools <- function() {
  if (isTRUE(CheckPrefixPath(pkg.global.path.prefix$data_path, print = FALSE))) {
    cat("************** Making Samtools Binary ************\n")
    current.path <- getwd()
    setwd(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/samtools-1.8/"))
    system2(command = 'make', args = "clean", stderr = FALSE)
    system2(command = 'make')
    system2(command = 'cp', args = c("samtools", paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/")), stdout = FALSE, stderr = FALSE)
    cat("samtools binaries are in: ", paste0("'", pkg.global.path.prefix$data_path, "RNAseq_bin/'\n\n"))
    on.exit(setwd(current.path))
  }
}

#' Install Hisat2, StringTie, Gffcompare, Samtools
#' @export
InstallAll <- function() {
  # give the function the location to RNAoutput file
  # fix cat problem
  # the the user choose the version(之後再說)

  # try stringtie
  #sink("RNAseq_program_install_report.txt")
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
  #sink()
}
