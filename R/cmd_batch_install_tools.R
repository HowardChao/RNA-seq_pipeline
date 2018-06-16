#' install all the tools and create a binary library.
#' @export
InstallToolsCMD <- function(path.prefix = "NOT_SET_YET", input.path.prefix = "NOT_SET_YET", gene.name = "NO_DATA", sample.pattern = "NO_DATA") {
  if (path.prefix == "NOT_SET_YET" || input.path.prefix == "NOT_SET_YET" ||gene.name == "NO_DATA" || sample.pattern == "NO_DATA"){
    if (path.prefix == "NOT_SET_YET") {
      cat("(\u2718) : 'path.prefix' is missing.\n\n")
    }
    if (input.path.prefix == "NOT_SET_YET") {
      cat("(\u2718) : 'input.path.prefix' is missing.\n\n")
    }
    if (gene.name == "NO_DATA") {
      cat("(\u2718) : 'gene.name' is missing.\n\n")
    }
    if (sample.pattern == "NO_DATA") {
      cat("(\u2718) : 'sample.pattern' is missing.\n\n")
    }
  } else {
    if (isTRUE(SetPrefixPath(path.prefix, print = FALSE))) {
      if (isTRUE(CheckInputDirFiles(input.path.prefix = input.path.prefix, gene.name = gene.name, sample.pattern = sample.pattern, print = FALSE))) {
        # CheckInputDirFiles will determin whether 'gene.name' and 'sample.pattern' is valid !!
        MkdirAll()
        if (isTRUE(CheckDirAll(print = TRUE))){
          r_script.dir <- dir.create(file.path(paste0(pkg.global.path.prefix$data_path, 'Rscript/')), showWarnings = FALSE) == 0
          r_script.out.dir <- dir.create(file.path(paste0(pkg.global.path.prefix$data_path, 'Rscript_out/')), showWarnings = FALSE) == 0
          fileConn<-file(paste0(pkg.global.path.prefix$data_path, "Rscript/INSTALL_TOOLS.R"))
          first <- "library(RNASeq)"
          second <- paste0("SetPrefixPath('", pkg.global.path.prefix$data_path, "')")
          third <- paste0('CheckInputDirFiles(input.path.prefix = "', input.path.prefix, '", gene.name = "', gene.name, '", sample.pattern = "', sample.pattern, '")')
          fourth <- paste0("CopyInputDir(input.path.prefix = '", pkg.global.path.prefix$input.files, "', gene.name = '", gene.name, "', sample.pattern = '", sample.pattern, "')")
          fifth <- "InstallAll()"
          sixth <-  "ExportPath()"
          seventh <- "CheckToolAll()"
          writeLines(c(first, second, third, fourth, fifth, sixth, seventh), fileConn)
          close(fileConn)
          system2(command = 'nohup', args = paste0("R CMD BATCH ", pkg.global.path.prefix$data_path, "/Rscript/INSTALL_TOOLS.R ", pkg.global.path.prefix$data_path, "/Rscript_out/INSTALL_TOOLS.Rout"), stdout = "", wait = FALSE)
          cat(paste0("\u2605 Tools are installing in the background. Check current progress in '", pkg.global.path.prefix$data_path, "/Rscript_out/INSTALL_TOOLS.Rout'\n\n"))
        }
      }
    }
  }
}

#' get operating system
#' @export
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

#' Install Hisat2 binay
#' @export
InstallHisat2Bianry <- function(){
  os <- as.character(get_os())
  url <- 'ftp://ftp.ccb.jhu.edu/pub/infphilo/hisat2/downloads/'
  # setwd(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/"))
  if (os == "linux"){
    os.file.name.zip <- "hisat2-2.1.0-Linux_x86_64.zip"
    os.file.name <- "hisat2-2.1.0"
    url <- paste0(url, os.file.name.zip)
  } else if (os == "osx"){
    os.file.name <- "hisat2-2.1.0-OSX_x86_64.zip"
    os.file.name <- "hisat2-2.1.0"
    url <- paste0(url, os.file.name)
  } else if (os == "windows"){
    stop("Hisat2 is not supporting windows.\n\n")
    return(FALSE)
  } else {
    stop("Unknow operating system.\n\n")
    return(FALSE)
  }
  cat(paste0("************** Installing Hisat2 ", "(", os.file.name.zip, ") ************\n"))
  download.file(url, paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Download/", os.file.name.zip))
  cat(paste0("\n************** Unpacking Hisat2 ", "(", os.file.name.zip, ") ************\n"))
  system2(command = 'unzip', args = paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Download/", os.file.name.zip," -d ", pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/"))
  current.path <- getwd()
  setwd(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/", os.file.name, "/"))
  cat("\n************** Moving Hisat2 Binary ************")
  system2(command = 'cp', args = c("hisat2*", "*.py", paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/")), stderr = FALSE)
  on.exit(setwd(current.path))
  cat(paste0("\n'", pkg.global.path.prefix$data_path, "RNAseq_bin/Download/", os.file.name.zip,"' has been installed.\n"))
  cat(paste0("Hisat2 has been unpacked. ('", pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/", os.file.name, "/')"), "\n\n")
  return(TRUE)
}

#' Install stringtie binary
#' @export
InstallStringTieBinary <- function(){
  os <- as.character(get_os())
  url <- 'http://ccb.jhu.edu/software/stringtie/dl/'
  if (os == "linux"){
    os.file.name.zip <- "stringtie-1.3.4d.Linux_x86_64.tar.gz"
    os.file.name <- "stringtie-1.3.4d.Linux_x86_64"
    url <- paste0(url, os.file.name.zip)
  } else if (os == "osx"){
    os.file.name <- "stringtie-1.3.4d.OSX_x86_64.tar.gz"
    os.file.name <- "stringtie-1.3.4d.OSX_x86_64"
    url <- paste0(url, os.file.name)
  } else if (os == "windows"){
    stop("Stringtie is not supporting windows.\n\n")
    return(FALSE)
  } else {
    stop("Unknow operating system.\n\n")
    return(FALSE)
  }
  cat(paste0("************** Installing stringtie ", "(", os.file.name.zip, ") ************\n"))
  download.file(url, paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Download/", os.file.name.zip))
  cat(paste0("\n************** Unpacking stringtie ", "(", os.file.name.zip, ") ************\n"))
  system2(command = 'tar', args = c("xvzf", paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Download/", os.file.name.zip), "-C", paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/")))
  current.path <- getwd()
  setwd(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/", os.file.name))
  cat("\n************** Moving stringtie Binary ************")
  system2(command = 'cp', args = c("stringtie", paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/")), stderr = FALSE)
  on.exit(setwd(current.path))
  cat(paste0("\n'", pkg.global.path.prefix$data_path, "RNAseq_bin/Download/", os.file.name.zip,"' has been installed.\n"))
  cat(paste0("StringTie has been unpacked. ('", pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/", os.file.name, "')"), "\n\n")
  return(TRUE)
}

#' Install Gffcompare binary
#' @export
InstallGffcompareBinary <- function(){
  os <- as.character(get_os())
  current.path <- getwd()
  url <- 'http://ccb.jhu.edu/software/stringtie/dl/'
  if (os == "linux"){
    os.file.name.zip <- "gffcompare-0.10.4.Linux_x86_64.tar.gz"
    os.file.name <- "gffcompare-0.10.4.Linux_x86_64"
    url <- paste0(url, os.file.name.zip)
  } else if (os == "osx"){
    os.file.name <- "gffcompare-0.10.4.OSX_x86_64.tar.gz"
    os.file.name <- "gffcompare-0.10.4.OSX_x86_64"
    url <- paste0(url, os.file.name)
  } else if (os == "windows"){
    stop("Gffcompare is not supporting windows.\n\n")
    return(FALSE)
  } else {
    stop("Unknow operating system.\n\n")
    return(FALSE)
  }
  cat(paste0("************** Installing gffcompare ", "(", os.file.name.zip, ") ************\n"))
  download.file(url, paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Download/", os.file.name.zip))
  cat(paste0("\n************** Unpacking gffcompare ", "(", os.file.name.zip, ") ************\n"))
  system2(command = 'tar', args = c("xvzf", paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Download/", os.file.name.zip), "-C", paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/")))
  current.path <- getwd()
  setwd(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/", os.file.name))
  cat("\n************** Moving gffcompare Binary ************")
  system2(command = 'cp', args = c("gffcompare", paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/")), stderr = FALSE)
  on.exit(setwd(current.path))
  cat(paste0("\n'", pkg.global.path.prefix$data_path, "RNAseq_bin/Download/", os.file.name.zip,"' has been installed.\n"))
  cat(paste0("Gffcompare has been unpacked. ('", pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/", os.file.name, "')"), "\n\n")
  return(TRUE)
}

#' Install Samtools binary
#' @export
InstallSamtoolsBinary <- function(){
  os <- as.character(get_os())
  current.path <- getwd()
  url <- 'https://github.com/samtools/samtools/releases/download/1.8/samtools-1.8.tar.bz2'
  if (os == "linux"){
    os.file.name.zip <- "samtools-1.8.tar.bz2"
    os.file.name <- "samtools-1.8"
    #url <- paste0(url, os.file.name.zip)
  } else if (os == "osx"){
    os.file.name <- "samtools-1.8.tar.bz2"
    os.file.name <- "samtools-1.8"
    #url <- paste0(url, os.file.name)
  } else if (os == "windows"){
    stop("Samtools is not supporting windows.\n\n")
    return(FALSE)
  } else {
    stop("Unknow operating system.\n\n")
    return(FALSE)
  }
  cat(paste0("************** Installing samtools ", "(", os.file.name.zip, ") ************\n"))
  system2(command = 'curl', args = c('-L', 'https://github.com/samtools/samtools/releases/download/1.8/samtools-1.8.tar.bz2', '>', paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Download/", os.file.name.zip)), stdout = "", wait = TRUE)
  #download.file(url, paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Download/", os.file.name.zip))
  cat(paste0("\n************** Unpacking samtools ", "(", os.file.name.zip, ") ************\n"))
  system2(command = 'tar', args = c("jxvf", paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Download/", os.file.name.zip), "-C", paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/")))
  current.path <- getwd()
  cat(paste0("\n************** Making samtools ", "(", os.file.name, ") ************"))
  setwd(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/", os.file.name))
  system2(command = 'make', args = "clean", stderr = FALSE)
  system2(command = 'make')
  cat("\n************** Moving samtools Binary ************")
  file.copy("samtools", paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/"))
  on.exit(setwd(current.path))
  cat(paste0("\n'", pkg.global.path.prefix$data_path, "RNAseq_bin/Download/", os.file.name.zip,"' has been installed.\n"))
  cat(paste0("Samtools has been unpacked. ('", pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/", os.file.name, "')"), "\n\n")
  return(TRUE)
}

#'
#' Install Hisat2, StringTie, Gffcompare, Samtools
InstallAll <- function() {
  cat("\u2618\u2618\u2618\u2618\u2618\u2618\u2618\u2618  Start installing ... \u2618\u2618\u2618\u2618\u2618\u2618\u2618\u2618\n")
  cat("   \u261E\u261E  \u25CF'hisat2', \u25CF'stringtie', \u25CF'gffcompare', \u25CF'samtools' will be installed. ... \n")
  cat(paste0("   \u261E\u261E  Compressed files will be in '", pkg.global.path.prefix$data_path, "RNAseq_bin/Download/'"), "\n")
  cat(paste0("   \u261E\u261E  Unpacked files will be in '", pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/'"), "\n")
  cat(paste0("   \u261E\u261E  Binary files will be copied to '", pkg.global.path.prefix$data_path, "RNAseq_bin/'"), "\n\n")
  InstallHisat2Bianry()
  InstallStringTieBinary()
  InstallGffcompareBinary()
  InstallSamtoolsBinary()
  #return(CheckToolAll(print=FALSE))
}


#' Check 'hisat2'
#' Check whether 'hisat2' is installed on the workstation
#' @export
CheckHisat2 <- function(print=TRUE){
  if (print) {
    cat("************** Checking hisat2 command ************\n")
  }
  hisat2.installed <- system('hisat2 --version', ignore.stdout = !print , ignore.stderr = !print)==0
  if( isTRUE(hisat2.installed)){
    if(isTRUE(print)){
      cat("(\u2714) : 'hisat2' is installed\n\n")
    }
    return(TRUE)
  }
  else{
    cat("(\u2718) : 'hisat2' command is not found on this device. Please run 'InstallAll()' to install the necessary programs or 'ExportPath' to update the path.\n\n")
    return(FALSE)
  }
}

#' Check s'tringtie'
#' Check whether 'stringtie' is installed on the workstation
#' @export
CheckStringTie <- function(print=TRUE){
  if (print){
    cat("************** Checking stringtie command ************\n")
  }
  stringtie.installed <- system( 'stringtie --version', ignore.stdout = !print, ignore.stderr = !print)==0
  if( isTRUE(stringtie.installed)){
    if(isTRUE(print)){
      cat("(\u2714) : 'stringtie' is installed\n\n")
    }
    return(TRUE)
  }
  else{
    cat("(\u2718) : 'stringtie' command is not found on this device. Please run 'InstallAll()' to install the necessary programs or 'ExportPath' to update the path.\n\n")
    return(FALSE)
  }
}

#' Check Gffcompare
#' Check whether Gffcompare is installed on the workstation
#' @export
CheckGffcompare <- function(print=TRUE) {
  if(print) {
    cat("************** Checking gffcompare command ************\n")
  }
  gffcompare.old <- system( 'gffcompare --version', ignore.stdout = !print, ignore.stderr = !print)==0
  if( isTRUE(gffcompare.old)){
    if(isTRUE(print)){
      cat("(\u2714) : 'gffcompare' is installed\n\n")
    }
    return(TRUE)
  }
  else{
    cat("(\u2718) : \'gffcompare\' command is not found on this device. Please run 'InstallAll()' to install the necessary programs or 'ExportPath' to update the path.\n\n")
    return(FALSE)
  }
}

#' Check Samtools
#' Check whether Samtools is installed on the workstation
#' @export
CheckSamtools <- function(print=TRUE){
  if (print) {
    cat("************** Checking samtools command ************\n")
  }
  samtools.old <- system( 'samtools --version', ignore.stdout = !print, ignore.stderr = !print)==0
  if( isTRUE(samtools.old)){
    if(isTRUE(print)){
      cat("(\u2714) : 'samtools' is installed\n\n")
    }
    return(TRUE)
  }
  else{
    cat("(\u2718) : \'samtools\' command is not found on this device. Please run 'InstallAll()' to install the necessary programs or 'ExportPath' to update the path.\n\n")
    return(FALSE)
  }
}

#' Check whether programs are installed
#' @export
CheckToolAll <- function(print=TRUE) {
  hisat2.check <- CheckHisat2(print=print)
  stringtie.check <- CheckStringTie(print=print)
  gff.check <- CheckGffcompare(print=print)
  samtool.check <- CheckSamtools(print=print)
  if (isTRUE(hisat2.check) && isTRUE(stringtie.check) && isTRUE(gff.check) && isTRUE(samtool.check)){
    return(TRUE)
  } else {
    stop("(\u2718) Necessary program is missing.\n     1. Check 'INSTALL_TOOLS.Rout' whether tools are properly installed.\n     2. Run 'ExportPath()' to set the environment.\n\n")
    return(FALSE)
  }
}




#' #' Install Hisat2 binary
#' #' @export
#' InstallHisat2Bianry <-function(download.file = "NOT_SET_YET"){
#'   if (download.file == "NOT_SET_YET") {
#'     cat("(\u2718) : 'download.file' is missing.\n\n")
#'   } else {
#'     # ftp server : ftp://ftp.ccb.jhu.edu/pub/infphilo/hisat2/downloads/
#'     if (isTRUE(CheckDirAll(print = FALSE))){
#'       cat("************** Installing Hisat2 (hisat2-2.1.0-source.zip) ************\n")
#'       current.path <- getwd()
#'       cat(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Download/\n"))
#'       setwd(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Download/"))
#'       url <- paste0("ftp://ftp.ccb.jhu.edu/pub/infphilo/hisat2/downloads/", download.file)
#'       download.file('ftp://ftp.ccb.jhu.edu/pub/infphilo/hisat2/downloads/hisat2-2.1.0-source.zip', 'hisat2-2.1.0-source.zip')
#'       #system2(command = 'curl', args = c('ftp://ftp.ccb.jhu.edu/pub/infphilo/hisat2/downloads/hisat2-2.1.0-source.zip', '--output', 'hisat2-2.1.0-source.zip'), stdout = "", wait = TRUE)
#'       on.exit(setwd(current.path))
#'       cat(paste0("'", pkg.global.path.prefix$data_path, "RNAseq_bin/Download/hisat2-2.1.0-source.zip' has been installed.\n\n"))
#'     }
#'   }
#' }

#' #' Install Hisat2
#' #' @export
#' InstallHisat2 <- function() {
#'   # ftp server : ftp://ftp.ccb.jhu.edu/pub/infphilo/hisat2/downloads/hisat2-2.1.0-source.zip
#'   if (isTRUE(CheckDirAll(print = FALSE))){
#'     cat("************** Installing Hisat2 (hisat2-2.1.0-source.zip) ************\n")
#'     current.path <- getwd()
#'     cat(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Download/\n"))
#'     setwd(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Download/"))
#'     download.file('ftp://ftp.ccb.jhu.edu/pub/infphilo/hisat2/downloads/hisat2-2.1.0-source.zip', 'hisat2-2.1.0-source.zip')
#'     #system2(command = 'curl', args = c('ftp://ftp.ccb.jhu.edu/pub/infphilo/hisat2/downloads/hisat2-2.1.0-source.zip', '--output', 'hisat2-2.1.0-source.zip'), stdout = "", wait = TRUE)
#'     on.exit(setwd(current.path))
#'     cat(paste0("'", pkg.global.path.prefix$data_path, "RNAseq_bin/Download/hisat2-2.1.0-source.zip' has been installed.\n\n"))
#'   }
#' }
#'
#' #' Install StringTie
#' #' @export
#' InstallStringTie <- function() {
#'   # http://ccb.jhu.edu/software/stringtie/dl/stringtie-1.3.4d.tar.gz
#'   if (isTRUE(CheckDirAll(print = FALSE))){
#'     cat("************** Installing StringTie (stringtie-1.3.4d.tar.gz) ************\n")
#'     current.path <- getwd()
#'     cat(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Download/\n"))
#'     setwd(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Download/"))
#'     download.file('ccb.jhu.edu/software/stringtie/dl/stringtie-1.3.4d.tar.gz', 'stringtie-1.3.4d.tar.gz')
#'     #system2(command = 'curl', args = c('ccb.jhu.edu/software/stringtie/dl/stringtie-1.3.4d.tar.gz', '--output', 'stringtie-1.3.4d.tar.gz'), stdout = "", wait = TRUE)
#'     on.exit(setwd(current.path))
#'     cat(paste0("'", pkg.global.path.prefix$data_path, "RNAseq_bin/Download/stringtie-1.3.4d.tar.gz' has been installed.\n\n"))
#'   }
#' }
#'
#' #' Install Gffcompare
#' #' @export
#' InstallGffcompare <- function() {
#'   # http://ccb.jhu.edu/software/stringtie/dl/gffcompare-0.10.4.tar.gz
#'   if (isTRUE(CheckDirAll(print = FALSE))){
#'     cat("************** Installing Gffcompare (gffcompare-0.10.4.tar.gz) ************\n")
#'     current.path <- getwd()
#'     cat(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Download/\n"))
#'     setwd(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Download/"))
#'     download.file('ccb.jhu.edu/software/stringtie/dl/gffcompare-0.10.4.tar.gz', 'gffcompare-0.10.4.tar.gz')
#'     #system2(command = 'curl', args = c('ccb.jhu.edu/software/stringtie/dl/gffcompare-0.10.4.tar.gz', '--output', 'gffcompare-0.10.4.tar.gz'), stdout = "", wait = TRUE)
#'     on.exit(setwd(current.path))
#'     cat(paste0("'", pkg.global.path.prefix$data_path, "RNAseq_bin/Download/gffcompare-0.10.4.tar.gz' has been installed.\n\n"))
#'   }
#' }
#'
#' #' Install Samtools
#' #' @export
#' InstallSamtools <- function() {
#'   # https://github.com/samtools/samtools/releases/download/1.8/samtools-1.8.tar.bz2
#'   if (isTRUE(CheckDirAll(print = FALSE))){
#'     cat("************** Installing Samtools (samtools-1.8.tar.bz2) ************\n")
#'     current.path <- getwd()
#'     cat(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Download/\n"))
#'     setwd(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Download/"))
#'     download.file('https://github.com/samtools/samtools/releases/download/1.8/samtools-1.8.tar.bz2', 'samtools-1.8.tar.bz2')
#'     #system2(command = 'curl', args = c('-L', 'https://github.com/samtools/samtools/releases/download/1.8/samtools-1.8.tar.bz2', '>', 'samtools-1.8.tar.bz2'), stdout = "", wait = TRUE)
#'     on.exit(setwd(current.path))
#'     cat(paste0("'", pkg.global.path.prefix$data_path, "RNAseq_bin/Download/samtools-1.8.tar.bz2' has been installed.\n\n"))
#'   }
#' }
#'
#' #' Unpack Hisat2
#' #' @export
#' UnpackHisat2 <- function() {
#'   if (isTRUE(CheckDirAll(print = FALSE))){
#'     cat("************** Unpacking Hisat2 (hisat2-2.1.0-source.zip) ************\n")
#'     current.path <- getwd()
#'     cat(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/\n"))
#'     setwd(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/"))
#'     unzip(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Download/hisat2-2.1.0-source.zip"))
#'     #system2(command = 'unzip', args = paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Download/hisat2-2.1.0-source.zip"))
#'     on.exit(setwd(current.path))
#'     cat(paste0("Hisat2 has been unpacked. ('", pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/hisat2-2.1.0/')"), "\n\n")
#'   }
#' }
#'
#' #' Unpack StringTie
#' #' @export
#' UnpackStringTie <- function() {
#'   if (isTRUE(CheckDirAll(print = FALSE))){
#'     cat("************** Unpacking StringTie (stringtie-1.3.4d.tar.gz) ************\n")
#'     current.path <- getwd()
#'     cat(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/\n"))
#'     setwd(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/"))
#'     untar( paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Download/stringtie-1.3.4d.tar.gz"))
#'     #system2(command = 'tar', args = c("xvzf", paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Download/stringtie-1.3.4d.tar.gz")))
#'     on.exit(setwd(current.path))
#'     cat(paste0("StringTie has been unpacked. ('", pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/stringtie-1.3.4d.tar.gz/')"), "\n\n")
#'   }
#' }
#'
#' #' Unpack Gffcompare
#' #' @export
#' UnpackGffcompare <- function() {
#'   if (isTRUE(CheckDirAll(print = FALSE))){
#'     cat("************** Unpacking Gffcompare (gffcompare-0.10.4.tar.gz) ************\n")
#'     current.path <- getwd()
#'     cat(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/\n"))
#'     setwd(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/"))
#'     untar(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Download/gffcompare-0.10.4.tar.gz"))
#'     #system2(command = 'tar', args = c("xvzf", paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Download/gffcompare-0.10.4.tar.gz")))
#'     on.exit(setwd(current.path))
#'     cat(paste0("Gffcompare has been unpacked. ('", pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/gffcompare-0.10.4/')"), "\n\n")
#'   }
#' }
#'
#' #' Unpack Samtools
#' #' @export
#' UnpackSamtools <- function() {
#'   if (isTRUE(CheckDirAll(print = FALSE))){
#'     cat("************** Unpacking Samtools (samtools-1.8.tar.bz2) ************\n")
#'     current.path <- getwd()
#'     cat(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/\n"))
#'     setwd(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/"))
#'     untar(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Download/samtools-1.8.tar.bz2"))
#'     #system2(command = 'tar', args = c("jxvf",  paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Download/samtools-1.8.tar.bz2")))
#'     on.exit(setwd(current.path))
#'     cat(paste0("Samtools has been unpacked. ('", pkg.global.path.prefix$data_path, "/RNAseq_bin/Unpacked/samtools-1.8/')"), "\n\n")
#'   }
#' }
#'
#' #' Making Hisat2 binary
#' #' @export
#' BinaryHisat2 <- function() {
#'   if (isTRUE(CheckDirAll(print = FALSE))){
#'     cat("************** Making Hisat2 Binary ************\n")
#'     current.path <- getwd()
#'     setwd(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/hisat2-2.1.0/"))
#'     system2(command = 'make', stderr = FALSE)
#'     system2(command = 'cp', args = c("hisat2*", "*.py", paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/")), stderr = FALSE)
#'     system2(command = 'rm', args = c("*.cpp", paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/")), stderr = FALSE)
#'     cat("hisat2 binaries are in: ", paste0("'", pkg.global.path.prefix$data_path, "RNAseq_bin/'\n\n"))
#'     on.exit(setwd(current.path))
#'   }
#' }
#'
#' #' Making StringTie binary
#' #' @export
#' BinaryStringTie <- function() {
#'   if (isTRUE(CheckDirAll(print = FALSE))){
#'     cat("************** Making StringTie Binary ************\n")
#'     current.path <- getwd()
#'     setwd(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/stringtie-1.3.4d/"))
#'     system2(command = 'make', args = c("clean", "release"), stderr = FALSE)
#'     file.copy("stringtie", paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/"))
#'     #system2(command = 'cp', args = c("stringtie", paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/")), stderr = FALSE)
#'     cat("stringtie binaries are in: ", paste0("'", pkg.global.path.prefix$data_path, "RNAseq_bin/'\n\n"))
#'     on.exit(setwd(current.path))
#'   }
#' }
#'
#' #' Making Gffcompare binary
#' #' @export
#' BinaryGffcompare <- function() {
#'   if (isTRUE(CheckDirAll(print = FALSE))){
#'     cat("************** Making Gffcompare Binary ************\n")
#'     current.path <- getwd()
#'     setwd(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/gffcompare-0.10.4/"))
#'     system2(command = 'make', stderr = FALSE)
#'     file.copy("gffcompare", paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/"))
#'     #system2(command = 'cp', args = c("gffcompare", paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/")), stderr = FALSE)
#'     cat("gffcompare binaries are in: ", paste0("'", pkg.global.path.prefix$data_path, "RNAseq_bin/'\n\n"))
#'     on.exit(setwd(current.path))
#'   }
#' }
#'
#'
#' #' Making Samtools binary
#' #' @export
#' BinarySamtools <- function() {
#'   if (isTRUE(CheckDirAll(print = FALSE))){
#'     cat("************** Making Samtools Binary ************\n")
#'     current.path <- getwd()
#'     setwd(paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/Unpacked/samtools-1.8/"))
#'     system2(command = 'make', args = "clean", stderr = FALSE)
#'     system2(command = 'make')
#'     file.copy("samtools", paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/"))
#'     #system2(command = 'cp', args = c("samtools", paste0(pkg.global.path.prefix$data_path, "RNAseq_bin/")), stdout = FALSE, stderr = FALSE)
#'     cat("samtools binaries are in: ", paste0("'", pkg.global.path.prefix$data_path, "RNAseq_bin/'\n\n"))
#'     on.exit(setwd(current.path))
#'   }
#' }

