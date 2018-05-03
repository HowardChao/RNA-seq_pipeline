#' Check 'hisat2'
#' Check whether 'hisat2' is installed on the workstation
#' @export
CheckHisat2 <- function(){
  cat("************** Checking hisat2 command ************\n")
  hisat2.installed <- system('hisat2 --version')==0
  if( isTRUE(hisat2.installed)){
    cat("'hisat2' is installed\n\n")
  }
  else{
    cat("'hisat2' command is not found on this device. Please run 'InstallAll()' to install the necessary programs or 'ExportPath' to update the path.\n\n")
  }
}

#' Check s'tringtie'
#' Check whether 'stringtie' is installed on the workstation
#' @export
CheckStringTie <- function(){
  cat("************** Checking stringtie command ************\n")
  stringtie.installed <- system( 'stringtie --version')==0
  if( isTRUE(stringtie.installed)){
    cat("'stringtie' is installed\n\n")
  }
  else{
    cat("'stringtie' command is not found on this device. Please run 'InstallAll()' to install the necessary programs or 'ExportPath' to update the path.\n\n")
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
    cat("\'gffcompare\' command is not found on this device. Please run 'InstallAll()' to install the necessary programs or 'ExportPath' to update the path.\n\n")
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
    cat("\'samtools\' command is not found on this device. Please run 'InstallAll()' to install the necessary programs or 'ExportPath' to update the path.\n\n")
  }
}

#' Check Ballgown
#' Check whether Ballgown is installed on the workstation
#' @export
CheckBallgown <- function(){
  cat("************** Checking ballgown command ************\n")
  #samtools.old <- system( 'ballgown --version')==0
  if( isTRUE(samtools.old)){
    cat("'samtools' is installed\n\n")
  }
  else{
    cat("\'samtools\' command is not found on this device. Please run 'InstallAll()' to install the necessary programs or 'ExportPath' to update the path.\n\n")
  }
}


#' Check whether programs are installed
#' @export
CheckToolAll <- function() {
  CheckHisat2()
  CheckStringTie()
  CheckGffcompare()
  CheckSamtools()
}

#' Add '~/RNAseq_bin/ to R environment "PATH"
#' @export
ExportPath <- function() {
  if (isTRUE(CheckPrefixPath(pkg.global.path.prefix$data_path, print = FALSE))){
    cat("************** Adding PATH to R environment ************\n")
    old.path <- Sys.getenv("PATH")
    Sys.setenv(
      PATH = paste(old.path, paste0(pkg.global.path.prefix$data_path, "RNAseq_bin"), sep = ":")
    )
    cat("R environment 'PATH': ", Sys.getenv("PATH"), "\n\n")
  }
}
