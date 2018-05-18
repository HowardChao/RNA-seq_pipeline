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
    cat("(\u2718) : Run 'ExportPath()' to set the environment.\n\n")
    return(FALSE)
  }
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
    cat("R environment 'PATH' : ", Sys.getenv("PATH"), "\n\n")
  }
}
