#' Check hisat2
#' Check whether hisat2 is installed on the workstation
#' @export
CheckHisat2 <- function(){
  hisat2.installed <- system('hisat2 --v')==0
  if(hisat2.installed == FALSE){
    print("\'hisat2\' command is not found on the device. Please install it and add it to your PATH.")
  }
  else{
    print("True")
  }
}

InstallHisat2 <- function(){

}
