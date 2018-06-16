QualityControl <- function(){
  current.path <- getwd()
  dir.create(paste0(pkg.global.path.prefix$data_path, "QualityControl"))
  setwd(paste0(paste0(pkg.global.path.prefix$data_path, "QualityControl")))
  systemPipeRdata::genWorkenvir("rnaseq")
  # targetspath <- system.file("extdata", "targets_chip.txt", package="systemPipeR")
  # targets <- read.delim(targetspath, comment.char = "#")
  # targets[1:4,-c(5,6)]
  on.exit(setwd(current.path))
}
