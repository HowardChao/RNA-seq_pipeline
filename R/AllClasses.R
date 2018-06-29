check_RNASeqWorkFlowParam <- function(object) {
  # 1. check path.prefix
  errors <- character()
  bool.prefix.path <- CheckPrefixPath(object@path.prefix, print = TRUE)
  if(!bool.prefix.path) {
    msg <- paste("path.prefix :", object@path.prefix, "is invalid!")
    errors <- c(errors, msg)
  }

  # 2. check input.path.prefix

}

CheckPrefixPath <- function(path.prefix = "NOT_SET_YET", print = TRUE) {
  # Check the prefix exist
  if (isTRUE(dir.exists(path.prefix))){
    if (substr(path.prefix, nchar(path.prefix), nchar(path.prefix)) != '/') {
      pkg.global.path.prefix$data_path <- paste0(path.prefix, '/')
    } else {
      pkg.global.path.prefix$data_path <- path.prefix
    }
    if (print) {
      cat(c("************** Setting prefix path ************\n"))
      cat(paste0("(\u270e) : The following files will be installed under '", pkg.global.path.prefix$data_path, "'\n\n"))
    }
    return(TRUE)
  } else if (path.prefix == "NOT_SET_YET") {
    cat("(\u2718) : Please give value to prefix path.\n\n")
    return(FALSE)
  } else {
    cat(paste0("(\u2718) : Prefix path '", path.prefix, "' is invalid. Please try another one.\n\n"))
    return(FALSE)
  }
}


#' Checking the absolute path
#' @export
CheckPrefixPath <- function(path.prefix = pkg.global.path.prefix$data_path, print = TRUE) {
  if (print) {
    cat(c("************** Checking prefix path ************\n"))
  }
  if (path.prefix == "NOT_SET_YET") {
    stop("(\u2718) You haven't set the prefix directory for the following steps.\n       Please run 'SetPrefixPath()' first to set the prefix directory.\n\n")
    return(FALSE)
  } else {
    if (print) {
      cat(paste0("(\u2714) :Prefix directory: '", pkg.global.path.prefix$data_path, "'\n\n"))
    }
    return(TRUE)
  }
}


#' RNASeq
#' An S4 class for storing RNA-Seq workflow parameters of this package
#' @aliases RNASeq
#'
#' @slot path.prefix the directory holding installations and analysis results
#' @slot input.path.prefix user has to prepared valid 'input_files/' under this directory
#' @slot gene.name gene name defined in this RNA-Seq workflow (ex. gene.name.fa, gene.name.gtf)
#' @slot sample.pattern  sample pattern describing the name of raw fastqc.gz files
#' @slot experiment.type set the type of the RNA-Seq analysis workflow. Character of one of "two.group", "multi.group.pairs", "multi.group.anova"
#' @slot main.variable main sample grouping variable
#' @slot additional.variable additional sample information
#'
#' @name RNASeqWorkFlowParam-class
#'
#' @rdname RNASeqWorkFlowParam-class
#'
#' @exportClass RNASeqWorkFlowParam
#' @author Kuan-Hao, Chao
#' @examples
#'   data(workflowParam)
#'   class(workflowParam) #"RNASeqWorkFlowParam"
#'   workflowParam@@path.prefix
#'   workflowParam@@input.path.prefix
#'   workflowParam@@gene.name
#'   workflowParam@@sample.pattern
#'   workflowParam@@experiment.type
#'   workflowParam@@main.variable
#'   workflowParam@@additional.variable
setClass("RNASeqWorkFlowParam",
         representation(
           path.prefix = "character",
           input.path.prefix = "character",
           gene.name = "character",
           sample.pattern = "character",
           experiment.type = "character",
           main.variable = "character",
           additional.variable = "character"
         ),
         prototype(
           path.prefix = "NOT_SET_YET",
           input.path.prefix = "NOT_SET_YET",
           gene.name = "NOT_SET_YET",
           sample.pattern = "NOT_SET_YET",
           experiment.type = "NOT_SET_YET",
           main.variable = "NOT_SET_YET",
           additional.variable = "NOT_SET_YET"
         ),
         validity = check_RNASeqWorkFlowParam
)
