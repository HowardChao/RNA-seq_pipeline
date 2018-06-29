#' RNASeq
#' An S4 class for storing RNA-Seq workflow parameters of this package
#' @aliases RNASeq
#'
#' @slot path.prefix directory holding installations and analysis results
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
    )
)
