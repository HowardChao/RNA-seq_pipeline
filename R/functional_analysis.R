#'
#' @export
DESelection <- function() {
  DEG_dataset <- read.csv(paste0(pkg.global.path.prefix$data_path, "DEG_results/FPKM_DEG_result.csv"))
  DEG_dataset <- DEG_dataset[DEG_dataset$log2FC >= 1,]
  DEG_dataset <- DEG_dataset[DEG_dataset$pval <0.05,]
}
