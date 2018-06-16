home.path <- system2(command = 'echo', args = "$HOME", stdout = TRUE)
pkg.global.path.prefix <- new.env()
pkg.global.path.prefix$data_path <- "NOT_SET_YET"
pkg.global.path.prefix$input.files <- "NOT_SET_YET"
pkg.global.ht2 <- new.env()
pkg.global.ht2$logic <- FALSE
#' @export
pkg.ballgown.data <- new.env()
pkg.ballgown.data$bg_chrX <- NULL
pkg.ballgown.data$bg_chrX_filt <- NULL

