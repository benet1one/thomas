
#' @import rlang
#' @importFrom tibble tibble
#' @importFrom stringr str_remove
#' @importFrom reshape2 melt
#' @importFrom reshape2 dcast
bayes <- function() {
  cat("Pr(A|B) = Pr(B|A) * Pr(A) / Pr(B)")
}

