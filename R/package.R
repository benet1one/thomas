
#' @import rlang
#' @importFrom tibble tibble
#' @importFrom stringr str_remove
#' @importFrom R2jags jags
#' @importFrom rstan stan
#' @importFrom rstan extract
#' @importFrom reshape2 melt
#' @importFrom reshape2 dcast
bayes <- function() {
  cat("Pr(A|B) = Pr(B|A) * Pr(A) / Pr(B)")
}

