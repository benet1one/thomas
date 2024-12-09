
#' Get posterior samples
#' @description
#' Get the posterior samples from a bayesian fit.
#'
#' @param x STAN or JAGS fit.
#' @param n Number of samples to extract.
#'
#' @rdname get_posterior_samples
#' @export
get_samples <- function(x, n = NULL, ...)
    UseMethod("get_samples")

#' @rdname get_posterior_samples
#' @export
get_posterior <- function(x, n = NULL, ...)
    UseMethod("get_samples")

samples_to_df <- function(samp, n = NULL) {
    names(dimnames(samp)) <- c("iter", "chain", "parameter")
    df <- reshape2::melt(samp) |> reshape2::dcast(chain + iter ~ parameter)
    df$chain <- as.integer(df$chain)

    if (is.null(n))
        n <- nrow(samp)
    ind <- sample.int(nrow(samp), size = n, replace = (n > nrow(samp)))
    df <- df[ind, ]

    if (rlang::is_installed("tibble"))
        tibble::as_tibble(df)
    else
        df
}
