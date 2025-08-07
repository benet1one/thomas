
#' Get posterior samples
#' @description
#' Get the posterior samples from a bayesian fit.
#'
#' @param fit STAN or JAGS fit.
#' @param as String, "df" to return a tibble with iteration and chain,
#' "list" to return a list of parameters, and
#' "array" to return a 3-D array (iter, chain, parameter).
#'
#' @rdname get_posterior_draws
#' @export
get_draws <- function(fit, as = c("df", "list", "array"))
    UseMethod("get_draws")

#' @rdname get_posterior_draws
#' @export
get_samples <- function(fit, as = c("df", "list", "array"))
    UseMethod("get_draws")

get_draws_anyway <- function(fit_or_draws) {
    if (inherits(fit_or_draws, "thomas_draw_df"))
        fit_or_draws
    else
        get_draws(fit_or_draws)
}

#' Permute Draws
#'
#' @description
#' Draws taken from the MCMC algorithm are correlated with the previous and next draws.
#' Permute them to remove this effect.
#'
#' @param within_chain Logical, whether to permute all draws (default) or permute
#' iterations within each chain.
permute_draws <- function(draws, within_chain = FALSE) {
    if (within_chain)
        draws <- draws |> dplyr::group_by(chain)
    dplyr::slice_sample(draws, n = nrow(draws)) |>
        dplyr::ungroup()
}

#' Get a statistic for each parameter.
#' @param fit Bayesian Fit.
#' @param fun Function to apply to the draws of each parameter.
#' @returns Named list, where the names are the parameters.
#' @seealso [get_means()], [get_sd()]
#' @export
get_statistic <- function(fit, fun = mean)
    UseMethod("get_statistic")

#' Get posterior means and standard deviations.
#' @rdname get_means_sd
#' @description
#' Get the posterior means from a bayesian fit.
#' @param fit Bayesian Fit.
#' @returns Named list, where the names are the parameters.
#' @seealso [get_statistic()]
#' @export
get_means <- function(fit)
    UseMethod("get_means")

#' @rdname get_means_sd
#' @export
get_sd <- function(fit)
    UseMethod("get_sd")

#' Get dimensions of parameters.
#' @param x A Bayesian Fit or a Draw Data Frame as generated from [get_draws()].
#' @export
get_parameter_dim <- function(fit)
    UseMethod("get_parameter_dim")

#' Get the names of parameters as a character vector.
#' @export
get_parameters <- function(fit) {
    names(get_parameter_dim(fit))
}

#' Get the Log-Probability of an optimized fit.
#' @export
get_lp <- function(fit) {
    UseMethod("get_lp")
}

#' Get the Maximum Likelihood Estimation of an optimized fit.
#' @export
get_mle <- function(fit) {
    UseMethod("get_mle")
}

vector_to_col <- function(x) {
    if (length(dim(x)) <= 1L)
        matrix(x, ncol = 1L)
    else
        x
}

#' @export
get_statistic.default <- function(fit, fun = mean) {
    draws <- get_draws_anyway(fit)
    pdim <- get_parameter_dim(fit)

    sapply(names(pdim), simplify = FALSE, \(p) {
        mat <- vector_to_col(draws[[p]])
        stat <- apply(mat, 2L, fun)
        array(stat, dim = pdim[[p]])
    })
}

#' @export
get_means.default <- function(fit) {
    get_statistic(fit, fun = mean)
}

#' @export
get_sd.default <- function(fit) {
    get_statistic(fit, fun = sd)
}

