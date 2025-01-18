
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
get_samples <- function(fit,as = c("df", "list", "array"))
    UseMethod("get_draws")

draws_to_df <- function(samp) {
    names(dimnames(samp)) <- c("iter", "chain", "parameter")
    df <- reshape2::melt(samp)
    par_names <- stringr::str_remove(df$parameter, r"(\[.+\])")
    tib <- df |>
        reshape2::dcast(chain + iter ~ parameter) |>
        tibble::tibble()

    tib <- tib[, 1:2]
    tib$chain <- as.integer(tib$chain)

    for (p in unique(par_names)) {
        mat <- df[par_names == p, ] |>
            reshape2::acast(chain + iter ~ parameter) |>
            unname()
        if (ncol(mat) == 1L)
            mat <- c(mat)
        tib[[p]] <- mat
    }

    return(tib)
}

#' Get posterior means and standard deviations.
#' @rdname get_means_sd
#' @description
#' Get the posterior means from a bayesian fit.
#' @param fit Stan or JAGS fit.
#' @returns Named list, where the names are the parameters.
#' @export
get_means <- function(fit)
    UseMethod("get_means")

#' @rdname get_means_sd
#' @export
get_sd <- function(fit)
    UseMethod("get_sd")


#' Get dimensions of parameters.
#' @param x JAGS or Stan fit.
get_parameter_dim <- function(fit)
    UseMethod("get_parameter_dim")


#' Plot chains to check convergence.
traceplot <- function(fit, pars) {
    draws <- get_draws(fit, as = "df")

    for (p in pars) {
        draws_p <- draws[p]
        plt <- ggplot2::ggplot(draws_p, aes(x = iter, color = chain, y = value)) +
            ggplot2::geom_line()
    }
}

attach_fit <- function(fit)
    UseMethod("attach_fit")
