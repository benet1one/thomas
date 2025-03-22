
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

vector_to_col <- function(x) {
    if (length(dim(x)) <= 1L)
        matrix(x, ncol = 1L)
    else
        x
}


#' Get a statistic for each parameter.
#' @param fit Bayesian Fit.
#' @param fun Function to apply to the draws of each parameter.
#' @returns Named list, where the names are the parameters.
#' @seealso [get_means()], [get_sd()]
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
#' @param fit Bayesian Fit.
#' @export
get_parameter_dim <- function(fit)
    UseMethod("get_parameter_dim")

#' Get the names of parameters as a character vector.
#' @export
get_parameters <- function(fit) {
    names(get_parameter_dim(fit))
}

get_statistic.default <- function(fit, fun = mean) {
    draws <- get_draws(fit, as = "df")
    pdim <- get_parameter_dim(fit)

    sapply(names(pdim), simplify = FALSE, \(p) {
        mat <- vector_to_col(draws[[p]])
        stat <- apply(mat, 2L, fun)
        array(stat, dim = pdim[[p]])
    })
}

get_means.default <- function(fit) {
    get_statistic(fit, fun = mean)
}

get_sd.default <- function(fit) {
    get_statistic(fit, fun = sd)
}


drop_matrices <- function(df) {
    for (k in seq_along(df)) {
        if (df[[k]] |> is.matrix()) {
            df[[k]] <- df[[k]][, 1L]
            names(df)[k] <- paste0(names(df)[k], "[1]")
        }
    }
    return(df)
}

#' Plot chains to check convergence.
traceplot <- function(fit, pars = character()) {

    rlang::check_installed("ggplot2")
    require(ggplot2)

    if (length(pars) == 0L)
        pars <- head(get_parameters(fit), 4L)

    draws <- get_draws(fit, as = "df")[c("chain", "iter", pars)] |>
        drop_matrices() |>
        melt(id.vars = c("chain", "iter"))

    ggplot(draws, aes(x = iter, y = value, color = factor(chain))) +
        facet_wrap(~variable, scales = "free") +
        geom_line(show.legend = FALSE) +
        theme_minimal()
}
