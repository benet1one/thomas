
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

draws_to_df <- function(samp, par_dim) {
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
            reshape2::acast(chain + iter ~ parameter)
        if (ncol(mat) == 1L)
            mat <- c(mat)
        tib[[p]] <- mat
    }

    structure(
        tib,
        class = c("thomas_draw_df", class(tib)),
        par_dim = par_dim
    )
}

#' Extract a draw and reshape it to the dimensions of each parameter
#'
#' @param x Data frame containing the draws.
#' @param ... Indexes. You can select one row and any number columns.
#'
#' @returns A named list where each parameter has the right dimension.
#' @export
#'
#' @examples
#' draws <- get_draws(fit)
#' draws[[1, ]]
#' draws[1, ]
`[[.thomas_draw_df` <- function(x, ...) {
    y <- x[...]

    if (nrow(y) != 1L) {
        stop("Use draws[[i, ]] to extract exactly one draw.")
    }

    y <- as.list(y)
    pd <- attr(x, "par_dim")

    for (k in seq_along(y)) {
        p <- names(y)[k]
        if (p %in% names(pd)) {
            y[[k]] <- array(y[[k]], dim = pd[[p]])
        }
    }

    attr(y, "par_dim") <- NULL
    return(y)
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

#' Get an arbitrary statistic from each parameter of a fit.
#' @param fit Bayesian fit.
#' @param fun Function to apply to each parameter.
#' @export
get_statistic.default <- function(fit, fun = mean) {
    draws <- get_draws(fit, as = "df")
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


drop_matrices <- function(df, max_cols = 4L) {
    df <- as.list(df)

    for (k in 1:length(df))  if (is.matrix(df[[k]])) {
        to <- min(max_cols, 1)
        df[[k]] <- df[[k]][, 1:to] |> as.data.frame()
    }

    dplyr::bind_cols(df)
}

#' Plot chains to check convergence.
#'
#' @param fit Bayesian fit.
#' @param ... Parameters to select, passed to [dplyr::select()].
#' @param .max_values For multivariate parameters, number of different values to plot.
#'
#' @export
traceplot <- function(fit, ..., .max_values = 4L) {

    rlang::check_installed("ggplot2")
    require(ggplot2, quietly = TRUE)

    draws <- get_draws(fit, as = "df")

    if (...length() == 0L) {
        draws <- draws[1:min(6L, ncol(draws))]
    } else {
        draws <- dplyr::select(draws, iter, chain, ...)
    }

    draws <- draws |>
        drop_matrices(max_cols = .max_values) |>
        reshape2::melt(id.vars = c("chain", "iter"))

    ggplot(draws, aes(x = iter, y = value, color = factor(chain))) +
        facet_wrap(~variable, scales = "free") +
        geom_line(show.legend = FALSE) +
        theme_minimal()
}
