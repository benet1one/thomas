
#' Title
#'
#' @param file String indicating the file containing the model. Can either be
#' a .exe file with the compiled model or a .stan file containing the model code.
#' @param quiet Logical, wether to suppress messages.
#' @param ... Other arguments passed on to
#' \code{\link{cmdstanr::cmdstan_model()}}.
#'
#' @return A CmdStanModel object.
#' @export
cmdstan_model <- function(file = NULL, quiet = FALSE, ...) {

    rlang::check_installed("cmdstanr")

    wrap <- if (quiet) suppressMessages else identity
    if (stringr::str_detect(file, r"(\.exe$)"))
        wrap(cmdstanr::cmdstan_model(exe_file = file, compile = TRUE, ...))
    else
        wrap(cmdstanr::cmdstan_model(stan_file = file, compile = TRUE, ...))
}

#' @export
print.CmdStanModel <- function(x, ...) {
    cat("<Cmd Stan Model>\n")
    x$print()
}

#' @export
run_cmdstan <- function(model, file, data, inits = NULL,
                        iter = 2000, burnin = floor(iter/2), thin = 1,
                        chains = 4, warmup = burnin,
                        seed = NULL, ...) {

    rlang::check_installed("cmdstanr")

    if (missing(model) + missing(file) != 1L)
        stop("Specify either 'model' (see cmdstan_model()) or 'file'.")
    if (missing(model))
        model <- cmdstan_model(file)
    model$sample(
        data = data,
        init = inits,

        iter_warmup = warmup,
        iter_sampling = iter - warmup,
        thin = thin,
        chains = chains,

        seed = seed,
        ...
    )
}

#' @export
get_draws.CmdStanMCMC <- function(fit, as = c("df", "list", "array")) {
    as <- as[1L]
    draws <- fit$draws(format = "array")
    switch(as,
        df = draws_to_df(draws),
        list = draws_to_df(draws) |> as.list(),
        array = draws
    )
}

#' @export
get_parameter_dim.CmdStanMCMC <- function(fit) {
    d <- fit$metadata()$stan_variable_sizes
    d[names(d) != "lp__"]
}
