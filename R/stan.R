
#' @export
stan_model <- function(file, string, ...) {

    rlang::check_installed("rstan")

    if (!missing(file) + !missing(string) != 1L)
        stop("Specify either a 'file' or a 'string' containing the stan model.")
    message("Compiling Stan model...")
    if (!missing(file))
        rstan::stan_model(file = file)
    else
        rstan::stan_model(model_code = string)
}

parse_stan_inits <- function(inits, chains) {
    if (is.null(inits))
        return("random")
    if (rlang::is_function(inits))
        return(inits)
    if (rlang::is_atomic(inits))
        stop("`inits` must be a list, data.frame, or function.")

    if (is.data.frame(inits))
        inits <- apply(inits, 1L, as.list, simplify = FALSE)
    if (is.list(inits) && !is.null(names(inits)))
        stop("If `inits` is a list, it must be an unnamed list of named lists. ",
             "If you want to use the same `inits` for each chain, you can use `list(list(...))`")

    if (length(inits) == 1L)
        inits <- rep(inits, chains)
    if (length(inits) != chains)
        stop("`inits` must be either length 1, or the number of chains (", chains,").")
    inits
}

parse_stan_model <- function(model) {
    if (inherits(model, "stanmodel"))
        return(model)
    else if (inherits(model, "thomas_stan_model"))
        return(model$stanmodel)
    else if (rlang::is_string(model))
        return(stan_model(string = model))
    stop("'model' must be a string or a stan_model().")
}

#' @export
run_stan <- function(model, file, data, inits = NULL, parameters = NA,
                     iter = 2000, burnin = floor(iter/2), thin = 1,
                     chains = 4, warmup = burnin,
                     seed = sample.int(.Machine$integer.max, 1),
                     parallel_chains = if (rlang::is_installed("parallel"))
                         parallel::detectCores()
                     else getOption("mc.cores", default = 1L),
                     ...) {

    rlang::check_installed("rstan")

    if (missing(model) + missing(file) != 1L)
        stop("Specify either 'model' (see stan_model()) or 'file'.")
    if (missing(model))
        model <- stan_model(file = file)

    message("Sampling from Stan model...")
    rstan::sampling(
        object = parse_stan_model(model),
        data = data,
        pars = parameters,
        init = parse_stan_inits(inits, chains),

        iter = iter,
        warmup = warmup,
        chains = chains,
        thin = thin,

        seed = seed,
        cores = parallel_chains,
        ...
    )
}

#' @export
get_draws.stanfit <- function(fit, as = c("df", "list", "array")) {
    as <- as[1L]
    pd <- get_parameter_dim(fit)
    draws <- rstan::extract(fit, permuted = FALSE)

    switch(as,
        df = draws_to_df(draws, pd),
        df_listed = draws_to_df(draws, pd, column_lists = TRUE),
        list = draws_to_df(draws, pd) |> as.list(),
        array = draws
    )
}

#' @export
get_statistic.stanfit <- function(fit, fun) {
    lapply(rstan::extract(fit, permuted = FALSE), function(x) {
        if (is.null(dim(x)) || length(dim(x)) == 1L)
            fun(x)
        else
            apply(x, 2:length(dim(x)), fun)
    })
}

#' @export
get_parameter_dim.stanfit <- function(fit) {
    pd <- fit@par_dims
    lapply(pd, function(d) {
        if (length(d) > 0) d  else 1L
    })
}

#' @export
get_parameters.stanfit <- function(fit) {
    p <- fit@model_pars
    p[p != "lp__"]
}
