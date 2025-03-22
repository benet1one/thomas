
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

setMethod("show", signature(object = "stanmodel"), function(object) {
    cat("<Stan Model>\n")
    cat(attr(object, "model_code"))
})

parse_stan_inits <- function(inits, chains) {
    if (is.null(inits))
        return("random")
    if (rlang::is_function(inits))
        return(inits)
    if (is.data.frame(inits))
        inits <- split.data.frame(inits, factor(1:nrow(inits)))

    if (length(inits) == 1L)
        inits <- rep(inits, chains)
    if (length(inits) != chains)
        stop("inits must be either length 1, or the number of chains (", chains,").")
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

run_stan <- function(model, file, data, inits = NULL, parameters = NA,
                     iter = 2000, burnin = floor(iter/2), thin = 1,
                     chains = 4, warmup = burnin,
                     seed = sample.int(.Machine$integer.max, 1),
                     cores = if (rlang::is_installed("parallel"))
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
        cores = cores,
        ...
    )
}

#' @export
get_draws.stanfit <- function(fit, as = c("df", "list", "array")) {
    as <- as[1L]
    switch(as,
        df = rstan::extract(fit, permuted = FALSE) |> draws_to_df(),
        array = rstan::extract(fit, permuted = FALSE),
        list = rstan::extract(fit) |> lapply(vector_to_col)
    )
}

#' @export
get_statistic.stanfit <- function(fit, fun) {
    lapply(rstan::extract(fit), function(x) {
        if (is.null(dim(x)) || length(dim(x)) == 1L)
            fun(x)
        else
            apply(x, 2:length(dim(x)), fun)
    })
}

#' @export
get_parameter_dim.stanfit <- function(fit) {
    pd <- fit@par_dims
    pd <- pd[names(pd) != "lp__"]
    lapply(pd, function(d) d %||% 1L)
}


