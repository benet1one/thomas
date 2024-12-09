
format_jags_block <- function(expr, block_name) {
    if (!rlang::is_symbolic(expr)  &&  !rlang::is_string(expr))
        stop("'", block_name, "' must be an expression or a string.")
    text <- format(expr) |>
        paste(collapse = "\n") |>
        gsub(pattern = r"( ?\%>\% ?)", replacement = " ")
    if (!grepl(r"(^\{)", text))
        text <- paste0("{\n    ", text, "\n}")
    paste(block_name, text)
}

#' Write a JAGS model with R's syntax highlighting.
#' @description
#' Instead of writing the model on a string, write it inside
#' this function and get syntax highlighting and warnings.
#' Alternatively, specify the file where the model is written.
#'
#' @param model Expression or string containing the code for the model block.
#' @param data Expression or string containing the code for the data block.
#' @param file String indicating where the model is written.
#'
#' @details
#' In order to truncate a distribution, you can use the magrittr pipe (%>%)
#' and NOT the R pipe (|>) between the distribution and the truncation, such as:
#' \code{dnorm(0, 1) %>% T(0, )}
#'
#' @export
#' @examples
#' my_model <- jags_model({
#'     a ~ dexp(0.01)
#'     b ~ dexp(0.01)
#'
#'     for (i in 1:n) {
#'         lambda[i] ~ dgamma(a, b)
#'         y[i] ~ dpois(lambda[i])
#'     }
#' })
jags_model <- function(model, data, file) {
    if (!missing(file)) {
        if (!missing(model) || !missing(data))
            stop("Do not specify 'model' or 'data' if a 'file' is supplied.")
        text <- readLines(file) |> paste(collapse = "\n") |> trimws()
        return(structure(text, class = "jags_model"))
    }

    if (missing(model))
        stop("Missing model.")
    model_text <- format_jags_block(rlang::enexpr(model), "model")
    data_text <- if (!missing(data))
        format_jags_block(rlang::enexpr(data), "data")
    text <- paste(data_text, model_text, sep = "\n")
    structure(text, class = "jags_model")
}

#' @export
print.jags_model <- function(x, ...) {
    cat("<JAGS Model>\n")
    cat(x)
}


parse_jags_inits <- function(inits, chains) {
    if (is.null(inits))
        return(NULL)
    if (is.data.frame(inits))
        inits <- split.data.frame(inits, factor(1:nrow(inits)))

    if (length(inits) == 1L)
        inits <- rep(inits, chains)
    if (length(inits) != chains)
        stop("inits must be either length 1, or the number of chains (", chains,").")
    inits
}

parse_jags_model <- function(model) {
    if (rlang::is_string(model))
        return(model)
    stop("'model' must be a string or a jags_model().")
}

#' Run MCMC on a JAGS model
#' @description
#' Generate posterior samples from a JAGS model using Markov Chain Monte-Carlo.
#'
#' @param model Jags model as a string or created with [jags_model()].
#' @param file Alternatively, a file where the JAGS model is written.
#' @param data List containing the data used to train the model.
#' @param inits List or data.frame of initial values for parameters.
#' Must be either length 1 or the number of chains.
#' If a list, each value must be a named list containing the initial value
#' for each parameter.
#' If a data.frame, each row must contain the initial value for each parameter.
#' @param parameters Character vector of parameters to monitor.
#' @param iter Number of iterations, including burnin, per chain.
#' @param burnin Number of samples to burn per chain.
#' @param warmup Alias for burnin.
#' @param thin Positive integer specifying the period for saving samples.
#' @param chains Positive integer specifying the number of Markov chains.
#' The default is 4.
#' @param ... Arguments passed on to [R2jags::jags()].
#'
#' @return An rjags fit.
#' @export
#'
#' @examples
run_jags <- function(model, file, data, inits = NULL, parameters,
                     iter = 2000, burnin = floor(iter/2), thin = 1,
                     chains = 4, warmup = burnin, seed, ...) {

    if (missing(model) + missing(file) != 1L)
        stop("Specify either model (see jags_model()) or file.")
    if (!missing(model)) {
        model <- parse_jags_model(model)
        file <- textConnection(model)
    }
    if (!missing(seed))
        set.seed(seed)

    fit <- R2jags::jags(
        model.file = file,
        data = data,
        parameters.to.save = parameters,
        inits = parse_jags_inits(inits, chains),

        n.iter = iter,
        n.burnin = warmup,
        n.thin = thin,
        n.chains = chains,
        ...
    )
}

#' @export
get_samples.rjags <- function(x, n = NULL) {
    samples_to_df(x$BUGSoutput$sims.array, n)
}





