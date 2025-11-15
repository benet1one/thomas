
# setwd("tests/testthat")

my_model <- jags_model({
    a ~ dexp(0.01)
    b ~ dexp(0.01)

    for (i in 1:n) {
        lambda[i] ~ dgamma(a, b)
        y[i] ~ dpois(lambda[i])
    }

    for (i in 1:3) {
        for (j in 1:4) {
            whatever[i, j] ~ dexp(1)
        }
    }

    l ~ dgamma(a, b)
})

my_data <- list(y = rpois(50, 32), n = 50)

jags_fit <- run_jags(
    model = my_model,
    data = my_data,
    parameters = c("a", "b", "l", "lambda", "whatever"),
    iter = 100
)

run_jags(
    file = "model.jags",
    data = my_data,
    parameters = c("a", "b", "l"),
    iter = 100
)

test_that("file and model", {
    expect_error(run_jags(
        model = my_model,
        file = "model.jags",
        data = my_data,
        parameters = c("a", "b", "l"),
        iter = 100
    ))
    expect_error(run_jags(
        data = my_data,
        parameters = c("a", "b", "l"),
        iter = 100
    ))
    expect_error(jags_model(
        model = {whatever},
        file = "model.jags"
    ))
})


draws <- get_draws(jags_fit, as = "df")
draws[[1, ]]

draws_listed <- get_draws(jags_fit, as = "df_listed")
draws_listed[[1, ]]

test_that("exactly_one_draw", {
    expect_error( draws[[1:2, ]] )
    expect_error( draws[[integer(), ]] )
})
