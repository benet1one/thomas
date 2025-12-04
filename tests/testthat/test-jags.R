
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
    inits = list(list(a = 2)),
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
