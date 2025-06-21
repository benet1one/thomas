
set.seed(123)

model <- thomas::jags_model({
    alpha ~ dpois(3)
    for (i in 1:3) {
        beta[i] ~ dnorm(i^2, i)
        for (j in 1:2) {
            gamma[i, j] ~ dexp(i/j)
        }
    }
    y ~ dnorm(0, 1)
})

jags_fit <- thomas::run_jags(
    model = model,
    data = list(y = 0),
    parameters = c("alpha", "beta", "gamma"),
    chains = 4,
    iter = 100
)

usethis::use_data(jags_fit, overwrite = TRUE)
