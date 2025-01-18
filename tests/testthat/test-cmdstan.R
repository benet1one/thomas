
my_data <- list(y = rpois(50, 32), n = 50)

model <- cmdstan_model(file = "tests/testthat/model.stan")
# stan_fit <- run_cmdstan(
#     model = model,
#     data = my_data,
#     iter = 100
# )
