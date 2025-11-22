
my_data <- list(y = rpois(50, 32), n = 50)

model <- cmdstan_model(file = "model.stan")
cmdstan_fit <- run_cmdstan(
    model = model,
    data = my_data,
    iter = 100,
    inits = tibble(a = 3)
)

cmdstan_advi_fit <- run_cmdstan_advi(
    model = model,
    data = my_data,
    n_draws = 10000,
    inits = list(a = 3),
    algorithm = "fullrank"
)

file.remove("model.exe")
