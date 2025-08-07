# model <- cmdstan_model("data-raw/model.stan")
# cmdstan_fit <- run_cmdstan(
#     model = model,
#     data = list(y = 3),
#     chains = 4,
#     iter = 100,
#     seed = 123
# )
# cmdstan_mle <- optimize_cmdstan(
#     model = model,
#     data = list(y = 3)
# )
#
# usethis::use_data(cmdstan_fit, overwrite = TRUE)
