
data("jags_fit")

draws <- get_draws(jags_fit, as = "df")
draws[[1, ]]

draws_listed <- get_draws(jags_fit, as = "df_listed")
draws_listed[[1, ]]

test_that("exactly_one_draw", {
    expect_error( draws[[1:2, ]] )
    expect_error( draws[[integer(), ]] )
})

map_draws(draws, \(d) rowSums(d$gamma))
map_draws(draws_listed, \(d) rowSums(d$gamma))
