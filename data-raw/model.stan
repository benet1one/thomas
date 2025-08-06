
data {
    int<lower=0> y;
}
parameters {
    real<lower=0, upper=1> alpha;
    vector[3] beta;
    array[3, 2] real<lower=0> gamma;
}
model {
    alpha ~ beta(2, 5);
    for (i in 1:3) {
        beta[i] ~ normal(i^2, i);
        for (j in 1:2) {
            gamma[i, j] ~ exponential(1.0 * i/j);
        }
    }
    y ~ poisson(2);
}
