data {
    int<lower=0> n;
    array[n] int<lower=0> y;
}
parameters {
    real<lower=0> a;
    real<lower=0> b;
    array[n] real<lower=0> lambda;
    array[3, 4] real whatever;
}
model {
    lambda ~ gamma(a, b);
    y ~ poisson(lambda);

    for (i in 1:3) {
        for (j in 1:4) {
            whatever[i, j] ~ normal(0, 5);
        }
    }
}
