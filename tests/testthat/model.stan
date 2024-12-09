data {
    int<lower=0> n;
    array[n] int<lower=0> y;
}
parameters {
    real<lower=0> a;
    real<lower=0> b;
    array[n] real<lower=0> lambda;
}
model {
    lambda ~ gamma(a, b);
    y ~ poisson(lambda);
}
