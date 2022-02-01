data {
    int<lower=0> N; //number of data
    vector[N] x1; //cumCapacityKw
    vector[N] x2; //price_si
    vector[N] y; //costPerKw
}

parameters {
    real alpha;//intercept
    real<upper=0> beta1; //slope
    real beta2; //slope
    real<lower=0> sigma; //scatter
}

model {
    //priors
    alpha ~ normal(0, 10);
    beta1 ~ normal(0, 10);
    beta2 ~ normal(0, 10);
    sigma ~ normal(0, 5);

    //likelihood
    for (i in 1:N)
        y[i] ~ normal(alpha + beta1 * log(x1[i]) + beta2 * x2[i], sigma);
}
