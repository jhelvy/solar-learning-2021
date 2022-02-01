data {
    int<lower=0> N; //number of data
    vector[N] x1; //cumCapacityKw_world
    vector[N] x2; //cumCapacityKw_other
    vector[N] x3; //log(price_si)
    vector[N] y;  //log(costPerKw)
}

parameters {
    real alpha;//intercept
    real<upper=0> beta1; //slope for cumCapKw
    real beta2; //slope for price_si
    real<lower=0> lambda; //lamba
    real<lower=0> sigma; //scatter
}

model {
    //priors
    alpha  ~ normal(0, 10);
    beta1  ~ normal(0, 10);
    beta2  ~ normal(0, 10);
    lambda ~ beta(1, 1);
    sigma  ~ normal(0, 5);

    //likelihood
    for (i in 1:N)
        y[i] ~ normal(alpha + beta1 * log(x1[i] - (lambda * x2[i])) + beta2 * x3[i], sigma);
}
