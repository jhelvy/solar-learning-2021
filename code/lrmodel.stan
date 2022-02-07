data {
    int<lower=0> N; // number of data
    vector[N] qw;   // cumCapacityKw_world
    vector[N] p;    // price_si
    vector[N] logc; // log(costPerKw)
}

parameters {
    real alpha;  //intercept
    real beta;   //slope for cumCapKw
    real gamma;  //slope for price_si
    real<lower=0> sigma; //scatter
}

model {
    //priors
    alpha  ~ normal(0, 10);
    beta   ~ normal(0, 10);
    gamma  ~ normal(0, 10);
    sigma  ~ normal(0, 5);

    //likelihood
    for (i in 1:N)
        logc[i] ~ normal(alpha + beta * log(qw[i]) + gamma * log(p[i]), sigma);
}