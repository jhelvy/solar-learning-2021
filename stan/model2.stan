data {
    int<lower=0> N; //number of data
    vector[N] x1; //cumCapacityKw
    vector[N] x2; //price_si
    vector[N] y; //costPerKw
}

parameters {
    real alpha;//intercept
    real beta1; //slope
    real beta2; //slope
    real<lower=0> sigma; //scatter
}

model {
    //priors
    alpha ~ normal(0, 10);
    beta1 ~ normal(0, 10);
    beta2 ~ normal(0, 10);
    sigma ~ normal(0, 1);
    
    y ~ normal(alpha + beta1 * x1 + beta2 * x2, sigma); //likelihood
}

generated quantities {
    vector[N] y_sim; //simulated data from posterior
    
    for(i in 1:N)
	    y_sim[i] = normal_rng(alpha + beta1 * x1[i] + beta2 * x2[i], sigma);
}
