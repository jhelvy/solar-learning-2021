data {
    int<lower=0> N; //number of data
    vector[N] x; //covariates
    vector[N] y; //variates
}

parameters {
    real<lower=0> alpha; //intercept
    real<upper=0> beta;  //slope
    real<lower=0> sigma; //scatter
}

model {
    //priors
    alpha ~ normal(0, 10);
    beta ~ normal(0, 10);
    sigma ~ normal(0, 5);
    
    //likelihood
    for (i in 1:N)
        y[i] ~ normal(alpha + beta * x[i], sigma);
}

generated quantities {
    vector[N] y_sim; //simulated data from posterior
    
    for(i in 1:N)
	    y_sim[i] = normal_rng(alpha + beta * x[i], sigma);
}
