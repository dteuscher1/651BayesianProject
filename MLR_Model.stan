data {                      // Data block
  int<lower=1> N;           // Sample size
  int<lower=1> K;           // Dimension of model matrix
  matrix[N, K] X;           // Model Matrix
  vector[N] y;              // Target variable
  real<lower=0> a;
  real<lower=0> b;
}

/* 
transformed data {          // Transformed data block. Not used presently.
} 
*/

parameters {                // Parameters block
  vector[K] beta;           // Coefficient vector
  real<lower=0> sigma2;
  real<lower=0> sigma2_beta;
  real<lower=0> alpha;// Error scale
}

transformed parameters {
  real<lower=0> sigma;
  real<lower=0> sigma_beta;
  sigma = sqrt(sigma2);
  sigma_beta = sqrt(sigma2_beta);
}

model {                     // Model block
  vector[N] mu;
  mu = alpha + X * beta;            // Creation of linear predictor
  
  // priors
  alpha ~ normal(4, 1.5);
  sigma2_beta ~ gamma(a, b);
  beta ~ normal(0, sigma_beta);
  sigma2 ~ inv_gamma(2, 10);     // With sigma bounded at 0, this is half-cauchy
  
  // likelihood
  y ~ normal(mu, sigma);
}

/*
generated quantities {      // Generated quantities block. Not used presently.
}
*/
