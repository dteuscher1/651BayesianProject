data {                  
  int<lower=1> N;           // Number of observations
  int<lower=1> K;           // Number of columns in the model matrix
  matrix[N, K] X;           // Model Matrix
  vector[N] y;              // Target variable
  real<lower=0> a;          // Hyperparameter value for variance of coefficients
  real<lower=0> b;          // Hyperparameter value for variance of coefficients
}


parameters {                
  vector[K] beta;           // Coefficient vector
  real<lower=0> sigma2;     // Variance for response variable
  real<lower=0> sigma2_beta; // Variance for coefficients
  real<lower=0> alpha; // Intercept value
}

transformed parameters { 
// Use this block to transform variance to standard deviations
  real<lower=0> sigma;  
  real<lower=0> sigma_beta;
  sigma = sqrt(sigma2);
  sigma_beta = sqrt(sigma2_beta);
}

model { 
  vector[N] mu;
  mu = alpha + X * beta;        // Creation of linear predictor; alpha + X*beta
  
  // priors
  alpha ~ normal(4, 1.5); // Prior for intercept; Conjugacy 
  sigma2_beta ~ gamma(a, b); // Prior for variance of coefficients; Not conjugate
  beta ~ normal(0, sigma_beta); // Prior for coefficients; Conjugacy
  sigma2 ~ inv_gamma(2, 10);     // Prior for variance of response; Conjugacy
  
  // likelihood
  y ~ normal(mu, sigma); // Likelihood of data
}
