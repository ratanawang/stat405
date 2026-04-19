data {
  int<lower=1> N;
  array[N] int<lower=0, upper=1> y;
  
  vector[N] intersection;
  vector[N] weekend;
}


parameters {
  real beta_0; // intercept
  real beta_i; // intersection effect
  real beta_w; // weekend effect
}


model {
  beta_0 ~ normal(0, 5);
  beta_i ~ normal(0, 2);
  beta_w ~ normal(0, 1);
  
  y ~ bernoulli_logit(beta_0 + beta_i*intersection + beta_w*weekend);
}
