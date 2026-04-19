data {
  int<lower=1> N;
  int<lower=1> K;
  array[N] int<lower=0, upper=1> y;
  
  matrix[N, K] time_dummies;
  vector[N] intersection;
  vector[N] weekend;
  vector[N] avg_snow_cm;
  vector[N] avg_rain_mm;
  vector[N] latitude;
  vector[N] longitude;
}


parameters {
  real beta_0; // intercept
  vector[K] beta_t; // time of day effect (there are K+1 time categories)
  real beta_i; // intersection effect
  real beta_w; // weekend effect
  real beta_snow; // snow effect
  real beta_rain; // rain effect
  real beta_lat; // latitude effect
  real beta_lon; // longitude effect
}


model {
  beta_0 ~ normal(0, 5);
  beta_t ~ normal(0, 1);
  beta_i ~ normal(0, 2);
  beta_w ~ normal(0, 1);
  beta_snow ~ normal(0, 3);
  beta_rain ~ normal(0, 3);
  beta_lat ~ normal(0, 2);
  beta_lon ~ normal(0, 2);
  
  
  y ~ bernoulli_logit(beta_0 + time_dummies*beta_t + 
                      beta_i*intersection + beta_w*weekend + 
                      beta_snow*avg_snow_cm + beta_rain*avg_rain_mm +
                      beta_lat*latitude + beta_lon*longitude);
}

