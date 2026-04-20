suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(require(cmdstanr))

source("model1_custom_mh_functions.R")


# load in the crash_weather data
# if not already done, run data.R first 
# which will generate data/crash_weather.csv
df <- read.csv("data/crash_weather_full.csv")


set.seed(405)

# param = 1 tests beta_0, 2 tests beta_i, 3 tests beta_w

n <- 200

for (param in 1:3) {
  forward_only <- replicate(1000, forward_posterior(n, 0, df, param))
  
  with_mcmc <- replicate(1000, forward_posterior(n, 200, df, param))
  
  t_test <- t.test(forward_only, with_mcmc)
  print(t_test$p.value)
  
  
  ks_test <- ks.test(forward_only, with_mcmc)
  print(ks_test$p.value)
}
