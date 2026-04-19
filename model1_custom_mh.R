suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(require(cmdstanr))

source("model1_custom_mh_functions.R")


# load in the crash_weather data
# if not already done, run data.R first 
# which will generate data/crash_weather.csv
df <- read.csv("data/crash_weather.csv")


set.seed(12345)

df_simple <- df |>
  slice_sample(prop = 0.1)
  

set.seed(1234)

initial_beta_0 <- rnorm(1, mean = 0, sd = 0.5)
initial_beta_i <- rnorm(1, mean = 0, sd = 0.5)
initial_beta_w <- rnorm(1, mean = 0, sd = 0.5)


n <- 2000

samples <- custom_mh(y = df_simple$Pedestrian.Flag, 
                     intersection = df_simple$Intersection.Crash,
                     weekend = df_simple$weekend,
                     n_iters = n,
                     beta_0 = initial_beta_0,
                     beta_i = initial_beta_i,
                     beta_w = initial_beta_w)


plot(samples$beta_0_trace)
plot(samples$beta_i_trace)
plot(samples$beta_w_trace)

print(samples$acceptance_beta_0)
print(samples$acceptance_beta_i)
print(samples$acceptance_beta_w)

print(samples$runtime)

beta_0_part_trace <- samples$beta_0_trace[1000:n]
beta_i_part_trace <- samples$beta_i_trace[1000:n]
beta_w_part_trace <- samples$beta_w_trace[1000:n]

plot(beta_0_part_trace, type="l")
plot(beta_i_part_trace, type="l")
plot(beta_w_part_trace, type="l")


hist(beta_0_part_trace)
hist(beta_i_part_trace)
hist(beta_w_part_trace)
