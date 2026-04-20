suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(require(cmdstanr))
suppressPackageStartupMessages(require(mcmcse))
suppressPackageStartupMessages(require(posterior))

source("model1_custom_mh_functions.R")


# load in the crash_weather data
# if not already done, run data.R first 
# which will generate data/crash_weather.csv
df_simple <- read.csv("data/crash_weather.csv")
  


set.seed(1234)

initial_beta_0 <- rnorm(1, mean = 0, sd = 0.5)
initial_beta_i <- rnorm(1, mean = 0, sd = 0.5)
initial_beta_w <- rnorm(1, mean = 0, sd = 0.5)


n <- 5000

samples <- custom_mh(y = df_simple$Pedestrian.Flag, 
                     intersection = df_simple$centered_intersection,
                     weekend = df_simple$weekend,
                     n_iters = n,
                     beta_0 = initial_beta_0,
                     beta_i = initial_beta_i,
                     beta_w = initial_beta_w)


print(samples$acceptance_beta_0)
print(samples$acceptance_beta_i)
print(samples$acceptance_beta_w)

print(samples$runtime)

beta_0_part_trace <- samples$beta_0_trace[1000:n]
beta_i_part_trace <- samples$beta_i_trace[1000:n]
beta_w_part_trace <- samples$beta_w_trace[1000:n]

pdf("figs/model1_custom_mh_trace_plots.pdf")

plot(y=beta_0_part_trace, x=1:length(beta_0_part_trace), type="l", col="steelblue",
     main="Trace Plot for beta_0", xlab="MCMC iteration", ylab="beta_0")
plot(y=beta_i_part_trace, x=1:length(beta_i_part_trace), type="l", col="steelblue",
     main="Trace Plot for beta_i", xlab="MCMC iteration", ylab="beta_i")
plot(y=beta_w_part_trace, x=1:length(beta_w_part_trace), type="l", col="steelblue",
     main="Trace Plot for beta_w", xlab="MCMC iteration", ylab="beta_w")

dev.off()

pdf("figs/model1_custom_mh_histograms.pdf")

hist(beta_0_part_trace, main="Posterior histogram for beta_0", 
     xlab="beta_0", col="steelblue", border="white")
hist(beta_i_part_trace, main="Posterior histogram for beta_i", 
     xlab="beta_i", col="steelblue", border="white")
hist(beta_w_part_trace, main="Posterior histogram for beta_w", 
     xlab="beta_w", col="steelblue", border="white")

dev.off()

mcmc_matrix <- cbind(beta_0_part_trace, beta_i_part_trace, beta_w_part_trace)

colnames(mcmc_matrix) <- c("beta_0", "beta_i", "beta_w")

draws_obj <- as_draws_matrix(mcmc_matrix)
fit_summary <- summarise_draws(draws_obj)
print(fit_summary)

write.csv(fit_summary, "results/model1_custom_mh_summary.csv", row.names=F)
