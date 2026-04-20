suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(require(cmdstanr))
suppressPackageStartupMessages(library(bayesplot))
suppressPackageStartupMessages(require(mcmcse))
suppressPackageStartupMessages(require(ggplot2))


# load in the crash_weather data
# if not already done, run data.R first 
# which will generate data/crash_weather.csv
df_simple <- read.csv("data/crash_weather.csv")

timeofday_matrix <- model.matrix(~ timeofday, data=df_simple)[,-1]



model2_data <- list(
  N = nrow(df_simple),
  K = ncol(timeofday_matrix),
  y = df_simple$Pedestrian.Flag,
  time_dummies = timeofday_matrix,
  intersection = df_simple$centered_intersection,
  weekend = df_simple$weekend,
  avg_snow_cm = df_simple$avg_snow_cm,
  avg_rain_mm = df_simple$avg_rain_mm,
  latitude = df_simple$centered_lat,
  longitude = df_simple$centered_lon
)


model2 <- cmdstan_model("stan/model2.stan")

dir.create(file.path("stan_out"), showWarnings=FALSE)

fit_hmc <- model2$sample(
  seed = 1,
  chains = 1,
  refresh = 500,
  iter_sampling = 2000,
  data = model2_data,
  output_dir = "stan_out"
)

fit_variational <- model2$variational(
  seed = 1,
  refresh = 500,
  output_dir = "stan_out",
  algorithm = "meanfield",
  output_samples = 2000,
  data = model2_data
)

variables <- c("beta_0", "beta_w", "beta_i", "beta_snow", "beta_rain",
               "beta_t[1]", "beta_t[2]", "beta_t[3]", "beta_t[4]",
               "beta_lat", "beta_lon")

hmc_draws <- fit_hmc$draws(variables = variables)

# save the trace plots from HMC
pdf("figs/model2_hmc_trace_plots.pdf")

for (param in variables) {
  trace_plot <- mcmc_trace(hmc_draws , pars = c(param)) +
    ggtitle(paste("Trace Plot for", param)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("MCMC Iteration")
  print(trace_plot)
}

dev.off()


vi_summary <- fit_variational$summary()

write.csv(vi_summary, "results/model2_vi_summary.csv", row.names=F)
print(vi_summary)


hmc_summary <- fit_hmc$summary()

write.csv(hmc_summary, "results/model2_hmc_summary.csv", row.names=F)
print(hmc_summary)


