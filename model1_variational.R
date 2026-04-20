suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(require(cmdstanr))


# load in the crash_weather data
# if not already done, run data.R first 
# which will generate data/crash_weather.csv
df_simple <- read.csv("data/crash_weather.csv")


model1_data <- list(
  N = nrow(df_simple),
  y = df_simple$Pedestrian.Flag,
  intersection = df_simple$centered_intersection,
  weekend = df_simple$weekend
)


model1 <- cmdstan_model("stan/model1.stan")

dir.create(file.path("stan_out"), showWarnings=FALSE)


fit_variational <- model1$variational(
  seed = 405,
  refresh = 500,
  output_dir = "stan_out",
  algorithm = "meanfield",
  output_samples = 4000,
  data = model1_data
)

vi_summary <- fit_variational$summary()

write.csv(vi_summary, "results/model1_vi_summary.csv", row.names=F)
print(vi_summary)




