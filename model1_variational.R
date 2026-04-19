suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(require(cmdstanr))


# load in the crash_weather data
# if not already done, run data.R first 
# which will generate data/crash_weather.csv
df <- read.csv("data/crash_weather.csv")


set.seed(12345)

# randomly keep only 10% of rows in the dataset, to reduce computational load

df_simple <- df |>
  slice_sample(prop = 0.1)


model1_data <- list(
  N = nrow(df_simple),
  y = df_simple$Pedestrian.Flag,
  intersection = df_simple$Intersection.Crash,
  weekend = df_simple$weekend
  #heavy = df_simple$Heavy.Veh.Flag
)


model1 <- cmdstan_model("stan/model1.stan")

dir.create(file.path("stan_out"), showWarnings=FALSE)


fit_variational <- model1$variational(
  seed = 405,
  refresh = 500,
  output_dir = "stan_out",
  algorithm = "meanfield",
  output_samples = 5000,
  data = model1_data
)

print(fit_variational$summary())




