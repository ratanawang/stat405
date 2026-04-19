suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(require(cmdstanr))
suppressPackageStartupMessages(library(bayesplot))
suppressPackageStartupMessages(require(mcmcse))
suppressPackageStartupMessages(require(ggplot2))


# load in the crash_weather data
# if not already done, run data.R first 
# which will generate data/crash_weather.csv
df <- read.csv("data/crash_weather.csv")



df$Time.Category <- as.factor(df$Time.Category)

df <- df %>%
  mutate(timeofday = case_when(
    Time.Category %in% c("00:00-02:59", "03:00-05:59") ~ "Late Night",
    Time.Category %in% c("06:00-08:59") ~ "Morning Rush",
    Time.Category %in% c("09:00-11:59", "12:00-14:59") ~ "Midday",
    Time.Category %in% c("15:00-17:59") ~ "Afternoon Rush",
    Time.Category %in% c("18:00-20:59", "21:00-23:59") ~ "Evening",
    TRUE ~ "Other"
  )) %>%
  mutate(timeofday = factor(timeofday, 
                            levels = c("Late Night", "Morning Rush", "Midday", "Afternoon Rush", "Evening")))



set.seed(12345)

df_simple <- df |>
  slice_sample(prop = 0.10)


mean_lat <- mean(df_simple$Latitude)
mean_lon <- mean(df_simple$Longitude)

sd_lat <- sd(df_simple$Latitude)
sd_lon <- sd(df_simple$Longitude)

df_simple <- df_simple |>
  mutate(
    centered_lat = (Latitude - mean_lat) / sd_lat,
    centered_lon = (Longitude - mean_lon) / sd_lon,
    centered_intersection = Intersection.Crash - mean(Intersection.Crash, na.rm=T)
  )


time_matrix <- model.matrix(~ Time.Category, data=df_simple)[,-1]
timeofday_matrix <- model.matrix(~ timeofday, data=df_simple)[,-1]

head(timeofday_matrix)

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

print(fit_hmc$summary())

print(fit_variational$summary())

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



