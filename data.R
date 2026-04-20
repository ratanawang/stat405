suppressPackageStartupMessages(library(dplyr))

REPO_BASE_URL <- "https://github.com/ratanawang/stat405/releases/download/data"

# load in the 2018-2022 dataset
# the file was originally named "British Columbia_Full Data_data.csv"
# (for both the 2018-22 and 2020-24 datasets)
crash18_raw <- read.csv(paste0(REPO_BASE_URL, "/icbc_crashes_2018_2022.csv"), sep = "\t", header = TRUE, fileEncoding = "utf-16")
crash18 <- crash18_raw |> rename(Derived.Crash.Configuration = Derived.Crash.Congifuration)

# change the month from words to numbers
month_names <- c("JANUARY", "FEBRUARY", "MARCH", "APRIL", "MAY", "JUNE",
                 "JULY", "AUGUST", "SEPTEMBER", "OCTOBER", "NOVEMBER", "DECEMBER")

crash18$month <- match(crash18$Month.Of.Year, month_names)
crash18$yearmonth <- crash18$Date.Of.Loss.Year + (crash18$month - 1) / 12

head(crash18)

# load in the 2020-2024 crash dataset
crash20 <- read.csv(paste0(REPO_BASE_URL, "/icbc_crashes_2020_2024.csv"), header = TRUE)
crash20$month <- match(crash20$Month.Of.Year, month_names)
crash20$yearmonth <- crash20$Date.Of.Loss.Year + (crash20$month - 1) / 12

head(crash20)

# filter out 2020-2022 crashes so only 2023-2024 crashes remain
# so we can then combine the two datasets to have 2018-2024 data
crash23 <- crash20 |> filter(yearmonth >= 2023)

# rename columns in crash18 to match those in crash23
crash18 <- crash18|> rename(Animal.Flag = Animal.Involved,
                            Cyclist.Flag = Cyclist.Involved,
                            Heavy.Veh.Flag = Heavy.Veh.Involved,
                            Motorcycle.Flag = Motorcycle.Involved,
                            Parked.Vehicle.Flag = Parked.Vehicle.Involved,
                            Parking.Lot.Flag = Parking.Lot.Crash,
                            Pedestrian.Flag = Pedestrian.Involved)



# combine the crash18 and crash23 data frames
crash <- bind_rows(crash18, crash23)
head(crash)

# view crashes only in the City of Vancouver, with known latitude/longitude
# and remove some unnecessary rows

crash_cov <- crash |>
  filter(Municipality.Name == "VANCOUVER") |>
  filter(!is.na(Latitude) & !is.na(Longitude)) |>
  select(-Crash.Breakdown.2, -Region, -Animal.Flag, -Month.Of.Year, -Metric.Selector,
         -Municipality.Name..ifnull., -Municipality.With.Boundary, -month) |>
  mutate(
    Cyclist.Flag = if_else(Cyclist.Flag == "Yes", 1, 0),
    Heavy.Veh.Flag = if_else(Heavy.Veh.Flag == "Yes", 1, 0),
    Intersection.Crash = if_else(Intersection.Crash == "Yes", 1, 0),
    Motorcycle.Flag = if_else(Motorcycle.Flag == "Yes", 1, 0),
    Parked.Vehicle.Flag = if_else(Parked.Vehicle.Flag == "Yes", 1, 0),
    Parking.Lot.Flag = if_else(Parking.Lot.Flag == "Yes", 1, 0),
    Pedestrian.Flag = if_else(Pedestrian.Flag == "Yes", 1, 0),
    weekend = if_else(Day.Of.Week %in% c("SATURDAY", "SUNDAY"), 1, 0)
  )

head(crash_cov)

# weather data
weather18 <- read.csv(paste0(REPO_BASE_URL, "/yvr_weather_2018.csv"), header = TRUE)
weather19 <- read.csv(paste0(REPO_BASE_URL, "/yvr_weather_2019.csv"), header = TRUE)
weather20 <- read.csv(paste0(REPO_BASE_URL, "/yvr_weather_2020.csv"), header = TRUE)
weather21 <- read.csv(paste0(REPO_BASE_URL, "/yvr_weather_2021.csv"), header = TRUE)
weather22 <- read.csv(paste0(REPO_BASE_URL, "/yvr_weather_2022.csv"), header = TRUE)
weather23 <- read.csv(paste0(REPO_BASE_URL, "/yvr_weather_2023.csv"), header = TRUE)
weather24 <- read.csv(paste0(REPO_BASE_URL, "/yvr_weather_2024.csv"), header = TRUE)

weather_dfs <- list(y2018 = weather18, 
                    y2019 = weather19, 
                    y2020 = weather20, 
                    y2021 = weather21,
                    y2022 = weather22, 
                    y2023 = weather23, 
                    y2024 = weather24)


# select columns to keep and create a "yearmonth" column for easier plotting/EDA
# weather[1] is 2018 data, weather[2] is 2019, ... , weather[7] is 2024

for (i in 1:length(weather_dfs)) {
  weather_dfs[[i]] <- weather_dfs[[i]] |>
    select(Date.Time, Year, Month, Day, Max.Temp...C., Min.Temp...C., Mean.Temp...C.,
           Total.Rain..mm., Total.Snow..cm., Total.Precip..mm.)
}

# all weather from 2018 to 2024
weather <- bind_rows(weather_dfs)

head(weather)


# take the average by month
weather_monthly <- weather |>
  group_by(Year, Month) |>
  summarize(
    avg_max_temp = mean(Max.Temp...C., na.rm=T),
    avg_min_temp = mean(Min.Temp...C., na.rm=T),
    avg_mean_temp = mean(Mean.Temp...C., na.rm=T),
    avg_rain_mm = mean(Total.Rain..mm., na.rm=T),
    avg_snow_cm = mean(Total.Snow..cm., na.rm=T),
    avg_precip_mm = mean(Total.Precip..mm., na.rm=T)
  ) |>
  mutate(yearmonth = Year + (Month - 1) / 12)

weather_monthly


crash_weather <- merge(crash_cov, weather_monthly, by="yearmonth") |>
  select(-Date.Of.Loss.Year)



crash_weather$Time.Category <- as.factor(crash_weather$Time.Category)

# combine some time categories, so that we have fewer parameters in our model
crash_weather <- crash_weather %>%
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

# center/standardize latitude and longitude, and center intersection

mean_lat <- mean(crash_weather$Latitude)
mean_lon <- mean(crash_weather$Longitude)

sd_lat <- sd(crash_weather$Latitude)
sd_lon <- sd(crash_weather$Longitude)

crash_weather <- crash_weather |>
  mutate(
    centered_lat = (Latitude - mean_lat) / sd_lat,
    centered_lon = (Longitude - mean_lon) / sd_lon,
    centered_intersection = Intersection.Crash - mean(Intersection.Crash, na.rm=T)
  )

set.seed(12345)

crash_weather_simple <- crash_weather |>
  slice_sample(prop = 0.10)


dir.create("data", showWarnings = FALSE)
write.csv(crash_weather, file="data/crash_weather_full.csv")
write.csv(crash_weather_simple, file="data/crash_weather.csv")
