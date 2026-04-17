suppressPackageStartupMessages(library(dplyr))
# load in the 2018-2022 dataset
# the file was originally named "British Columbia_Full Data_data.csv" 
# (for both the 2018-22 and 2020-24 datasets)
crash18_raw <- read.csv("data/icbc_crashes_2018_2022.csv", sep="\t", header=TRUE, fileEncoding="utf-16")
crash18 <- crash18_raw |> rename(Derived.Crash.Configuration = Derived.Crash.Congifuration)


# change the month from words to numbers
month_names <- c("JANUARY", "FEBRUARY", "MARCH", "APRIL", "MAY", "JUNE",
                 "JULY", "AUGUST", "SEPTEMBER", "OCTOBER", "NOVEMBER", "DECEMBER")

crash18$month <- match(crash18$Month.Of.Year, month_names)
crash18$yearmonth <- crash18$Date.Of.Loss.Year + (crash18$month - 1) / 12

head(crash18)



# load in the 2020-2024 crash dataset
crash20_raw <- read.csv("data/icbc_crashes_2020_2024.csv", header=TRUE)
crash20 <- crash20_raw


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
  mutate(Cyclist.Flag = if_else(Cyclist.Flag == "Yes", 1, 0),
         Heavy.Veh.Flag = if_else(Heavy.Veh.Flag == "Yes", 1, 0),
         Intersection.Crash = if_else(Intersection.Crash == "Yes", 1, 0),
         Motorcycle.Flag = if_else(Motorcycle.Flag == "Yes", 1, 0),
         Parked.Vehicle.Flag = if_else(Parked.Vehicle.Flag == "Yes", 1, 0),
         Parking.Lot.Flag = if_else(Parking.Lot.Flag == "Yes", 1, 0),
         Pedestrian.Flag = if_else(Pedestrian.Flag == "Yes", 1, 0))

head(crash_cov)



# weather data
weather18 <- read.csv("data/yvr_weather_2018.csv", header=TRUE)
weather19 <- read.csv("data/yvr_weather_2019.csv", header=TRUE)
weather20 <- read.csv("data/yvr_weather_2020.csv", header=TRUE)
weather21 <- read.csv("data/yvr_weather_2021.csv", header=TRUE)
weather22 <- read.csv("data/yvr_weather_2022.csv", header=TRUE)
weather23 <- read.csv("data/yvr_weather_2023.csv", header=TRUE)
weather24 <- read.csv("data/yvr_weather_2024.csv", header=TRUE)

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


write.csv(crash_weather, file="crash_weather.csv")
