library(fmi2)
library(dplyr)
library(sf)
library(lubridate)
library(ggplot2)
library(ggpmisc)

stn_fmisid <- 101154 # Lammi, change as needed
starttime.char <- "2023-06-01 21:00" # UTC midnight in Finland

if (!file.exists("fmi-weather-data-wide.Rda")) {
  # Used only once or when replacing all data
  starttime <- ymd_hm(starttime.char, tz = "UTC")
  wide_weather_data <- data.frame()
} else {
  load("fmi-weather-data-wide.Rda")
  # we start 59 min after end of previously downloaded data
  starttime <-force_tz(max(wide_weather_data$time), tzone = "UTC") + minutes(59)
}

endtime <- trunc(now(), units = "mins")

# we read the new data to a new dataframe
# (to avoid appending repeatedly to a long one)
new_wide_data <- data.frame()
while (starttime < endtime) {
  sliceendtime <- starttime + days(28) # keep query size at max of 4 weeks
  if (sliceendtime > endtime) {
    sliceendtime <- endtime
  }
  lammi_data <- obs_weather_hourly(starttime = as.character(starttime),
                                   endtime = as.character(sliceendtime),
                                   fmisid = stn_fmisid)

  slice_data <- lammi_data %>%
    tidyr::spread(variable, value) %>%
    # convert the sf object into a regular tibble
    sf::st_set_geometry(NULL)

  new_wide_data <- rbind(new_wide_data, slice_data)
  starttime <- sliceendtime + minutes(1)
  cat(".")
}

range(new_wide_data$time) # freshly read

wide_weather_data <- rbind(wide_weather_data, new_wide_data)
range(wide_weather_data$time) # all data to be saved

colnames(wide_weather_data)

save(wide_weather_data, file = "fmi-weather-data-wide.Rda")

fmi2::describe_variables(colnames(wide_weather_data)[-1])

ggplot(wide_weather_data, aes(with_tz(time, tzone = "EET"), TA_PT1H_AVG)) +
  geom_line()


