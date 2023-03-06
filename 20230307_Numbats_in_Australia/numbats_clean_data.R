# Cleaning script provided at
# https://github.com/numbats/numbats-tidytuesday/blob/main/code/data.R Slightly
# updated here.

library(galah) # API to ALA
library(lubridate)
library(tidyverse)
library(rnoaa)
library(here)

# Downloading data is free but you need to 
# create an account https://www.ala.org.au then
# use this email address to pull data.
# galah_config(email = YOUR_EMAIL_ADDRESS)
id <- galah_identify("numbat")

numbats <- atlas_occurrences(identify = id)
numbats <- numbats %>%
  mutate(
    year = year(eventDate),
    month = month(eventDate, label=TRUE, abbr=TRUE),
    wday = wday(eventDate, label=TRUE, abbr=TRUE, week_start = 1),
    hour = hour(eventDate),
    day = ymd(as.Date(eventDate))
  )

narrogin <- meteo_pull_monitors(
  monitors = "ASN00010614",
  var = c("PRCP", "TMAX", "TMIN"),
  date_min = "2005-01-01",
  date_max = "2023-02-23")

narrogin %>%
  pivot_longer(cols = prcp:tmin, names_to = "var", values_to = "value") %>%
  mutate(day = lubridate::yday(date), year = lubridate::year(date)) %>%
  ggplot(aes(x = day, y= year, fill = is.na(value))) +
  geom_tile() +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  facet_wrap(vars(var), ncol = 1) +
  scale_fill_brewer(palette = "Dark2", name = "missing") +
  xlab("Day of the year")

narrogin_latlon <- tibble(lon = 117.1782, lat = -32.9310)

within_rad <- function(x, y, lon, lat, km) {
  deg <- km/111
  inside <- sqrt((lon-x)^2 + (lat-y)^2) < deg
  return(inside)
}

# Only sites within 50km radius of Narrogin weather station
# which is Dryandra Woodlands
numbats <- numbats %>%
  mutate(
    dryandra = within_rad(
      decimalLongitude, decimalLatitude, 
      narrogin_latlon$lon, narrogin_latlon$lat,
      50
    )
  )

numbats <- numbats %>% 
  left_join(narrogin, by = join_by(day == date)) %>%
  mutate(
    prcp = if_else(dryandra, prcp, NA, missing = NA),
    tmax = if_else(dryandra, tmax, NA, missing = NA),
    tmin = if_else(dryandra, tmin, NA, missing = NA)
  ) %>%
  select(-id)

# Things are only in this dataset if they were PRESENT.
numbats <- numbats |> 
  select(-occurrenceStatus)

# Those last three values are in values to coerce them to integers, and might be
# confusing. Translate them to doubles.
numbats <- numbats |> 
  mutate(
    prcp = prcp/10,
    tmax = tmax/10,
    tmin = tmin/10
  )

write_csv(numbats, "20230307_Numbats_in_Australia/numbats.csv")
