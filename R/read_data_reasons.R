library(data.table, pos = 1)
library(dplyr)

reasons_ytd <- fread("./data/attendance_data_dashboard/EES_ytd_data.csv")

reasons_ytd <- reasons_ytd %>%
  mutate(geo_breakdown = case_when(
    geographic_level == "National" ~ "National", # NA_character_,
    geographic_level == "Regional" ~ region_name,
    geographic_level == "Local authority" ~ la_name
  ))

reasons_ytd <- reasons_ytd[, .(geographic_level, geo_breakdown)]

reasons_weekly <- fread("./data/attendance_data_dashboard/EES_weekly_data.csv")
reasons_weekly[, week_number := sapply(time_identifier, function(x) as.numeric(gsub("Week ", "", x)))]
latest_week <- reasons_weekly[, .(week_number = max(week_number)), by = time_period]

head(reasons_weekly, 3)
