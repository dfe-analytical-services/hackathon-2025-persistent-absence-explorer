### functions to plot figures

# packages required:
# dplyr
# ggplot
# tidyr
# reactable
# shiny
# shinyGovstyle

# prepare filtered data

# read in data
week_29_pa <- read.csv("data/week_29_persistent_absence.csv")
pa_merged <- read.csv("data/persistent_absence_merged.csv")

#
glimpse(week_29_pa)

# Prepare data
week_29_pa <- week_29_pa %>%
  # mutate time identifier to numeric (and remove text)
  mutate(
    persistent_absence_percent = as.numeric(persistent_absence_percent),
    geo_breakdown = case_when(
      geographic_level == "National" ~ "National", # NA_character_,
      geographic_level == "Regional" ~ region_name,
      geographic_level == "Local authority" ~ la_name
    ),
    time_identifier = as.numeric(gsub("Week ", "", time_identifier)),
    geographic_level = factor(geographic_level,
      levels = c(
        "National",
        "Regional",
        "Local authority"
      )
    )
  )

# mock up duplicate years
pa_24 <- week_29_pa %>%
  mutate(
    time_period = 2024,
    persistent_absence_percent = persistent_absence_percent - 2
  )
pa_23 <- week_29_pa %>%
  mutate(
    time_period = 2023,
    persistent_absence_percent = persistent_absence_percent - 1
  )
pa_series <- week_29_pa %>%
  bind_rows(pa_24) %>%
  bind_rows(pa_23)

# mock input
# Create the DataFrame
input <- data.frame(
  la_choice = "Hartlepool",
  geographic_level = "Local authority",
  education_phase = "Secondary",
  stringsAsFactors = FALSE
)


# get parent region of selected LA
parent_region <- "North East"
parent_region <- pa_series %>% # should probably be a separate geog lookup
  filter(la_name == input$la_choice) %>%
  pull(region_name) %>%
  unique()



# what are we trying to display?

# TABLES
# GRAPHS

# create summary statistics for each breakdown, to use as comparators in tables / figures

# challenges:
# 1: We only have percentages, so can't actually calculate any additional statistics
# 2: Not sure how the plots will interact with shiny / dashboard, i.e. when we've
#     got filters to a specific LA, how will this actually affect the data being used
#     to plot the chart (i.e. what data will remain, how will the 'selected' LA be
#     differentiated from others?)


# 1: Create table

# This probably isn't the best way to get diff to region but was tight for time

# create regional comparison
pa_series_regions <- pa_series %>%
  filter(geographic_level == "Regional") %>%
  distinct() %>%
  rename(persistent_absence_percent_regional = persistent_absence_percent)

# create national comparison
pa_series_national <- pa_series %>%
  filter(geographic_level == "National") %>%
  distinct() %>%
  rename(persistent_absence_percent_national = persistent_absence_percent)

# join
pa_series_LA <- pa_series %>%
  filter(geographic_level == "Local authority") %>%
  left_join(., select(
    pa_series_regions,
    time_period,
    time_identifier,
    time_frame,
    region_name,
    region_code,
    education_phase,
    persistent_absence_percent_regional
  ),
  by = c(
    "time_period",
    "time_identifier",
    "time_frame",
    "region_name",
    "region_code",
    "education_phase"
  )
  ) %>%
  left_join(., select(
    pa_series_national,
    time_period,
    time_identifier,
    time_frame,
    country_name,
    country_code,
    education_phase,
    persistent_absence_percent_national
  ),
  by = c(
    "time_period",
    "time_identifier",
    "time_frame",
    "country_name",
    "country_code",
    "education_phase"
  )
  ) %>%
  mutate(
    persistent_absence_percent = as.numeric(persistent_absence_percent),
    persistent_absence_percent_regional = as.numeric(persistent_absence_percent_regional),
    persistent_absence_percent_national = as.numeric(persistent_absence_percent_national),
    regional_difference = persistent_absence_percent - persistent_absence_percent_regional,
    national_difference = persistent_absence_percent - persistent_absence_percent_national
  )
## some NAs

# order by persistent absence (for most recent year, but could filter on this var?)
pa_series_LA %>%
  arrange(
    time_period,
    time_identifier,
    time_frame,
    persistent_absence_percent
  ) %>%
  View()

# order by regional difference
pa_series_LA %>%
  arrange(
    time_period,
    time_identifier,
    time_frame,
    persistent_absence_percent
  )






# 1: Persistent absence rate of selected local authorities compared to previous years
# with National, Regional, Statistical neighbours as well? toggleable?
# Not sure how the filtering works, i.e. after the

# plot line chart
plot_line_chart <- function(filtered_data, input) {
  pa_series %>%
    filter(
      geographic_level == "National" |
        (geographic_level == "Regional" &
          region_name %in% parent_region) |
        (geographic_level == "Local authority" &
          la_name == input$la_choice),
      # take highest value of time_identifier
      time_identifier == max(time_identifier),
      education_phase == input$education_phase
    ) %>%
    group_by(
      geographic_level,
      la_name
    ) %>%
    ggplot(aes(
      x = time_period, y = persistent_absence_percent,
      group = geographic_level, colour = geographic_level
    )) +
    geom_line(size = 1) +
    theme_bw() +
    labs(
      title = paste0(
        "Persistent Absence rates over time in ",
        input$la_choice,
        ", ",
        parent_region,
        ", and England"
      ),
      x = "Persistent Absence (%)",
      y = "Time period"
    ) +
    scale_x_continuous(breaks = seq(min(pa_series$time_period),
      max(pa_series$time_period),
      by = 1
    ))
}



# plot bar chart if time?
plot_bar_chart <- function(filtered_data, input) {
  pa_series %>%
    filter(
      geographic_level == "National" |
        (geographic_level == "Regional" &
          region_name %in% parent_region) |
        (geographic_level == "Local authority" &
          la_name == input$la_choice),
      # take highest value of time_identifier
      time_identifier == max(time_identifier),
      education_phase == input$education_phase
    ) %>%
    group_by(
      geographic_level,
      la_name
    ) %>%
    ggplot(aes(
      x = time_period, y = persistent_absence_percent,
      group = geographic_level, colour = geographic_level
    )) +
    geom_line(size = 1) +
    theme_bw() +
    labs(
      title = paste0(
        "Persistent Absence rates over time in ",
        input$la_choice,
        ", ",
        parent_region,
        ", and England"
      ),
      x = "Persistent Absence (%)",
      y = "Time period"
    ) +
    scale_x_continuous(breaks = seq(min(pa_series$time_period),
      max(pa_series$time_period),
      by = 1
    ))
}





# 2: Table of Local Authority persistent absence rates, ordered by PA rate
# 2.1: Table of Local Authority persistent absence rates, ordered by diff to benchmark
# 2.2: Could also get difference compared to previous year?
check <- pa_series %>%
  pivot_wider(
    names_from = time_period,
    values_from = persistent_absence_percent
  ) %>%
  filter(
    geographic_level == "National" |
      (region_name %in% parent_region),
    # take highest value of time_identifier
    time_identifier == max(time_identifier),
    education_phase == input$education_phase
  ) %>%
  select(
    geographic_level,
    region_name,
    la_name,
    education_phase,
    `2025`,
    `2024`,
    `2023`
  ) %>%
  arrange(
    geographic_level,
    `2025`
  )

reactable(check,
  rowStyle = function(index) {
    if (check$la_name[index] == input$la_choice) {
      list(fontWeight = "bold")
    } else {
      NULL
    }
  }
)


output$my_table <- renderReactable({
  reactable(check,
    rowStyle = function(index) {
      if (check$la_name[index] == input$la_choice) {
        list(fontWeight = "bold")
      } else {
        NULL
      }
    }
  )
})





# compare LA time series to other LAs within region (highlighted in diff colour)
# bar chart

# function receives a reactive dataset

# reactive

# input would be basically,
# variables useful in title, but data will be pre-filtered based on parameters
# reactive values would be a list - object 1 is the dataset, object 2 is the user selections

# think about dropdowns around school types
