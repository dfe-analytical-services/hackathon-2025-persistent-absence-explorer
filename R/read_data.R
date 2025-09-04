# -----------------------------------------------------------------------------
# Script where we provide functions to read in the data file(s).
#
# IMPORTANT: Data files pushed to GitHub repositories are immediately public.
# You should not be pushing unpublished data to the repository prior to your
# publication date. You should use dummy data or already-published data during
# development of your dashboard.
#
# In order to help prevent unpublished data being accidentally published, the
# template will not let you make a commit if there are unidentified csv, xlsx,
# tex or pdf files contained in your repository. To make a commit, you will need
# to either add the file to .gitignore or add an entry for the file into
# datafiles_log.csv.
# -----------------------------------------------------------------------------

# Geo-Mappings
read_geo_mappings <- function() {
  data.table::fread("./data/geo_mappings.csv")
}


# Revenue data ----------------------------------------------------------------
read_revenue_data <- function(file = "data/la_maintained_schools_revenue_reserve_final.csv") {
  # This reads in an example file. For the purposes of this demo, we're using
  # the LA expenditure data downloaded from an EES release
  df_revenue <- read.csv(file)

  df_revenue <- df_revenue %>% mutate(
    # Convert 6 digit year to 4 digit for end year
    year = as.numeric(paste0("20", substr(format(time_period), 5, 6))),

    # Create a flat column listing all locations
    area_name = case_when(
      geographic_level == "National" ~ country_name,
      geographic_level == "Regional" ~ region_name,
      .default = la_name
    )
  )
  return(df_revenue)
}

# Upper Tier data ----------------------------------------------------------------

# read_upper_tier_data <- function(file = "data/Local_Authority_Districts_All_simplified.geojson") {
#   df_upper_tier <- sf::read_sf(file)
#   return(df_upper_tier)
# }

load_PA_data <- function() {
  PA <- read.csv("data/persistent_absence_merged.csv")
}
# Read mapping data to append to PA ----------------------------------------------
load_map_data_wPA <- function(PA_data) {
  # pa_data_2025 <- read.csv("data/week_29_persistent_absence.csv")

  # load the LA and region boundary geometries.
  # https://geoportal.statistics.gov.uk/datasets/e0e00383f27d4437ae87d26e673c2d54_0/explore
  ourLA_boundary <- st_read("data/Counties_and_Unitary_Authorities_December_2024_Boundaries_UK_BUC.geojson")
  # https://geoportal.statistics.gov.uk/datasets/d471e7de92fc43aba1050dcec35d1fb3_0/explore
  ourRegion_boundary <- st_read("data/Regions_December_2024_Boundaries_EN_BUC.geojson") %>%
    st_transform(st_crs(ourLA_boundary)) # match the crs of the LA data

  # join to PA data for the right geo level.
  pa_wGeom <- PA_data %>% # prev pa_data_2025
    filter((time_period == 2025 & time_identifier == "Week 29")) %>%
    left_join(
      ourLA_boundary %>% select(CTYUA24CD, geometry) %>% mutate(geographic_level = "Local authority"),
      by = c("new_la_code" = "CTYUA24CD", "geographic_level")
    ) %>%
    # join region geom
    left_join(
      ourRegion_boundary %>% select(RGN24CD, geometry) %>% mutate(geographic_level = "Regional"),
      by = c("region_code" = "RGN24CD", "geographic_level")
    )
  # test for empty with st_is_empty
  if (any(!st_is_empty(pa_wGeom$geometry.x) & !st_is_empty(pa_wGeom$geometry.y))) {
    stop("Couldn't merge geometries")
  }

  pa_wGeom <- pa_wGeom %>%
    mutate(
      geometry = st_sfc(
        ifelse(
          st_is_empty(geometry.x),
          geometry.y,
          geometry.x
        ),
        crs = st_crs(geometry.x)
      ), # Ensure CRS is preserved
      geo_breakdown = case_when(
        geographic_level == "National" ~ "National", # NA_character_,
        geographic_level == "Regional" ~ region_name,
        geographic_level == "Local authority" ~ la_name
      ) # for joining, reference the name of the current row
    ) %>%
    select(-c(geometry.x, geometry.y))

  # browser()
  # only empty geometry should be national -- true = flag.
  if (any(pa_wGeom %>% filter(st_is_empty(geometry)) %>% distinct(geographic_level) != "National")) {
    stop("some LA/regional geometries are empty")
  }
  # no duplicates -- true = flag -- not true since base data larger
  # if(nrow(pa_wGeom) != nrow(PA_data)){stop("duplicates created in joining map data")}

  pa_wGeom <- st_as_sf(pa_wGeom)
  return(pa_wGeom)
}
