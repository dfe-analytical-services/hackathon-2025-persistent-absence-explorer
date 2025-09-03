# function to produce the leaflet visualisation:

#************* aside for now **

# load the PA data for LAs and regions
pa_data_2025 <- read.csv("data/week_29_persistent_absence.csv")
pa_LAs <- pa_data_2025 %>% filter(geographic_level == "Local authority")

pa_Regions <- pa_data_2025 %>% filter(geographic_level == "Regional")

# load the LA and region boundary geometries.
# https://geoportal.statistics.gov.uk/datasets/e0e00383f27d4437ae87d26e673c2d54_0/explore
ourLA_boundary <- st_read("data/Counties_and_Unitary_Authorities_December_2024_Boundaries_UK_BUC.geojson")
# https://geoportal.statistics.gov.uk/datasets/d471e7de92fc43aba1050dcec35d1fb3_0/explore
ourRegion_boundary <- st_read("data/Regions_December_2024_Boundaries_EN_BUC.geojson")

# join to PA data for the right geo level.
pa_wGeom <- pa_data_2025 %>%
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
any(!st_is_empty(pa_wGeom$geometry.x) & !st_is_empty(pa_wGeom$geometry.y))

pa_wGeom <- pa_wGeom %>%
  mutate(
    geometry = st_sfc(
      ifelse(
        st_is_empty(geometry.x),
        geometry.y,
        geometry.x
      ),
      crs = st_crs(geometry.x) # Ensure CRS is preserved
    )
  ) %>%
  select(-c(geometry.x, geometry.y))
# only empty geometry should be national -- true = flag.
any(pa_wGeom %>% filter(st_is_empty(geometry)) %>% distinct(geographic_level) != "National")
# no duplicates -- true = flag.
nrow(pa_wGeom) != nrow(pa_data_2025)


pa_wGeom <- st_as_sf(pa_wGeom)

#************* aside for now END **

# function to produce the leaflet visualisation:

# PA_England_plot <- function(geographic_level){
# Filter to Local Authority level
df_selectedLvl <- pa_wGeom %>%
  filter(geographic_level == "Local authority") %>%
  mutate(persistent_absence_percent = as.numeric(persistent_absence_percent))

# Define a palette function up front
pal <- colorNumeric(
  palette = "YlOrRd",
  domain = df_selectedLvl$persistent_absence_percent
)

leaflet(df_selectedLvl, options = leafletOptions(minZoom = 6)) %>%
  addProviderTiles(
    layerId = "tiles",
    provider = "CartoDB.Positron"
  ) %>%
  addPolygons(
    fillColor = ~ pal(persistent_absence_percent),
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = ~ paste0(new_la_code, ": ", round(persistent_absence_percent, 1), "%"),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) %>%
  addLegend(
    pal = pal,
    values = ~persistent_absence_percent,
    opacity = 0.7,
    title = "Persistent Absence (%)",
    position = "bottomright"
  ) %>%
  setMaxBounds(
    lng1 = -9.0,
    lat1 = 48.0, # more down (south)
    lng2 = 4.0, # more right (east)
    lat2 = 61.01
  )
# }
