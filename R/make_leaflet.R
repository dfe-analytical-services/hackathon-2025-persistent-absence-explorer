# function to produce the leaflet visualisation:

#************* aside for now **

# load the PA data for LAs and regions
load_map_data <- function() {
  pa_data_2025 <- read.csv("data/week_29_persistent_absence.csv")
  pa_LAs <- pa_data_2025 %>% filter(geographic_level == "Local authority")

  pa_Regions <- pa_data_2025 %>% filter(geographic_level == "Regional")

  # load the LA and region boundary geometries.
  # https://geoportal.statistics.gov.uk/datasets/e0e00383f27d4437ae87d26e673c2d54_0/explore
  ourLA_boundary <- st_read("data/Counties_and_Unitary_Authorities_December_2024_Boundaries_UK_BUC.geojson")
  # https://geoportal.statistics.gov.uk/datasets/d471e7de92fc43aba1050dcec35d1fb3_0/explore
  ourRegion_boundary <- st_read("data/Regions_December_2024_Boundaries_EN_BUC.geojson") %>%
    st_transform(st_crs(ourLA_boundary)) # match the crs of the LA data

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
        crs = st_crs(geometry.x)
      ), # Ensure CRS is preserved
      geo_breakdown = case_when(
        geographic_level == "National" ~ "National", # NA_character_,
        geographic_level == "Regional" ~ region_name,
        geographic_level == "Local authority" ~ la_name
      ) # for joining, reference the name of the current row
    ) %>%
    select(-c(geometry.x, geometry.y))
  # only empty geometry should be national -- true = flag.
  any(pa_wGeom %>% filter(st_is_empty(geometry)) %>% distinct(geographic_level) != "National")
  # no duplicates -- true = flag.
  nrow(pa_wGeom) != nrow(pa_data_2025)

  pa_wGeom <- st_as_sf(pa_wGeom)
  return(pa_wGeom)
}

#************* aside for now END **

PA_England_plot <- function(phase = c("Primary", "Secondary", "Special", "All schools"),
                            geographic_level = c("National", "Regional", "Local authority"),
                            region_name = NULL,
                            LA_name = NULL) {
  phase <- match.arg(phase)
  geographic_level <- match.arg(geographic_level)
  pa_wGeom <- pa_wGeom %>% filter(education_phase == phase)
  # Adjust filtering logic depending on level chosen
  if (geographic_level == "National") {
    # Show Regions if "National"
    df_selectedLvl <- pa_wGeom %>%
      filter(geographic_level == "Regional") %>%
      mutate(persistent_absence_percent = as.numeric(persistent_absence_percent))
  } else if (geographic_level == "Regional") {
    # Show LAs within chosen region
    if (is.null(region_name)) stop("Must supply region_name when geographic_level = 'Regional'")

    df_selectedLvl <- pa_wGeom %>%
      filter((geographic_level == "Local authority" & region_name == !!region_name) |
        (geographic_level == "Regional" & region_name != !!region_name)) %>% # we want to still plot the other regions.
      mutate(persistent_absence_percent = as.numeric(persistent_absence_percent))

    # Get the region itself to highlight.
    df_SelectOutline <- pa_wGeom %>%
      filter(geo_breakdown == !!region_name)
  } else if (geographic_level == "Local authority") {
    if (is.null(LA_name)) stop("Must supply LA_name when geographic_level = 'Local authority'")
    # Show all LAs (or optionally filter to LA_name)
    df_selectedLvl <- pa_wGeom %>%
      filter((geographic_level == "Local authority" & la_name == !!LA_name) |
        (geographic_level == "Local authority" & region_name == !!region_name)) %>%
      mutate(persistent_absence_percent = as.numeric(persistent_absence_percent))

    # get the LA itself to highlight.
    df_SelectOutline <- pa_wGeom %>%
      filter(geo_breakdown == !!LA_name)
  }

  # Define palette
  pal <- colorNumeric(
    palette = "YlOrRd",
    domain = df_selectedLvl$persistent_absence_percent
  )

  # Build leaflet map
  m <- leaflet(df_selectedLvl, options = leafletOptions(minZoom = 6)) %>%
    addMapPane("outline", zIndex = 410) %>% # create a pane above polygons
    addProviderTiles(
      layerId = "tiles",
      provider = "CartoDB.Positron"
    ) %>%
    addPolygons(
      fillColor = ~ pal(persistent_absence_percent),
      weight = 1,
      opacity = 1,
      color = "black",
      dashArray = "", # not dashed
      fillOpacity = 0.7,
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = ~ paste0(geo_breakdown, ": ", round(persistent_absence_percent, 1), "%"),
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
      lat1 = 48.0,
      lng2 = 4.0,
      lat2 = 61.01
    )

  # Add overlay to highlight selected region.
  if (geographic_level != "National") {
    m <- m %>% addPolygons(
      data = df_SelectOutline,
      fill = FALSE,
      weight = 1.5,
      color = "#1d70b8",
      opacity = 1,
      options = pathOptions(pane = "outline") # force into "outline" pane
    )
  }

  return(m)
}

PA_England_plot(phase = "Secondary", geographic_level = "Local authority", region_name = "North West", LA_name = "Lancashire") # region_name="North West", LA_name="Lancashire"
