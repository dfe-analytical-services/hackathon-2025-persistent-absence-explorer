# function to produce the leaflet visualisation:

PA_leaflet_plot <- function(df,
                            phase = c("Primary", "Secondary", "Special", "All schools"),
                            geographic_level = c("National", "Regional", "Local authority"),
                            geograpic_breakdown = NULL) {
  phase <- match.arg(phase)
  geographic_level <- match.arg(geographic_level)
  pa_wGeom <- df %>% filter(school_type == phase)
  # Adjust filtering logic depending on level chosen region_name
  if (geographic_level == "National") {
    # Show Regions if "National"
    df_selectedLvl <- pa_wGeom %>%
      filter(geographic_level == "Regional") %>%
      mutate(persistent_absence_percent = as.numeric(persistent_absence_percent))
  } else if (geographic_level == "Regional") {
    # Show LAs within chosen region
    if (is.null(geograpic_breakdown)) stop("Must supply region_name when geographic_level = 'Regional'")

    df_selectedLvl <- pa_wGeom %>%
      filter((geographic_level == "Local authority" & region_name == !!geograpic_breakdown) |
        (geographic_level == "Regional" & region_name != !!geograpic_breakdown)) %>% # we want to still plot the other regions.
      mutate(persistent_absence_percent = as.numeric(persistent_absence_percent))

    # Get the region itself to highlight.
    df_SelectOutline <- pa_wGeom %>%
      filter(geo_breakdown == !!geograpic_breakdown)
  } else if (geographic_level == "Local authority") {
    if (is.null(geograpic_breakdown)) stop("Must supply LA_name when geographic_level = 'Local authority'")
    # Show all LAs (or optionally filter to LA_name)
    region_deduced <- pa_wGeom %>%
      filter(geographic_level == "Local authority" & la_name == !!geograpic_breakdown) %>%
      pull(region_name) %>%
      unique() # get region name from LA
    df_selectedLvl <- pa_wGeom %>%
      filter((geographic_level == "Local authority" & la_name == !!geograpic_breakdown) |
        (geographic_level == "Local authority" & region_name == !!region_deduced)) %>%
      mutate(persistent_absence_percent = as.numeric(persistent_absence_percent))

    # get the LA itself to highlight.
    df_SelectOutline <- pa_wGeom %>%
      filter(geo_breakdown == !!geograpic_breakdown)
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
      label = ~ paste0("Persistent Absence: <br> <b>", geo_breakdown, "</b>: ", round(persistent_absence_percent, 1), "%") %>%
        lapply(htmltools::HTML),
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
      weight = 2,
      color = "#1d70b8",
      opacity = 1,
      options = pathOptions(pane = "outline") # force into "outline" pane
    )
  }

  return(m)
}

# PA_leaflet_plot(phase = "Secondary", geographic_level = "Region", geograpic_breakdown = "South West") # region_name="North West", LA_name="Lancashire"
