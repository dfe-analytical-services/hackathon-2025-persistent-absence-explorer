map_panel <- function() {
  tabsetPanel(
    tabPanel(
      "Map",
      sidebarLayout(
        sidebarPanel(
          selectInput("level", "Geographic level:",
            choices = c("National", "Regional", "Local authority"),
            selected = "National"
          ),
          uiOutput("region_ui"),
          uiOutput("la_ui")
        ),
        mainPanel(
          leafletOutput("map", height = "800px")
        )
      )
    )
  )
}
