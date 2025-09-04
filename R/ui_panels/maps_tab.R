maps_tab <- function() {
  tabPanel(
    "Maps",
    fluidRow(
      br(),
      column(
        width = 6,
        leafletOutput("map_PA", height = 800) # renderLeaflet()
      ),
      column(
        width = 6,
        "Hello"
      ),
    ),
  )
}
