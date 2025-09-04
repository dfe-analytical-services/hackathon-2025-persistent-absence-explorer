explorer_panel <- function() {
  tabPanel(
    "Explorer",
    gov_main_layout(
      # Title row
      gov_row(
        column(
          width = 12,
          h2("Dataset Explorer"),
        ),
        # End Title Row
        # Geo-select Row
        gov_row(
          # Input boxes for geographic level and geographic breakdown
          div(
            class = "geo_input_box",
            style = "min-height:100%; height = 100%; overflow-y: visible",
            layout_columns(
              selectizeInput(
                inputId = "select_geography",
                label = "Select a geographical level:",
                choices = unique(geo_mappings %>% filter(geographic_level != "Statistical neighbours (median)") %>% pull("geographic_level")),
                selected = NULL,
                multiple = FALSE,
                options = NULL
              ),
              conditionalPanel(condition = "input.select_geography != 'National'", selectizeInput(
                inputId = "geographic_breakdown",
                label = "Select a location: ",
                choices = NULL,
                selected = NULL,
                multiple = FALSE,
                options = NULL
              )),
              col_widths = c(4, 4, 4)
            ),
            # checkboxes for comparisons
            layout_columns(
              conditionalPanel(
                condition = "input.select_geography != 'National'",
                column(
                  width = 12,
                  checkbox_Input(
                    inputId = "national_comparison_checkbox",
                    cb_labels = "Compare with national",
                    checkboxIds = "Yes_national",
                    label = "",
                    hint_label = NULL,
                    small = TRUE
                  )
                )
              ),
              conditionalPanel(
                condition = "(input.select_geography == 'Local authority')",
                column(
                  width = 12,
                  checkbox_Input(
                    inputId = "region_comparison_checkbox",
                    cb_labels = "Compare with region",
                    checkboxIds = "Yes_region",
                    label = "",
                    hint_label = NULL,
                    small = TRUE
                  )
                )
              ),
              conditionalPanel(
                condition = "(input.select_geography == 'Local authority')",
                column(
                  width = 12,
                  checkbox_Input(
                    inputId = "sn_comparison_checkbox",
                    cb_labels = "Compare with statistical neighbours",
                    checkboxIds = "Yes_sn_o1",
                    label = "",
                    hint_label = NULL,
                    small = TRUE
                  )
                )
              ),
              col_widths = c(4, 4, 4)
            )
          )
        ),
        # End geo-select row
        # TabsetPanel start
        gov_row(
          div(
            tabsetPanel(
              id = "explorer_panels",
              type = "tabs",
              # Domain 1 --------------
              maps_tab(),
              # Domain 1 --------------
              plots_tab()
              # End Charts tab
            ) # TabsetPanel End
          )
        )
      )
    )
  )
}
