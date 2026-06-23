library(shiny)
library(tidyverse)
library(tidycensus)
library(leaflet)
library(sf)
library(scales)

# Demo: Shiny + leaflet with multiple toggleable layers.
# checkboxGroupInput controls which ACS variable layers are visible.
# Uses leafletProxy + showGroup/hideGroup to avoid redrawing the full map.
#
# Requires a Census API key. Register free at https://api.census.gov/data/key_signup.html
# then run: tidycensus::census_api_key("YOUR_KEY", install = TRUE)
#

# ── Data (pulled once at startup) ─────────────────────────────────────────────
va <- get_acs(
  geography = "county",
  state     = "VA",
  variables = c(income = "B19013_001",
                poor   = "B17001_002",
                total  = "B17001_001",
                age    = "B01002_001"),
  year      = 2022,
  geometry  = TRUE,
  output    = "wide"
) |>
  st_transform(crs = 4326) |>
  mutate(
    county       = str_remove(NAME, ", Virginia"),
    income       = incomeE,
    poverty_rate = round(poorE / totalE * 100, 1),
    median_age   = ageE
  )

# One palette per layer
pal_income  <- colorNumeric(c("#FFFFFF", "#861F41"), domain = va$income)
pal_poverty <- colorNumeric("YlOrRd",               domain = va$poverty_rate)
pal_age     <- colorNumeric("Blues",                domain = va$median_age)

all_layers <- c("Median Income", "Poverty Rate", "Median Age")

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  titlePanel("Virginia County Layer Explorer"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "layers", "Show layers:",
        choices  = all_layers,
        selected = "Median Income"
      )
    ),
    mainPanel(
      leafletOutput("map", height = 560)
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # Build the base map with all three layers added but two hidden
  output$map <- renderLeaflet({
    leaflet(va) |>
      addProviderTiles(providers$CartoDB.Positron) |>

      addPolygons(
        group       = "Median Income",
        fillColor   = ~pal_income(income),
        fillOpacity = 0.75, color = "white", weight = 1,
        popup       = ~paste0("<b>", county, "</b><br>",
                              "Median Income: ", dollar(income, accuracy = 1))
      ) |>

      addPolygons(
        group       = "Poverty Rate",
        fillColor   = ~pal_poverty(poverty_rate),
        fillOpacity = 0.75, color = "white", weight = 1,
        popup       = ~paste0("<b>", county, "</b><br>",
                              "Poverty Rate: ", poverty_rate, "%")
      ) |>

      addPolygons(
        group       = "Median Age",
        fillColor   = ~pal_age(median_age),
        fillOpacity = 0.75, color = "white", weight = 1,
        popup       = ~paste0("<b>", county, "</b><br>",
                              "Median Age: ", median_age, " years")
      ) |>

      # Start with only Median Income visible
      hideGroup("Poverty Rate") |>
      hideGroup("Median Age")
  })

  # When checkboxes change, show/hide layers AND update the legend
  observe({
    proxy <- leafletProxy("map", session)

    # Toggle each polygon group
    for (layer in all_layers) {
      if (layer %in% input$layers) proxy |> showGroup(layer)
      else                         proxy |> hideGroup(layer)
    }

    # Remove all legends then re-add only the ones that are checked
    proxy |> clearControls()

    if ("Median Income" %in% input$layers)
      proxy |> addLegend(
        "bottomleft", pal = pal_income, values = va$income,
        title    = "Median Income ($)",
        labFormat = labelFormat(prefix = "$", big.mark = ","),
        layerId  = "legend_income"
      )

    if ("Poverty Rate" %in% input$layers)
      proxy |> addLegend(
        "bottomright", pal = pal_poverty, values = va$poverty_rate,
        title    = "Poverty Rate (%)",
        labFormat = labelFormat(suffix = "%"),
        layerId  = "legend_poverty"
      )

    if ("Median Age" %in% input$layers)
      proxy |> addLegend(
        "topright", pal = pal_age, values = va$median_age,
        title   = "Median Age (years)",
        layerId = "legend_age"
      )
  })
}

shinyApp(ui, server)
