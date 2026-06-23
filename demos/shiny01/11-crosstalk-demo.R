library(tidyverse)
library(tidycensus)
library(crosstalk)
library(plotly)
library(DT)
library(leaflet)
library(sf)
library(scales)

# Demo: crosstalk -- DT table + plotly scatter + leaflet map, all linked.
# Click a row in the table -> the scatter and map highlight that county.
# Lasso/box-select points in the scatter -> table and map update to match.
#
# HOW TO RUN: Select all lines and press Ctrl+Enter (or Source).
# Output appears in the RStudio Viewer pane.
#
# Requires a Census API key. Register free at https://api.census.gov/data/key_signup.html
# then run: tidycensus::census_api_key("YOUR_KEY", install = TRUE)
#

# ── Pull Virginia county ACS data ─────────────────────────────────────────────
va_raw <- get_acs(
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
  st_transform(crs = 4326)

# Compute county centroids for the map markers
va <- va_raw |>
  mutate(
    county       = str_remove(NAME, ", Virginia"),
    income       = incomeE,
    poverty_rate = round(poorE / totalE * 100, 1),
    median_age   = ageE,
    lng          = st_coordinates(st_centroid(geometry))[, 1],
    lat          = st_coordinates(st_centroid(geometry))[, 2]
  ) |>
  st_drop_geometry() |>
  select(county, income, poverty_rate, median_age, lng, lat)

# ── SharedData: the single object that links all three widgets ────────────────
shared_va <- SharedData$new(va)

# ── Layout: table on top, scatter + map side by side below ───────────────────
bscols(
  widths = 12,

  # Table -- click a row to select; selection propagates to scatter and map
  datatable(
    shared_va,
    rownames = FALSE,
    colnames = c("County", "Median Income ($)", "Poverty Rate (%)",
                 "Median Age", "lng", "lat"),
    options  = list(
      pageLength  = 8,
      scrollX     = TRUE,
      # Hide the lng/lat columns -- needed by leaflet but not shown to users
      columnDefs  = list(list(visible = FALSE, targets = c(4, 5)))
    ),
    selection = "single"
  ),

  bscols(
    widths = c(6, 6),

    # Scatter: income vs poverty rate -- brush to select a group of counties
    plot_ly(
      shared_va,
      x      = ~income,
      y      = ~poverty_rate,
      text   = ~county,
      type   = "scatter",
      mode   = "markers",
      marker = list(color = "#861F41", size = 8, opacity = 0.7)
    ) |>
      layout(
        xaxis = list(title = "Median Income ($)",
                     tickformat = "$,"),
        yaxis = list(title = "Poverty Rate (%)")
      ),

    # Map: one marker per county -- updates when table row or scatter is selected
    leaflet(shared_va) |>
      addTiles() |>
      addCircleMarkers(
        lng         = ~lng,
        lat         = ~lat,
        radius      = 6,
        color       = "#861F41",
        fillColor   = "#E5751F",
        fillOpacity = 0.7,
        popup       = ~paste0(
          "<b>", county, "</b><br>",
          "Income: ", dollar(income, accuracy = 1), "<br>",
          "Poverty: ", poverty_rate, "%<br>",
          "Median Age: ", median_age
        )
      )
  )
)
