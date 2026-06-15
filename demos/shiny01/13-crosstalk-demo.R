# Demo: Linked Widgets with crosstalk (Chart + Table + Map)
# DSPG 2026 | Virginia Tech
#
# crosstalk links a plotly chart, a DT table, and a leaflet map so that
# clicking or filtering one widget instantly updates the others —
# with NO Shiny server. The output is a static HTML file.
#
# Run each section with Ctrl+Enter and observe the linked filtering.

library(tidyverse)
library(crosstalk)
library(plotly)
library(DT)
library(leaflet)
library(htmltools)

# ── Dataset ───────────────────────────────────────────────────────────────────
cities <- tibble(
  city       = c("New York", "Los Angeles", "Chicago", "Houston",
                 "Phoenix", "Philadelphia", "San Antonio", "San Diego",
                 "Dallas", "San Jose"),
  state      = c("NY", "CA", "IL", "TX", "AZ", "PA", "TX", "CA", "TX", "CA"),
  lat        = c(40.71, 34.05, 41.88, 29.76, 33.45,
                 39.95, 29.42, 32.72, 32.78, 37.34),
  lng        = c(-74.01, -118.24, -87.63, -95.37, -112.07,
                 -75.16, -98.49, -117.16, -96.80, -121.89),
  population = c(8336817, 3979576, 2693976, 2320268, 1680992,
                 1584064, 1547253, 1423851, 1304379, 1013240),
  median_inc = c(63998, 65290, 58247, 52338, 57459,
                 45927, 52455, 79868, 54747, 117477)
)

# ── Section 1: Chart + Table ──────────────────────────────────────────────────
# One SharedData object — both widgets share the same selection state.

shared_cities <- SharedData$new(cities)

chart_table <- bscols(
  widths = c(6, 6),

  # Bar chart: click a bar → highlights the matching row in the table
  plot_ly(shared_cities,
          x = ~city, y = ~median_inc, type = "bar",
          color = ~state,
          colors = c(AZ = "#861F41", CA = "#E5751F", IL = "#4A90D9",
                     NY = "#5BAD6F", PA = "#9B59B6", TX = "#2D2D2D")) |>
    layout(
      title  = "Median Income by City",
      xaxis  = list(title = ""),
      yaxis  = list(title = "Median Income ($)"),
      legend = list(title = list(text = "State"))
    ),

  # Table: click a row → highlights the matching bar in the chart
  datatable(shared_cities,
            rownames = FALSE,
            options  = list(pageLength = 5, dom = "tp"),
            colnames = c("City", "State", "Lat", "Lng",
                         "Population", "Median Income"))
)

# View in browser
browsable(chart_table)


# ── Section 2: Chart + Table + Map ────────────────────────────────────────────
# Three widgets sharing one SharedData object.

shared2 <- SharedData$new(cities)

pal_ct <- colorFactor(
  palette = c("#861F41", "#E5751F", "#2D2D2D", "#4A90D9", "#5BAD6F"),
  domain  = cities$state
)

three_panel <- bscols(
  widths = c(12, 7, 5),   # full-width filter row, then map + table

  # Optional: filter_select() for state-level filtering across all widgets
  filter_select("state_filter", "Filter by State:", shared2, ~state),

  leaflet(shared2) |>
    addTiles() |>
    setView(lng = -98, lat = 38, zoom = 4) |>
    addCircleMarkers(
      lng         = ~lng,
      lat         = ~lat,
      color       = ~pal_ct(state),
      fillOpacity = 0.85,
      radius      = 10,
      popup       = ~paste0(
        "<b>", city, "</b><br>",
        "State: ", state, "<br>",
        "Population: ", scales::comma(population), "<br>",
        "Median Income: $", scales::comma(median_inc)
      )
    ),

  datatable(shared2,
            rownames = FALSE,
            options  = list(pageLength = 5, dom = "tp"),
            colnames = c("City", "State", "Lat", "Lng",
                         "Population", "Median Income"))
)

browsable(three_panel)


# ── Section 3: Adapt to your project data ─────────────────────────────────────
# Replace `cities` with any small-to-medium data frame.
# If your data has lat/lng, you can add a leaflet map too.

# my_shared <- SharedData$new(your_project_data)
#
# bscols(
#   plot_ly(my_shared, x = ~your_x_var, y = ~your_y_var, type = "scatter",
#           mode = "markers"),
#   datatable(my_shared, rownames = FALSE,
#             options = list(pageLength = 10))
# ) |> browsable()
