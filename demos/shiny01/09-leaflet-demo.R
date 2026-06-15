library(tidyverse)
library(tidycensus)
library(leaflet)
library(sf)
library(scales)

# Demo: leaflet choropleth map using tidycensus county-level ACS data.
#
# HOW TO RUN: Select the lines for each section and press Ctrl+Enter.
# Do NOT use Source / Run All â€” each widget must be printed one at a time.
#
# Requires a Census API key. Register free at https://api.census.gov/data/key_signup.html
# then run: tidycensus::census_api_key("YOUR_KEY", install = TRUE)
#

# â”€â”€ Section 1: Virginia county median income choropleth â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
va_income <- get_acs(
  geography = "county",
  state     = "VA",
  variables = "B19013_001",   # median household income
  year      = 2022,
  geometry  = TRUE
) |>
  st_transform(crs = 4326) |>
  mutate(county = str_remove(NAME, ", Virginia"))

pal_income <- colorNumeric(
  palette = c("#FFFFFF", "#861F41"),
  domain  = va_income$estimate
)

print(
  leaflet(va_income) |>
    addTiles() |>
    addPolygons(
      fillColor   = ~pal_income(estimate),
      fillOpacity = 0.75,
      color       = "white",
      weight      = 1,
      popup       = ~paste0(
        "<b>", county, "</b><br>",
        "Median Income: ", dollar(estimate, accuracy = 1)
      )
    ) |>
    addLegend(
      pal      = pal_income,
      values   = ~estimate,
      title    = "Median Income ($)",
      labFormat = labelFormat(prefix = "$", big.mark = ","),
      position = "bottomright"
    )
)


# â”€â”€ Section 2: Swap variable â€” poverty rate â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
va_poverty <- get_acs(
  geography = "county",
  state     = "VA",
  variables = c(poor = "B17001_002", total = "B17001_001"),
  year      = 2022,
  geometry  = TRUE,
  output    = "wide"
) |>
  st_transform(crs = 4326) |>
  mutate(
    poverty_rate = poorE / totalE * 100,
    county       = str_remove(NAME, ", Virginia")
  )

pal_poverty <- colorNumeric(
  palette = "YlOrRd",
  domain  = va_poverty$poverty_rate
)

print(
  leaflet(va_poverty) |>
    addTiles() |>
    addPolygons(
      fillColor   = ~pal_poverty(poverty_rate),
      fillOpacity = 0.75,
      color       = "white",
      weight      = 1,
      popup       = ~paste0(
        "<b>", county, "</b><br>",
        "Poverty Rate: ", round(poverty_rate, 1), "%"
      )
    ) |>
    addLegend(
      pal      = pal_poverty,
      values   = ~poverty_rate,
      title    = "Poverty Rate (%)",
      labFormat = labelFormat(suffix = "%"),
      position = "bottomright"
    )
)
