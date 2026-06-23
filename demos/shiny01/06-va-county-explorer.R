library(shiny)
library(tidyverse)
library(tidycensus)
library(leaflet)
library(sf)
library(scales)

# Demo: Virginia County Explorer
# selectInput (county + variable) -> text headline + leaflet choropleth + summary table
# Demonstrates reactive() shared across three different output types.
#
# Requires a Census API key. Register free at https://api.census.gov/data/key_signup.html
# then run: tidycensus::census_api_key("YOUR_KEY", install = TRUE)
#

vars <- c(
  "Median Income ($)"     = "B19013_001",
  "Median Age (years)"    = "B01002_001",
  "Median Home Value ($)" = "B25077_001",
  "Population"            = "B01003_001"
)

# Pull all four variables for every Virginia county once at startup.
# geometry = TRUE returns an sf object ready for leaflet.
va <- get_acs(
  geography = "county",
  state     = "VA",
  variables = vars,
  year      = 2022,
  geometry  = TRUE
) |>
  st_transform(crs = 4326) |>
  mutate(county = str_remove(NAME, ", Virginia"))

ui <- fluidPage(
  titlePanel("Virginia County Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("county", "County:",
                  choices  = sort(unique(va$county)),
                  selected = "Fairfax County"),
      selectInput("var", "Variable:",
                  choices = names(vars))
    ),
    mainPanel(
      textOutput("headline"),
      tags$br(),
      leafletOutput("map", height = 380),
      tags$br(),
      tableOutput("tbl")
    )
  )
)

server <- function(input, output, session) {

  # reactive(): filter to the selected variable once --
  # all three outputs share this without re-running the filter.
  # tidycensus stores the NAME of the variable (not the ACS code) in the
  # variable column when a named vector is passed to get_acs(), so filter
  # by input$var directly rather than vars[input$var]
  var_data <- reactive({
    va |> filter(variable == input$var)
  })

  output$headline <- renderText({
    d         <- var_data()
    row       <- d |> filter(county == input$county)
    state_med <- median(d$estimate, na.rm = TRUE)
    direction <- if (row$estimate >= state_med) "above" else "below"

    paste0(
      input$county, " has a ", tolower(input$var), " of ",
      comma(round(row$estimate)),
      ", which is ", direction,
      " the Virginia county median of ",
      comma(round(state_med)), "."
    )
  })

  output$map <- renderLeaflet({
    d   <- var_data()
    pal <- colorNumeric("YlOrRd", d$estimate, na.color = "grey80")

    leaflet(d) |>
      addTiles() |>
      addPolygons(
        fillColor   = ~pal(estimate),
        fillOpacity = 0.7,
        color       = "white",
        weight      = 1,
        popup       = ~paste0("<b>", county, "</b><br>",
                              input$var, ": ", comma(round(estimate)))
      ) |>
      # Highlight the selected county with a maroon border
      addPolygons(
        data        = filter(d, county == input$county),
        fill        = FALSE,
        color       = "#861F41",
        weight      = 3
      ) |>
      addLegend(
        pal      = pal,
        values   = ~estimate,
        title    = input$var,
        position = "bottomright"
      )
  })

  output$tbl <- renderTable({
    d <- var_data()
    tibble(
      Metric = c(input$county, "VA Median", "VA Min", "VA Max"),
      Value  = comma(round(c(
        filter(d, county == input$county)$estimate,
        median(d$estimate, na.rm = TRUE),
        min(d$estimate,    na.rm = TRUE),
        max(d$estimate,    na.rm = TRUE)
      )))
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
}

shinyApp(ui, server)
