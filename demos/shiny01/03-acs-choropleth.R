library(shiny)
library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)

options(tigris_use_cache = TRUE)

vars <- c(
  "Median Income ($)"     = "B19013_001",
  "Median Age (years)"    = "B01002_001",
  "Median Home Value ($)" = "B25077_001",
  "Poverty Rate (%)"      = "B17001_002"
)

# Pull ACS data
state_data <- get_acs(
  geography = "state",
  variables = vars,
  year      = 2022,
  survey    = "acs5"
)

# Download state boundaries — keep ALL 52 first, shift, then filter
# (shift_geometry needs AK + HI present to reposition them)
states_sf <- states(cb = TRUE, year = 2022, resolution = "20m") |>
  shift_geometry() |>                                          # reposition AK + HI first
  filter(!STUSPS %in% c("PR", "VI", "GU", "MP", "AS"))        # then drop territories

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  titlePanel("US State ACS Map"),
  sidebarLayout(
    sidebarPanel(
      selectInput("map_var", "Variable:",
                  choices  = names(vars),
                  selected = "Median Income ($)")
    ),
    mainPanel(
      plotOutput("choro", height = "500px")
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  var_data <- reactive({
    state_data |>
      filter(variable == input$map_var) |>
      select(GEOID, estimate)
  })

  map_data <- reactive({
    states_sf |>
      left_join(var_data(), by = "GEOID")
  })

  output$choro <- renderPlot({
    ggplot(map_data()) +
      geom_sf(aes(fill = estimate), color = "white", linewidth = 0.2) +
      scale_fill_viridis_c(
        option   = "C",
        name     = input$map_var,
        labels   = scales::comma,
        na.value = "grey80"
      ) +
      labs(title = input$map_var) +
      theme_void(base_size = 13) +
      theme(
        legend.position = "right",
        plot.title      = element_text(size = 16, face = "bold")
      )
  })
}

shinyApp(ui, server)
