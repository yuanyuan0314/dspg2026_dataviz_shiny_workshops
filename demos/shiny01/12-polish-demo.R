library(shiny)
library(bslib)
library(tidyverse)
library(palmerpenguins)

# Demo: bslib page_sidebar with Bootstrap 5 theming.
# Swap bootswatch = "flatly" for "cosmo", "lux", "minty", etc. to change look.

ui <- page_sidebar(
  title = "Penguin Explorer",
  theme = bs_theme(
    bootswatch = "flatly",
    primary    = "#861F41",
    base_font  = font_google("Inter")
  ),
  sidebar = sidebar(
    sliderInput("mass_range", "Body Mass (g):",
                min = 2700, max = 6300,
                value = c(2700, 6300), step = 100),
    selectInput("species_select", "Species:",
                choices  = c("All", levels(penguins$species)),
                selected = "All"),
    selectInput("island_select", "Island:",
                choices  = c("All", levels(penguins$island)),
                selected = "All")
  ),
  layout_columns(
    card(
      card_header("Scatter: Bill Length vs Body Mass"),
      plotOutput("scatter")
    ),
    card(
      card_header("Count by Species"),
      plotOutput("bar")
    )
  )
)

server <- function(input, output, session) {

  filtered <- reactive({
    df <- penguins |> drop_na()
    if (input$species_select != "All") df <- df |> filter(species == input$species_select)
    if (input$island_select  != "All") df <- df |> filter(island  == input$island_select)
    df |> filter(body_mass_g >= input$mass_range[1],
                 body_mass_g <= input$mass_range[2])
  })

  output$scatter <- renderPlot({
    filtered() |>
      ggplot(aes(bill_length_mm, body_mass_g, color = species)) +
      geom_point(size = 2, alpha = 0.7) +
      scale_color_manual(values = c("#861F41", "#E5751F", "#2D2D2D")) +
      labs(x = "Bill Length (mm)", y = "Body Mass (g)") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  })

  output$bar <- renderPlot({
    filtered() |>
      count(species) |>
      ggplot(aes(species, n, fill = species)) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = c("#861F41", "#E5751F", "#2D2D2D")) +
      labs(x = NULL, y = "Count") +
      theme_minimal(base_size = 13)
  })
}

shinyApp(ui, server)
