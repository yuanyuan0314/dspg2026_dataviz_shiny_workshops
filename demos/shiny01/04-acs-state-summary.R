library(shiny)
library(tidyverse)

# Demo: selectInput (single) -> pick a state -> summary table of all variables.
# Uses state.x77 (built-in R census data -- no download needed).

state_df <- as.data.frame(state.x77) |>
  tibble::rownames_to_column("state") |>
  rename(
    `Per Capita Income ($)`    = Income,
    `Illiteracy Rate (%)`      = Illiteracy,
    `Life Expectancy (years)`  = `Life Exp`,
    `Murder Rate (per 100k)`   = Murder,
    `HS Graduation Rate (%)`   = `HS Grad`,
    `Mean Frost Days`          = Frost,
    `Population (thousands)`   = Population,
    `Land Area (sq mi)`        = Area
  )

ui <- fluidPage(
  titlePanel("State Profile"),
  sidebarLayout(
    sidebarPanel(
      selectInput("state_pick", "Select a state:",
                  choices  = sort(state_df$state),
                  selected = "Virginia")
    ),
    mainPanel(
      h4(textOutput("state_title")),
      tableOutput("state_table")
    )
  )
)

server <- function(input, output, session) {

  output$state_title <- renderText({
    paste("Summary statistics for", input$state_pick)
  })

  output$state_table <- renderTable({
    state_df |>
      filter(state == input$state_pick) |>
      select(-state) |>
      pivot_longer(everything(),
                   names_to  = "Variable",
                   values_to = "Value") |>
      mutate(Value = round(Value, 2))
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
}

shinyApp(ui, server)
