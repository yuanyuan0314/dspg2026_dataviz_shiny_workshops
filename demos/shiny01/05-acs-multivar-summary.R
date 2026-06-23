library(shiny)
library(tidyverse)

# Demo: selectInput (multiple) -> pick variables -> summary statistics table.
# Uses state.x77 (built-in R census data -- no download needed).

state_df <- as.data.frame(state.x77) |>
  tibble::rownames_to_column("state") |>
  rename(
    income      = Income,
    illiteracy  = Illiteracy,
    life_exp    = `Life Exp`,
    murder_rate = Murder,
    hs_grad     = `HS Grad`,
    frost_days  = Frost,
    population  = Population,
    area        = Area
  )

numeric_vars <- c("income", "illiteracy", "life_exp",
                  "murder_rate", "hs_grad", "frost_days",
                  "population", "area")

ui <- fluidPage(
  titlePanel("Variable Summary Statistics"),
  sidebarLayout(
    sidebarPanel(
      selectInput("var_pick", "Variables (select any):",
                  choices  = numeric_vars,
                  selected = c("income", "life_exp", "hs_grad"),
                  multiple = TRUE)
    ),
    mainPanel(
      tableOutput("summary_table")
    )
  )
)

server <- function(input, output, session) {
  output$summary_table <- renderTable({
    req(input$var_pick)
    state_df |>
      select(all_of(input$var_pick)) |>
      pivot_longer(everything(), names_to = "Variable") |>
      group_by(Variable) |>
      summarise(
        N      = sum(!is.na(value)),
        Mean   = round(mean(value, na.rm = TRUE), 2),
        SD     = round(sd(value,   na.rm = TRUE), 2),
        Min    = round(min(value,  na.rm = TRUE), 2),
        Median = round(median(value, na.rm = TRUE), 2),
        Max    = round(max(value,  na.rm = TRUE), 2)
      )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
}

shinyApp(ui, server)
