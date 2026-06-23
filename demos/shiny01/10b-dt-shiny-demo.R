# Demo: DT inside a Shiny App
# DSPG 2026 | Virginia Tech
#
# Shows how to render a DT table reactively in Shiny:
# - renderDT() / DTOutput() pair (replaces renderTable/tableOutput)
# - Filters update the table instantly
# - Click a row to see details in a panel below
#
# HOW TO RUN: Open this file in RStudio and click "Run App"

library(shiny)
library(DT)
library(tidyverse)
library(nycflights13)

# ── Data prep ─────────────────────────────────────────────────────────────────
flights_tbl <- flights |>
  select(month, carrier, origin, dest, dep_delay, arr_delay, air_time) |>
  mutate(month = month.abb[month]) |>
  rename(
    Month      = month,
    Carrier    = carrier,
    Origin     = origin,
    Dest       = dest,
    `Dep Delay` = dep_delay,
    `Arr Delay` = arr_delay,
    `Air Time`  = air_time
  )

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  titlePanel("NYC Flights Explorer"),

  sidebarLayout(
    sidebarPanel(
      selectInput("carrier", "Carrier:",
                  choices  = c("All", sort(unique(flights_tbl$Carrier))),
                  selected = "All"),

      selectInput("origin", "Origin airport:",
                  choices  = c("All", sort(unique(flights_tbl$Origin))),
                  selected = "All"),

      sliderInput("max_delay", "Max departure delay (min):",
                  min = -30, max = 300, value = 60, step = 10),

      hr(),
      helpText("Click any row to see details below.")
    ),

    mainPanel(
      DTOutput("flights_table"),
      br(),
      # Row-click detail panel
      conditionalPanel(
        condition = "input.flights_table_rows_selected.length > 0",
        wellPanel(
          h4("Selected flight"),
          tableOutput("row_detail")
        )
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # Reactive filtered data
  filtered <- reactive({
    df <- flights_tbl

    if (input$carrier != "All")
      df <- df |> filter(Carrier == input$carrier)

    if (input$origin != "All")
      df <- df |> filter(Origin == input$origin)

    df |> filter(is.na(`Dep Delay`) | `Dep Delay` <= input$max_delay)
  })

  # Render the DT table
  output$flights_table <- renderDT({
    datatable(
      filtered(),
      selection  = "single",       # allow clicking one row
      filter     = "top",
      rownames   = FALSE,
      extensions = "Buttons",
      options    = list(
        pageLength = 10,
        scrollX    = TRUE,
        dom        = "Bfrtip",
        buttons    = c("csv", "excel")
      )
    ) |>
      formatStyle(
        columns    = "Dep Delay",
        background = styleInterval(
          cuts   = c(0, 30),
          values = c("#d4edda", "#fff3cd", "#f8d7da")
        )
      )
  })

  # Show clicked row as a detail table
  output$row_detail <- renderTable({
    req(input$flights_table_rows_selected)
    filtered()[input$flights_table_rows_selected, , drop = FALSE] |>
      pivot_longer(everything(), names_to = "Field", values_to = "Value")
  })
}

shinyApp(ui, server)
