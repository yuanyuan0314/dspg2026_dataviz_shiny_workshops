# Demo: Common Shiny Input Functions
# DSPG 2026 | Virginia Tech
#
# Six input widgets, each in its own tab.
# Run the app and explore each tab to see how inputs control outputs.
#
# HOW TO RUN: Click "Run App" in RStudio.

library(shiny)
library(tidyverse)
library(palmerpenguins)
library(nycflights13)

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- navbarPage("Input Functions Demo",

  # 1. selectInput (single) — one choice at a time
  tabPanel("selectInput",
    sidebarLayout(
      sidebarPanel(
        selectInput("species1", "Species:",
                    choices  = c("Adelie", "Chinstrap", "Gentoo"),
                    selected = "Adelie")
      ),
      mainPanel(plotOutput("plot1"))
    )
  ),

  # 2. selectInput (multiple) — any number of choices
  tabPanel("selectInput (multiple)",
    sidebarLayout(
      sidebarPanel(
        selectInput("species2", "Species (pick any):",
                    choices  = c("Adelie", "Chinstrap", "Gentoo"),
                    multiple = TRUE,
                    selected = c("Adelie", "Gentoo"))
      ),
      mainPanel(plotOutput("plot2"))
    )
  ),

  # 3. radioButtons — mutually exclusive, always one selected
  tabPanel("radioButtons",
    sidebarLayout(
      sidebarPanel(
        radioButtons("island", "Island:",
                     choices  = c("Biscoe", "Dream", "Torgersen"),
                     selected = "Biscoe")
      ),
      mainPanel(plotOutput("plot3"))
    )
  ),

  # 4. checkboxGroupInput — any subset, can be empty
  tabPanel("checkboxGroupInput",
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput("vars", "Variables to summarize:",
                           choices  = c("bill_length_mm", "bill_depth_mm",
                                        "flipper_length_mm", "body_mass_g"),
                           selected = c("bill_length_mm", "body_mass_g"))
      ),
      mainPanel(tableOutput("table4"))
    )
  ),

  # 5. sliderInput — numeric range or single value
  tabPanel("sliderInput",
    sidebarLayout(
      sidebarPanel(
        sliderInput("delay_range", "Departure delay range (min):",
                    min   = -30,
                    max   = 120,
                    value = c(0, 60),   # two-handle range slider
                    step  = 5),
        sliderInput("n_bins", "Histogram bins:",
                    min   = 5,
                    max   = 50,
                    value = 20,         # single-handle slider
                    step  = 5)
      ),
      mainPanel(
        plotOutput("plot5"),
        br(),
        helpText("Two-handle slider: value is a length-2 vector input$delay_range[1] and input$delay_range[2].")
      )
    )
  ),

  # 6. dateRangeInput — filter by date window
  tabPanel("dateRangeInput",
    sidebarLayout(
      sidebarPanel(
        dateRangeInput("date_range", "Flight date range:",
                       start = "2013-01-01",
                       end   = "2013-03-31",
                       min   = "2013-01-01",
                       max   = "2013-12-31"),
        selectInput("origin2", "Origin airport:",
                    choices  = c("All", "EWR", "JFK", "LGA"),
                    selected = "All")
      ),
      mainPanel(
        plotOutput("plot6"),
        br(),
        helpText("input$date_range returns a Date vector of length 2: start and end.")
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # Tab 1: selectInput (single)
  output$plot1 <- renderPlot({
    penguins |>
      filter(species == input$species1) |>
      drop_na() |>
      ggplot(aes(bill_length_mm, body_mass_g)) +
      geom_point(color = "#861F41", size = 2, alpha = 0.7) +
      labs(title = paste("Species:", input$species1),
           x = "Bill Length (mm)", y = "Body Mass (g)") +
      theme_minimal(base_size = 13)
  })

  # Tab 2: selectInput (multiple)
  output$plot2 <- renderPlot({
    req(input$species2)
    penguins |>
      filter(species %in% input$species2) |>
      drop_na() |>
      ggplot(aes(bill_length_mm, body_mass_g, color = species)) +
      geom_point(size = 2, alpha = 0.7) +
      scale_color_manual(values = c("#861F41", "#E5751F", "#2D2D2D")) +
      labs(x = "Bill Length (mm)", y = "Body Mass (g)") +
      theme_minimal(base_size = 13)
  })

  # Tab 3: radioButtons
  output$plot3 <- renderPlot({
    penguins |>
      filter(island == input$island) |>
      drop_na() |>
      ggplot(aes(bill_length_mm, body_mass_g, color = species)) +
      geom_point(size = 2, alpha = 0.7) +
      scale_color_manual(values = c("#861F41", "#E5751F", "#2D2D2D")) +
      labs(title = paste("Island:", input$island),
           x = "Bill Length (mm)", y = "Body Mass (g)") +
      theme_minimal(base_size = 13)
  })

  # Tab 4: checkboxGroupInput
  output$table4 <- renderTable({
    req(input$vars)
    penguins |>
      select(all_of(input$vars)) |>
      drop_na() |>
      summarise(across(everything(),
                       list(Mean = mean, SD = sd, Min = min, Max = max))) |>
      pivot_longer(everything(),
                   names_to  = c("Variable", "Stat"),
                   names_sep = "_(?=[^_]+$)") |>
      pivot_wider(names_from = Stat, values_from = value) |>
      mutate(across(where(is.numeric), \(x) round(x, 2)))
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # Tab 5: sliderInput
  output$plot5 <- renderPlot({
    flights |>
      filter(
        !is.na(dep_delay),
        dep_delay >= input$delay_range[1],   # lower bound
        dep_delay <= input$delay_range[2]    # upper bound
      ) |>
      ggplot(aes(x = dep_delay)) +
      geom_histogram(bins = input$n_bins, fill = "#861F41", color = "white") +
      labs(
        title = paste0("Dep Delay: ", input$delay_range[1],
                       " to ", input$delay_range[2], " min"),
        x = "Departure Delay (min)", y = "Count"
      ) +
      theme_minimal(base_size = 13)
  })

  # Tab 6: dateRangeInput
  output$plot6 <- renderPlot({
    df <- flights |>
      mutate(date = as.Date(paste(year, month, day, sep = "-"))) |>
      filter(
        date >= input$date_range[1],   # start date
        date <= input$date_range[2]    # end date
      )

    if (input$origin2 != "All")
      df <- df |> filter(origin == input$origin2)

    df |>
      count(date) |>
      ggplot(aes(x = date, y = n)) +
      geom_line(color = "#861F41", linewidth = 0.8) +
      geom_smooth(color = "#E5751F", se = FALSE, linewidth = 0.6) +
      labs(
        title = "Daily Flight Count",
        x = "Date", y = "Number of Flights"
      ) +
      theme_minimal(base_size = 13)
  })
}

shinyApp(ui, server)
