library(shiny)
library(tidyverse)
library(palmerpenguins)

# Demo: Four common input functions in one navbarPage app.
# Each tab shows a different input widget and what it produces.

ui <- navbarPage("Input Functions Demo",

  # selectInput (single) â€” one choice at a time
  tabPanel("selectInput (single)",
    sidebarLayout(
      sidebarPanel(
        selectInput("species1", "Species:",
                    choices  = c("Adelie", "Chinstrap", "Gentoo"),
                    selected = "Adelie")
      ),
      mainPanel(plotOutput("plot1"))
    )
  ),

  # selectInput (multiple) â€” any number of choices
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

  # radioButtons â€” mutually exclusive, always one selected
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

  # checkboxGroupInput â€” any subset, can be empty
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
  )
)

server <- function(input, output, session) {

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
}

shinyApp(ui, server)
