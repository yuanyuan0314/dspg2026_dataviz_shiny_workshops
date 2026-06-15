library(shiny)

# Demo: Three common Shiny UI layouts in one navbarPage app.

ui <- navbarPage("Layout Demo",

  # Tab 1 â€” sidebarLayout: controls on the left, output on the right
  tabPanel("Sidebar Layout",
    sidebarLayout(
      sidebarPanel(
        sliderInput("bins", "Number of bins:", min = 5, max = 50, value = 20)
      ),
      mainPanel(
        plotOutput("hist")
      )
    )
  ),

  # Tab 2 â€” fluidRow + column: free 12-column grid
  tabPanel("Grid Layout",
    fluidRow(
      column(4, wellPanel(
        selectInput("var", "Variable:", choices = c("mpg", "hp", "wt"))
      )),
      column(8,
        plotOutput("scatter")
      )
    )
  ),

  # Tab 3 â€” fillPage: fills the entire browser viewport (good for maps)
  tabPanel("Full-Screen Layout",
    fillPage(padding = 0,
      plotOutput("full_plot", height = "100vh")
    )
  )
)

server <- function(input, output, session) {

  output$hist <- renderPlot({
    hist(mtcars$mpg, breaks = input$bins,
         col = "#861F41", border = "white",
         main = "MPG Distribution", xlab = "Miles per Gallon")
  })

  output$scatter <- renderPlot({
    plot(mtcars[[input$var]], mtcars$mpg,
         xlab = input$var, ylab = "mpg",
         col = "#E5751F", pch = 16, cex = 1.5)
  })

  output$full_plot <- renderPlot({
    par(mar = c(4, 4, 2, 1))
    plot(mtcars$wt, mtcars$mpg,
         col = "#861F41", pch = 16, cex = 1.5,
         xlab = "Weight (1000 lbs)", ylab = "MPG",
         main = "Weight vs Fuel Efficiency")
  })
}

shinyApp(ui, server)
