#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(reactable)

ui <- navbarPage("Highway MPG",

                 # ── Page 1: Distribution ──────────────────────────────────────────────────
                 tabPanel("Distribution",
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput("bins",
                                          "Number of bins:",
                                          min   = 1,
                                          max   = 50,
                                          value = 30),
                              radioButtons("facet_var",
                                           "Facet by:",
                                           choices  = c("Year"          = "year",
                                                        "Transmission"  = "trans",
                                                        "Vehicle Class" = "class"),
                                           selected = "year"),
                              width = 2
                            ),
                            mainPanel(
                              plotOutput("distPlot")
                            )
                          )
                 ),

                 # ── Page 2: Summary Stats ─────────────────────────────────────────────────
                 tabPanel("Summary Stats",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("facet_var_stats",
                                           "Group by:",
                                           choices  = c("Year"          = "year",
                                                        "Transmission"  = "trans",
                                                        "Vehicle Class" = "class"),
                                           selected = "year"),
                              width = 2
                            ),
                            mainPanel(
                              reactableOutput("statsTable")
                            )
                          )
                 )
)

server <- function(input, output) {

  output$distPlot <- renderPlot({
    ggplot(mpg, aes(hwy)) +
      geom_histogram(bins = input$bins, fill = "darkgray") +
      facet_wrap(vars(!!sym(input$facet_var))) +
      labs(
        x     = "Miles per Gallon (MPG)",
        title = "Distribution of Highway MPG"
      ) +
      theme_minimal()
  })

  output$statsTable <- renderReactable({
    mpg |>
      group_by(!!sym(input$facet_var_stats)) |>
      summarise(
        mean = round(mean(hwy, na.rm = TRUE), 1),
        sd   = round(sd(hwy,   na.rm = TRUE), 1),
        min  = min(hwy, na.rm = TRUE),
        max  = max(hwy, na.rm = TRUE)
      ) |>
      reactable()
  })
}

shinyApp(ui = ui, server = server)
