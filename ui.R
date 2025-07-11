#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("What is our perception of stock status?"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          h4(strong(em("Choose a stock"))),

          h4(strong(em("Choose a year to compare all values"))),
          fluidRow(column(width = 6, numericInput("Year_comp", "Year for comparison", value = 2000, min = 0, max = 2030, step = 1)))
          
        ),

        # Show a plot of the generated distribution
        mainPanel(
          h4(strong("Time series of population outputs relative to a chosen year")),
          h4("Horizontal and vertical lines intersect at the chosen year (i.e., a relative value of 1)"),
          h4("Hover the pointer over any series and point to get the specific values"),
          plotlyOutput("CompPlot")
#          plotlyOutput("DepPlot"),
#          plotlyOutput("SpawnOutPlot"),
#         plotlyOutput("SummaryBPlot"),
#          plotlyOutput("TotalBPlot")
        )
    )
)
