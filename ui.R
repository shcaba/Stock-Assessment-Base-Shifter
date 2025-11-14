#

library(shiny)
library(r4ss)
library(ggplot2)
library(plotly)
library(shinyFiles)


# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("How does our perception of current stock status change depending on the year we compare it to?"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          h4(strong(em("Choose a stock Report file"))),
            shinyDirButton(
            id = "Report_dir",
            label = "Select folder",
            title = "Choose folder containing the SS3 Report file"
          ),
          br(),
          h5(strong(textOutput("ReportPath", inline = TRUE))),
          
          h4(strong(em("Choose a year to compare all values"))),
          fluidRow(column(width = 6, numericInput("Year_comp", "Year for comparison", value = 2000, min = 0, max = 2030, step = 1))),
          actionButton("run_baseline_comps", strong("Update Baseline Comparisons"),
                       width = "100%",
                       icon("circle-play"),
                       style = "font-size:120%;border:2px solid;color:#FFFFFF; background:#236192"
          ),
          
        ),

        # Show a plot of the generated distribution
        mainPanel(
          h4(strong("Time series of population outputs relative to a chosen year")),
          h4("Horizontal and vertical lines intersect at the chosen year"), 
          h4("Top panel is all years relative to the chosen years (i.e., Year/Chosen_Year); Bottom panel is the percent difference from the chosen year)"),
          h4("Hover the pointer over any series and point to get the specific values"),
          plotlyOutput("CompPlot"),
          plotlyOutput("CompPlotRE")
#          plotlyOutput("DepPlot"),
#          plotlyOutput("SpawnOutPlot"),
#         plotlyOutput("SummaryBPlot"),
#          plotlyOutput("TotalBPlot")
        )
    )
)
