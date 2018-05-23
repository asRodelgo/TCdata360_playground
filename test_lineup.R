############################
# Testing the lineup package plus crosstalk
#
# asanchezrodelgo@worldbank.org
############################

# library(devtools)
# library(httr)
# set_config(config(ssl_verifypeer = 0L))
# devtools::install_github("rstudio/crosstalk")
# devtools::install_github("sgratzl/lineup_htmlwidget")
# devtools::install_github("jcheng5/d3scatter")

library(shiny)
library(crosstalk)
library(lineup)
library(d3scatter)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("LineUp Shiny Example"),
  
  fluidRow(
    column(5, d3scatterOutput("scatter1")),
    column(7, lineupOutput("lineup1"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #shared_iris <- SharedData$new(iris)
  shared_iris <- SharedData$new(mtcars)
  
  output$scatter1 <- renderD3scatter({
    #d3scatter(shared_iris, ~Petal.Length, ~Petal.Width, ~Species, width = "100%")
    d3scatter(shared_iris, ~mpg, ~hp, ~cyl, width = "100%")
  })
  
  output$lineup1 <- renderLineup({
    lineup(shared_iris, width = "100%")
  })
}

# Run the application
shinyApp(ui = ui, server = server)