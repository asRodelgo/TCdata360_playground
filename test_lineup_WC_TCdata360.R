############################
# shiny app to show exploratory analysis (lineup package) of countries and indicators from TCdata360
#
# asanchezrodelgo@worldbank.org
############################

library(shiny)
library(crosstalk)
library(lineup)
library(d3scatter)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("LineUp WC TCdata360 Example"),
  
  fluidRow(
    #column(5, d3scatterOutput("scatter1")),
    column(12, lineupOutput("lineup1"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # my_data <- filter(wc_data_lineup,  grepl("Spain|Portugal",CountryName)) %>%
  #   gather(Indicator, Value, -CountryName, -Period) %>%
  #   filter(!is.na(Value)) %>%
  #   spread(CountryName, Value) 
  
  my_data <- wc_data_ranks %>%
  #my_data <- filter(wc_data_ranks,  grepl("Spain|Portugal",CountryName)) %>%
    filter(!is.na(Value)) %>%
    group_by(Indicator) %>%
    filter(Period == max(Period)) %>%
    ungroup() %>%
    distinct(CountryName, Indicator, .keep_all=TRUE) %>%
    select(-CountryCode, -Value, -Period) %>%
    spread(CountryName, rank, fill = 0) %>%
    as.data.frame()
  
  row.names(my_data) <- my_data$Indicator
  
  my_data <- select(my_data, -Indicator)
  
  #my_data <- select_at(wc_data_lineup,vars(CountryName,names(wc_data_lineup)[which(grepl("WEF ",names(wc_data_lineup)))]))
  my_data <- SharedData$new(my_data)
  
  output$scatter1 <- renderD3scatter({
    d3scatter(my_data, ~eval(names(my_data)[3]), ~eval(names(my_data)[4]), ~eval(names(my_data)[5]), width = "100%")
  })
  
  output$lineup1 <- renderLineup({
    lineup(my_data, width = "100%")
  })
}

# Run the application
shinyApp(ui = ui, server = server)