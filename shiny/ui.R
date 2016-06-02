
# This is the user-interface definition of a Shiny web application.
# 
# http://shiny.rstudio.com
#
# library(devtools)
# install_github("osdata","alnever")
library(osdata)

library(shiny)
require(googleVis)

data(osusers)
year <- gsub("-[0-9][0-9]","",levels(osusers$Date))
year <- as.numeric(year)

month <- gsub("[0-9][0-9][0-9][0-9]-","",levels(osusers$Date))
month <- as.numeric(month)

dates <- as.data.frame(cbind(year, month))


shinyUI(fluidPage(

  # Application title
  titlePanel("The popularity of the operating systems worldwide"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
          ## Select input for selecting the Application function
          selectInput('appFunc','Function:',c('Popularity','Prediction','Map'), selected = "Popularity"),
          ## Conditional panel for the app-function == "Popularity"
          conditionalPanel(
                condition = "input.appFunc == 'Popularity'",
                h5("Start of the time frame:"),
                selectInput('minMonth','Month:',1:12, selected = dates[1,"month"]),
                sliderInput("minYear","Year:", 
                            min = min(dates$year), 
                            max = max(dates$year),
                            value = min(dates$year)
                ),
                h5("End of the time frame:"),
                selectInput('maxMonth','Month:',1:12, selected = dates[nrow(dates),"month"]),
                sliderInput("maxYear","Year:", 
                            min = min(dates$year), 
                            max = max(dates$year),
                            value = max(dates$year)
                ),
                checkboxInput('topTen',"Show only top-ten operating systems"),
                checkboxInput('showTable',"Show also data within the table")
          ),
          ## Conditional panel for the app-function == "Map"
          conditionalPanel(
                condition = "input.appFunc == 'Map'",
                selectInput('mapOS',"Operating Systems:",names(osmap)[-1],selected = NULL),
                checkboxInput('mostOS','Show the most popular OS for each country')
                ),          
          ## Conditional panel for the app-function == "Prediction"
          conditionalPanel(
                condition = "input.appFunc == 'Prediction'", 
                selectInput('predMonth','Month:',1:12, selected = 1),
                sliderInput("predYear","Year:", 
                            min = max(dates$year) + 1, 
                            max = max(dates$year) + 11,
                            value = max(dates$year) + 1
                ),
                selectInput('predOS',"Operating Systems:",names(osmap)[-1],selected = NULL)
          )
          
    ),

    # Show data generated within the server.io
    mainPanel(
          htmlOutput("pageTitle"),
          h4('Time frame: ',textOutput('outTimeFrame')),
          htmlOutput("popLineChart"),
          htmlOutput("popTable")
          
    )
  )
))
