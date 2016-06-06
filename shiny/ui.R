
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
                checkboxInput('showTable',"Show also data within the table"),
                h5("Description:"),
                p("The Popularity function may be used to obtain the charts illustrating the popularity level of the operating systems. The user can choose the time frame for chart. Also the user can swith the view between all OS and the top-ten popular OS. The main type of data presentation is a line chart. There are a possibility to show or hide the source data represented as a table under the chart.")
                
          ),
          ## Conditional panel for the app-function == "Map"
          conditionalPanel(
                condition = "input.appFunc == 'Map'",
                selectInput('mapOS',"Operating Systems:",names(osmap)[-1],selected = NULL),
                checkboxInput('mostOS','Show the most popular OS for each country'),
                h5("Description:"),
                p("The Map function allows the user to see within the map view the popularity level of the different OS in different countries. The user can choose the operating system using the combobox. Also the user can obtain the map showing the most popular OS in different countries. When this type of view is choosen, there isn't any responce on the changing the combobox value.")
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
                selectInput('predOS',"Operating Systems:",names(osmap)[-1],selected = NULL),
                h5("Description:"),
                p("The Prediction function allows user to see the result of the forecasting for the selected operating system. The chart shows the predicted values for the monthes begining from the May 2016 and ending with the month defined by the user.  In addition to the predicted values the upper and lower levels of the confidence intervals are shown.")
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
