
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
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
          selectInput('appFunc','Function:',c('Popularity','Prediction'), selected = "Popularity"),
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
          conditionalPanel(
                condition = "input.appFunc == 'Prediction'", 
                selectInput('predMonth','Month:',1:12, selected = 1),
                sliderInput("predYear","Year:", 
                            min = max(dates$year), 
                            max = max(dates$year) + 10,
                            value = max(dates$year)
                )
          ),
          actionButton('actBtn',"Show results")
    ),

    # Show a plot of the generated distribution
    mainPanel(
          # plotOutput("distPlot")
          h3('The popylarity of the operating systems'),
          h4('Time frame: ',textOutput('outTimeFrame')),
          htmlOutput("popLineChart"),
          htmlOutput("popTable")
          
    )
  )
))
