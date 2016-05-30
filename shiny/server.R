
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
require(osdata)
require(googleVis)


shinyServer(function(input, output) {

      
      minDate <- reactive({paste(input$minYear,'-',sprintf("%02d",as.numeric(input$minMonth)), sep = "")})
      maxDate <- reactive({paste(input$maxYear,'-',sprintf("%02d",as.numeric(input$maxMonth)), sep = "")})
      topTen  <- reactive({input$topTen})
      showTab <- reactive({input$showTable})

      output$outTimeFrame <- renderText(paste(minDate(),maxDate(),sep=" - "))
      
      prepareDataSet <- function(p_minDate, p_maxDate, p_topTen) {
            osu <- subset(osusers, as.character(osusers$Date) >= p_minDate & as.character(osusers$Date) <= p_maxDate)  
            if (p_topTen) {
                  last_data <- osu[nrow(osu),]
                  m <- melt(last_data[-1])
                  m <- m[order(-m$value),]
                  m <- m[1:10,]
                  mnames <- as.character(m$variable)
                  osu <- osu[,c("Date",mnames)]
            }
            return(osu)
      }
      
      myOptions <- list(
                   page = 'enable',
                   pageSize = 20,
                   width = 800
             )

      output$popLineChart <- renderGvis({
            osu <- prepareDataSet(minDate(),maxDate(),topTen())
            gvisLineChart(data = osu, 
                          "Date", 
                          names(osu)[-1],
                          options = list(height = 600, width = 800))
      })
      
      output$popTable <- renderGvis({
            if (showTab()) {
                  osu <- prepareDataSet(minDate(),maxDate(),topTen())
                  gvisTable(osu, options = myOptions)
            }
      })

})
