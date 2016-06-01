
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
# library(devtools)
# install_github("osdata","alnever")
library(osdata)

library(shiny)
require(googleVis)
library(reshape2)

shinyServer(function(input, output) {

      
      minDate <- reactive({paste(input$minYear,'-',sprintf("%02d",as.numeric(input$minMonth)), sep = "")})
      maxDate <- reactive({paste(input$maxYear,'-',sprintf("%02d",as.numeric(input$maxMonth)), sep = "")})
      topTen  <- reactive({input$topTen})
      showTab <- reactive({input$showTable})
      appFunc <- reactive({
            switch(input$appFunc,
                   'Popularity' = 1,
                   'Prediction' = 2,
                   'Map' = 3
                   )
      })
      mapOS <- reactive({input$mapOS})
      mostOS <- reactive({input$mostOS})

      output$outTimeFrame <- renderText({
            if (appFunc() == 1) {
                  paste(minDate(),maxDate(),sep=" - ")
            } else if (appFunc() == 2) {
                  
            } else if (appFunc() == 3) {
                  '2016-02 - 2016-04'
            }
      })
      
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
            opt_list = list(height = 600, width = 800) 
            if (appFunc() == 1) {
                  osu <- prepareDataSet(minDate(),maxDate(),topTen())
                  gvisLineChart(data = osu, 
                                "Date", 
                                names(osu)[-1],
                                options = opt_list)
            } 
            else if (appFunc() == 2) {
                  
            }
            else if (appFunc() == 3) {
                  if (mostOS() == FALSE) {
                        gvisGeoChart(data = osmap, locationvar = "Continent", 
                                                 colorvar = mapOS(), 
                                                 hovervar = "",
                                                 options = opt_list)
                  } else {
                        oscountry <- as.character(osmap$Continent)
                        osm <- osmap[,-1]
                        maxim <- apply(osm,1, function(x) {t <-names(osm)[which(x == max(x))]; t[1]})
                        osmax <- as.data.frame(cbind(oscountry,maxim))
                        names(osmax) <- c("Country","Operating.System")
                        osmax$OS <- as.factor(osmax$Operating.System)
                        vAxesParams <- paste("[{title:'",levels(osmax$OS)[1],"'},{title:'",levels(osmax$OS)[length(levels(osmax$OS))],"'}]",sep="")
                        gvisGeoChart(data = osmax, locationvar = "Country", 
                                     colorvar = "OS", 
                                     hovervar = "Operating.System",
                                     options = list(height = 600, width = 800,
                                                    colors="['#ff0000','#00ff00']",
                                                    vAxes=vAxesParams,
                                                    backgroundColor="lightblue"
                                     ))

                  }
            }
      })
      
      output$popTable <- renderGvis({
            if (showTab()) {
                  osu <- prepareDataSet(minDate(),maxDate(),topTen())
                  gvisTable(osu, options = myOptions)
            }
      })

})
