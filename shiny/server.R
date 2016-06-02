
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
      predOS <- reactive({input$predOS})
      predYear <- reactive({sprintf("%04d",as.numeric(input$predYear))})
      predMonth <- reactive({sprintf("%02d",as.numeric(input$predMonth))})
      
      output$pageTitle <- renderText({
            if (appFunc() == 1) {
                  if (topTen()) {
                        "<h3>Top-ten the most popular OS</h3>"
                  } else {
                        "<h3>The popularity level of the different OS</h3>"
                  }
                  
            } else if (appFunc() == 2) {
                  paste("<h3>Prediction of the popularity level of",predOS(),"</h3>")
                  
            } else if (appFunc() == 3) {
                  if (mostOS()) {
                        "<h3>The most popular OS in the different countries</h3>"   
                  } else {
                        paste("<h3>Map of the popularity level for",mapOS(),"</h3>")
                  }
            }
            
      })

      output$outTimeFrame <- renderText({
            if (appFunc() == 1) {
                  paste(minDate(),maxDate(),sep=" - ")
            } else if (appFunc() == 2) {
                  paste("Prediction to ",predYear(),"-",predMonth(),sep="")
                  
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
                  predValues <- getPredValues(predYear(),predMonth())
                  predUpr    <- getPredUpr(predYear(),predMonth())
                  predLwr    <- getPredLwr(predYear(),predMonth())
                  df <- cbind(predValues$Date,predValues[,predOS()],predUpr[,predOS()],predLwr[,predOS()])
                  df <- as.data.frame(df)
                  names(df) <- c("Date","Popularity","Upr","Lwr")
                  
                  df$Popularity <- as.numeric(as.character(df$Popularity))
                  df$Upr <- as.numeric(as.character(df$Upr))
                  df$Lwr <- as.numeric(as.character(df$Lwr))
                  
                  gvisLineChart(df, xvar = "Date", yvar = c("Popularity","Upr","Lwr"),
                                options = list(height = 600, width = 800,
                                               title = paste("Predictions for",predOS()),
                                               series="[{color:'black', targetAxisIndex: 0}, 
                                                        {color: 'blue',targetAxisIndex:0},
                                                        {color: 'red',targetAxisIndex:0}
                                                      ]"
                                               )
                                )
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
            if (showTab() & appFunc() == 1) {
                  osu <- prepareDataSet(minDate(),maxDate(),topTen())
                  gvisTable(osu, options = myOptions)
            }
      })

})
