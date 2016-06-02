
# This is the server logic for a Shiny web application.
# 
# http://shiny.rstudio.com
#
# library(devtools)
# install_github("osdata","alnever")

# Loading own package, which contains the data and prediction functions
library(osdata)

# Loading the necessary libraries

library(shiny)
require(googleVis)
library(reshape2)

# Shiny server function

shinyServer(function(input, output) {

      # Get reactive variables
      ## Minimal date for popularity plot
      minDate <- reactive({paste(input$minYear,'-',sprintf("%02d",as.numeric(input$minMonth)), sep = "")})
      ## Maximal date for popularity plot
      maxDate <- reactive({paste(input$maxYear,'-',sprintf("%02d",as.numeric(input$maxMonth)), sep = "")})
      ## Show top-ten OS or not (if == TRUE, then we'll show only top-ten OS)
      topTen  <- reactive({input$topTen})
      ## Should we show the data table (if == TRUE, then show)
      showTab <- reactive({input$showTable})
      
      ## Which function of the app was selected
      appFunc <- reactive({
            switch(input$appFunc,
                   'Popularity' = 1, ## Show popularity data
                   'Prediction' = 2, ## Show prediction data
                   'Map' = 3         ## Show the popularity map
                   )
      })
      
      ## Which OS should be shown within the map
      mapOS <- reactive({input$mapOS})
      ## Show the OS popularity or the most popular OS for each country
      ## if == TRUE, then show the most popular OS for each country
      mostOS <- reactive({input$mostOS})
      
      ## Select OS for making prediction
      predOS <- reactive({input$predOS})
      ## The last year of the prediction preiod
      predYear <- reactive({sprintf("%04d",as.numeric(input$predYear))})
      ## The last month of the prediction preiod
      predMonth <- reactive({sprintf("%02d",as.numeric(input$predMonth))})
      
      ## Rendering the page title according the application function
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

      ## Rendering the text with the time period data within according the app function
      output$outTimeFrame <- renderText({
            if (appFunc() == 1) {
                  paste(minDate(),maxDate(),sep=" - ")
            } else if (appFunc() == 2) {
                  paste("Prediction to ",predYear(),"-",predMonth(),sep="")
                  
            } else if (appFunc() == 3) {
                  '2016-02 - 2016-04'
            }
      })

      ## Supporting function
      ## It prepares dataset for the plotting of the popularity chart
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
      
      ## Creating common option's list for the googleVis charts
      myOptions <- list(
                   page = 'enable',
                   pageSize = 20,
                   width = 800
             )

      ## Rendering of the googleVis chart according the app functions
      output$popLineChart <- renderGvis({
            opt_list = list(height = 600, width = 800)
            ## For the popularity line chart
            ### Prepare the dataset and show all OS or just top-ten of them
            if (appFunc() == 1) {
                  osu <- prepareDataSet(minDate(),maxDate(),topTen())
                  gvisLineChart(data = osu, 
                                "Date", 
                                names(osu)[-1],
                                options = opt_list)
            } 
            ## For the prediction page
            else if (appFunc() == 2) {
                  
                  ### Get prediction values according the given Year and Month
                  predValues <- getPredValues(predYear(),predMonth())
                  
                  ### Get the values of the confidence intervals
                  predUpr    <- getPredUpr(predYear(),predMonth())
                  predLwr    <- getPredLwr(predYear(),predMonth())
                  
                  ### Create the data.frame for googleVis gvisLineChart function
                  df <- cbind(predValues$Date,predValues[,predOS()],predUpr[,predOS()],predLwr[,predOS()])
                  df <- as.data.frame(df)
                  names(df) <- c("Date","Popularity","Upr","Lwr")
                  
                  df$Popularity <- as.numeric(as.character(df$Popularity))
                  df$Upr <- as.numeric(as.character(df$Upr))
                  df$Lwr <- as.numeric(as.character(df$Lwr))
                  
                  ### Plot the line chart
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
            ## For the map page
            else if (appFunc() == 3) {
                  ## If we shouldn't show the most pop OS for each country
                  if (mostOS() == FALSE) {
                        ## so we plot the map of the popularity for the given OS
                        gvisGeoChart(data = osmap, locationvar = "Continent", 
                                                 colorvar = mapOS(), 
                                                 hovervar = "",
                                                 options = opt_list)
                  } else {
                        ## Plot the map indicating the most pop OS for each country
                        oscountry <- as.character(osmap$Continent)
                        
                        ## Prepare new dataset
                        osm <- osmap[,-1]
                        
                        ## Select the OS having the highest rating in each of given countries
                        maxim <- apply(osm,1, function(x) {t <-names(osm)[which(x == max(x))]; t[1]})
                        
                        ## Create the temporary dataset for gvisGeoChart
                        osmax <- as.data.frame(cbind(oscountry,maxim))
                        names(osmax) <- c("Country","Operating.System")
                        osmax$OS <- as.factor(osmax$Operating.System)
                        
                        ## Defining the additional parameters for the map
                        vAxesParams <- paste("[{title:'",levels(osmax$OS)[1],"'},
                                             {title:'",levels(osmax$OS)[length(levels(osmax$OS))],"'}]",sep="")
                        
                        ## Plot the map
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
      
      ## Output the popularity table for 1st page of the application
      ## if the checkbox "Show data table" is switched on
      output$popTable <- renderGvis({
            if (showTab() & appFunc() == 1) {
                  osu <- prepareDataSet(minDate(),maxDate(),topTen())
                  gvisTable(osu, options = myOptions)
            }
      })

})
