G <- gvisGeoChart(data = osm, locationvar = "Continent", 
           colorvar = names(osm)[-1], 
           hovervar = "",
           options = opt_list)
plot(G)

Geo=gvisGeoChart(Exports, locationvar="Country", 
                 colorvar="Profit",
                 options=list(projection="kavrayskiy-vii"))
plot(Geo)

Geo=gvisGeoChart(e, locationvar="Country", 
                 colorvar="Profit",
                 options=list(projection="kavrayskiy-vii"))
plot(Geo)


oscountry <- as.character(osmap$Continent)
maxim <- apply(osm,1, function(x) {t <-names(osm)[which(x == max(x))]; t[1]})
osmax <- as.data.frame(cbind(oscountry,maxim))
osmax$OS <- as.factor(osmax$Operating.System)
names(osmax) <- c("Country","Operating.System","OS")
G <- gvisGeoChart(data = osmax, locationvar = "Country", 
             colorvar = "OS", 
             hovervar = "Operating.System",
             options = opt_list)
plot(G)

vAxesParams <- paste("[{title:'",levels(osmax$OS)[1],"'},{title:'",levels(osmax$OS)[length(levels(osmax$OS))],"'}]",sep="")

G <- gvisGeoChart(data = osmax, locationvar = "Country", 
                  colorvar = "OS", 
                  hovervar = "Operating.System",
                  options = list(height = 600, width = 800,
                                 colors="['#ff0000','#00ff00']",
                                 vAxes=vAxesParams,
                                 backgroundColor="lightblue"
                  ))
plot(G)



predYear <- "2017"
predMonth <- "12"
predOS <- "Android"
predValues <- getPredValues(predYear,predMonth)
predUpr    <- getPredUpr(predYear,predMonth)
predLwr    <- getPredLwr(predYear,predMonth)
df <- cbind(predValues$Date,predValues[,predOS],predUpr[,predOS],predLwr[,predOS])
df <- as.data.frame(df)
names(df) <- c("Date","Popularity","Upr","Lwr")
df$Popularity <- as.numeric(as.character(df$Popularity))
df$Upr <- as.numeric(as.character(df$Upr))
df$Lwr <- as.numeric(as.character(df$Lwr))
G <- gvisLineChart(df, xvar = "Date", yvar = c("Popularity","Upr","Lwr"),
              options = list(height = 600, width = 800,
                             series="[{color:'black', targetAxisIndex: 0}, 
                             {color: 'blue',targetAxisIndex:0},
                             {color: 'red',targetAxisIndex:0}
                             ]"
              )
              )
plot(G)

