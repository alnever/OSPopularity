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