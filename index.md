Analyzing of the Popularity of the Operating Systems
========================================================
author: Aleksei Neverov
autosize: true

The data for this project were obtained from the following resource and under the following license:

[The Stat Counter Web-Site](http://gs.statcounter.com/) 

[License CC BY-SA 3.0](http://gs.statcounter.com">StatCounter</a> / <a rel="license" href="http://creativecommons.org/licenses/by-sa/3.0/)

[The Stat Counter About page](http://gs.statcounter.com/about?PHPSESSID=cjgsasrma4847lrunklejg5944)

Project description
========================================================

* The data for this project were obtained from [The Stat Counter Web-Site](http://gs.statcounter.com/)
* The data show the popularity of the different operating systems according the preferences of the Internet users
* The indexes are colleced according the following method [About the StatCounter](http://gs.statcounter.com/about?PHPSESSID=cjgsasrma4847lrunklejg5944)

**The project contains:**

* The **osdata** package containing the datasets and some function to make forecasting of the OS popularity levels in the future;
* The **shiny application** allowing to review the popularity level of the different operating systems in form of charts, tables and maps;
* The **shiny application** is allowing to forecast the popularity level of the different operating systems for the time period begining after April 2016. The end of the forecasting period should be indicated by the user.


Creating of the "osdata" package 
========================================================

The **osdata** package is deployed in the [GitHub Repository](https://github.com/alnever/osdata.git). It contains:

* the **osusers** dataset containing the monthly data of the operating system popularity indexis between the December 2008 and April 2016;
* the **osmap** dataset containing the popularity indexis of the operating systems for different country;
* a set of *prediction* functions allowing to obtain the forcasted indexis of the OS popularity based on the **osusers** dataset. 

The predictions contains following data: (1) the set monthes between the May 2016 and some month in the future, which should be indicated by the user; (2) the set of predicted values for each month within the defined time frame and for each operating system from the **osusers** dataset;

The are 2 main common functions within the package:

* **fitRegressionModels** returns the list of the regressions model for each operating system;
* **getPredictions** returns for each forecasted month the predicted value and the lower and upper values of the 95%-confidence interval

Prediction of the Popularity of the Operating Systems (in osdata package)
========================================================


```r
fitRegressionModels <- function()
{     # Prepare dataset for model fitting
      osu <- osusers; dates <- as.character(osu$Date)
      dates <- paste(dates,"01",sep="-"); dates <- as.Date(dates)

      ## become a list of regression models
      fits <- apply(osu[,-1], 2, function(x) {lm(x~dates)})

      return(fits)
}
```


```r
getPredictions <- function(a_Year, a_Month) {
      # Fit the set of regression models
      fits <- fitRegressionModels()
      # Get the monthes for the prediction
      predictDates <- getForecastingInterval(a_Year, a_Month)
      # Make a prediction
      predictions <- lapply(fits, 
            function(x) {predict(x, newdata = predictDates, interval = "confidence")})

      return(predictions)
}
```


Shiny-application Content and Functions
========================================================

The **shiny application** is accessible by this link [https://alnever.shinyapps.io/OS-Popularity/](https://alnever.shinyapps.io/OS-Popularity/). 

It allows the user to use three functions:

* The **Popularity** function may be used to obtain the charts illustrating the popularity level of the operating systems. 
* The **Map** function allows the user to see within the map view the popularity level of the different OS in different countries. * The **Prediction** function allows user to see the result of the forecasting for the selected operating system. 

The full description is included into the application itself.l


