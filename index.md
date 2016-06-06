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
* The indexes are colleced according the following method:

*"Stats are based on aggregate data collected by StatCounter on a sample exceeding 15 billion pageviews per month collected from across the StatCounter network of more than 3 million websites. Stats are updated and made available every 4 hours, however are subject to quality assurance testing and revision for 14 days from publication."* [About the StatCounter](http://gs.statcounter.com/about?PHPSESSID=cjgsasrma4847lrunklejg5944)

**The project contains:**

* The **osdata** package containing the datasets and some function to make forecasting of the OS popularity levels in the future;
* The **shiny application** allowing to review the popularity level of the different operating systems in form of charts, tables and maps;
* The **shiny application** is allowing to forecast the popularity level of the different operating systems for the time period begining after April 2016. The end of the forecasting period should be indicated by the user.


Creating of the "osdata" package 
========================================================

The **osdata** package is deployed in the [GitHub Repository](https://github.com/alnever/osdata.git). It contains:

* the **osusers** dataset containing the monthly data of the operating system popularity indexis among the Internet users. It includes the data collected between the December 2008 and April 2016;
* the **osmap** dataset containing the popularity indexis of the operating systems for different country. Each observation of this data set contains the country name and the indexis of 24 operating systems calculated for this country;
* a set of *prediction* functions allowing to obtain the forcasted indexis of the OS popularity based on the **osusers** dataset. 

The predictions contains following data:

* the set monthes between the May 2016 and some month in the future, which should be indicated by the user;
* the set of predicted values for each month within the defined time frame and for each operating system from the **osusers** dataset;

The are 2 main common functions within the package:

* **fitRegressionModels** returns the list of the regressions model for each operating system;
* **getPredictions** returns for each forecasted month the predicted value and the lower and upper values of the 95%-confidence interval


Shiny-application Content and Functions
========================================================

The **shiny application** is accessible by this link [https://alnever.shinyapps.io/OS-Popularity/](https://alnever.shinyapps.io/OS-Popularity/). 

It allows the user to use three functions:

* The **Popularity** function may be used to obtain the charts illustrating the popularity level of the operating systems. The user can choose the time frame for chart. Also the user can swith the view between all OS and the top-ten popular OS. The main type of data presentation is a *line chart*. There are a possibility to show or hide the source data represented as a table under the chart.
* The **Map** function allows the user to see within the map view the popularity level of the different OS in different countries. The user can choose the operating system using the combobox. Also the user can obtain the map showing the most popular OS in different countries. When this type of view is choosen, there isn't any responce on the changing the combobox value.
* The **Prediction** function allows user to see the result of the forecasting for the selected operating system. The chart shows the predicted values for the monthes begining from the May 2016 and ending with the month defined by the user.  In addition to the predicted values the upper and lower levels of the confidence intervals are shown.

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
