# this script gets a forecast for a specific region by querying the aWhere API. 
# forecase data can be aggregated for n day forecast(s).

# install / load required packages ----------------------------------------

# install aWhere R packages
#devtools::install_github("aWhereAPI/aWhere-R-Library")
#devtools::install_github("aWhereAPI/aWhere-R-ChartLibrary")

library(dplyr)
library(ggmap)
library(tibble)
library(wicket)
library(ggplot2)
library(aWhereAPI)
library(aWhereCharts)

# load external functions 
source("supporting_functions.R")


# define input paths and variables ----------------------------------------

# working directory - where input files are located and outputs will be saved.
working.dir <- "~/Documents/aWhere/"

# filename containing your aWhere credientials (key and secret)
# to access data from the aWhere API.
credentials.file <- "credentials.txt"

# length of forecast(s) in a vector. For a 7-day and 3-day forecast, 
# n.day.forecasts <- c(7, 3)
n.day.forecasts <- c(3)

# starting and ending years and days for the forecast. each is a vector with 
# the starting value in position 1 and the ending value in position 2. 
years <- c(2010, 2017)

# start day can "today", "yesterday", "tomorrow", or "YYYY-MM-DD". 
# end day is calculated using the largest value in n.day.forecasts vector.
# to specify a different end day, add the "ending.day" argument
# to the GetDays function with a specific end date, "YYYY-MM-DD". 
day.start <- "yesterday"
days <- GetDays(starting.day = day.start, 
                forecast.days = n.day.forecasts)

# template file containing geographic data across a region.
template.file <- "CLIMARKonlyWardTemplate.csv"

# to select subarea(s) of interest, list their names in this vector.
# for now, these subareas are limited to ward names. To generate a forecast
# for the entire region instead, set subarea.select to ENTIRE_REGION. 
subarea.select <- "ENTIRE_REGION"
#subarea.select <- "KARARE" #c("KARARE", "GOLBO")

# base filename for outputs. currently incorporates the name(s) of the 
# subarea(s) of interest, but you can set it to be anything. 
filename.out <- paste("AOI_Forecast",
                      paste(subarea.select, collapse="_"),
                      sep = "_")

# base map location and zoom values for mapping the forecast data. 
map.lat <- 2.5
map.lon <- 38
map.zoom <- 7

# set thresholds for different variables during mapping
thresh.precip.max <- 300 # [mm]
thresh.precip.min <- 0 # [mm]

# processing steps --------------------------------------------------------

# set the working directory 
base::setwd(working.dir) 

# load the aWhere API credentials file 
aWhereAPI::load_credentials(credentials.file) 

# read the template data 
template.place <- utils::read.csv(template.file) 

# filter the template for subarea(s) of interest
if (!identical(subarea.select, "ENTIRE_REGION")){ 
  
  template.place <- template.place %>% 
    dplyr::filter(WARD_NAME %in% subarea.select) 
  
} 

# query the aWhere API to get forecast data and write to file 
forecast.all <- GetForecastData(template.place, days, years, 
                                write.file = TRUE, filename.out = filename.out)

# calculate the n-day forecast summaries,
# aggregate weather data over the specified number of days.
forecasts.n <- GetForecastSummary(forecast.all, n.day.forecasts, 
                                  template.place)



# map forecast summaries --------------------------------------------------

# create a data frame for the variable thresholds.
# access the variable by referencing the column (thresholds$precip)
# the minimum values is in position 1, maximum value in position 2
thresholds <- base::as.data.frame(c(thresh.precip.min, thresh.precip.max))
base::colnames(thresholds) <- "precip"
base::row.names(thresholds) <- (c("min","max"))

# create the base map using the parameters defined earlier
base.map = get_map(location = c(lon = map.lon, 
                                lat = map.lat), 
                   zoom = map.zoom, 
                   color = "bw")

# display map of region
gg.map <- ggmap(base.map)
gg.map

# map the forecast summaries one at a time 
forecast.maps <- MapForecast(forecasts.n, base.map, thresholds)

# to access one of the individual forecast maps: 
forecast.maps$`3-day`


