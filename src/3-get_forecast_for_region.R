# this script gets a forecast for a specific region by querying the aWhere API. 
# forecase data can be aggregated for n day forecast(s).

# install / load required packages ----------------------------------------

#library(devtools)
#devtools::install_github("aWhereAPI/aWhere-R-Library")
#devtools::install_github("aWhereAPI/aWhere-R-Charts")

library(dplyr)
library(ggmap)
library(tibble)
library(wicket)
library(ggplot2)
library(raster)
library(aWhereAPI)
library(aWhereCharts)


# define input paths and variables ----------------------------------------

# working directory - where input files are located and outputs will be saved.
working.dir <- c('c:/aWhere/projects/climark')

# set the working directory 
setwd(working.dir) 

dir.create(path = 'figures/',showWarnings = FALSE, recursive = TRUE)
dir.create(path = 'shapefiles', showWarnings = FALSE, recursive = TRUE)


# load external functions 
source('src/0-supporting_functions.R')

# filename containing your aWhere credientials (key and secret)
credentials.file <- 'c:/aWhere/credentials/credentials.txt'

# load the aWhere API credentials file 
aWhereAPI::load_credentials(credentials.file)

# The forecast is limited to a maximum number of 7 days from today. 
# starting day can be "today" or "tomorrow"
days <- GetDays(starting.day = "yesterday",
                forecast.days = 7)


# "years"" is a vector with the starting year in position 1 and the ending
# year in position 2, for the long-term analysis. 
years <- c(2010, 2017)


# length of forecast(s) in a vector. For a 7-day and 3-day forecast, 
# n.day.forecasts <- c(7, 3)
n.day.forecasts <- c(7, 3)

# template file containing geographic data across a region.
template.file <- "templateFiles/CLIMARKonlyWardTemplate.csv"

# read the template data 
template.place <- utils::read.csv(template.file,
                                  stringsAsFactors=FALSE) 

print("Wards in data set: ")

print(unique(template.place$WARD_NAME))

# to select subarea(s) of interest, list their names in this vector.
# for now, these subareas are limited to ward names. 
subarea.select <- c("LOIYANGALANI", "GOLBO")
subarea.select <- "KARARE" 

# filter the template for subarea(s) of interest
if (!identical(subarea.select, "ENTIRE_REGION")){ 
  
  template.place <- template.place %>% 
    dplyr::filter(WARD_NAME %in% subarea.select) 
  
} 

# calculate how many grids are located within the selected subarea(s). 
print(paste0("Your forecast will pull ", as.character(nrow(template.place)), 
             " grids from the aWhere API"))


# base filename for outputs. currently incorporates the name(s) of the 
# subarea(s) of interest, but you can set it to be anything. 
filename.out <- paste("outputCSVs/AOI_Forecast",
                      paste(subarea.select, collapse="_"),
                      sep = "_")

# specific lat and lon
map.lat <- 2.5
map.lon <- 38
map.zoom <- 7

# create the base map 
base.map = ggmap::get_map(location = c(lon = map.lon,  lat = map.lat), 
                          zoom = map.zoom, 
                          color = "bw")

# display map of region
gg.map <- ggmap(base.map)
gg.map

# OR use location = country name Create directory shapefiles will be stored in. 


#Query list of all available countries
country.codes <- raster::getData('ISO3')

#We are not going to use this function because it is dependent on Google's API
#which we have no control over and there are query limits.  Instead we will use
#the raster package to query for a shapefile of the country and then will
#extract the bounding box to use for the call

#base.map <- ggmap::get_map(location = "Kenya", zoom = 6, color = "bw",source = 'google')

#Specify what county the boundary is requested for.  By altering the query below
#it is possible to only request the region for a subset of the country or for
#multiple countries
#
# The Download = TRUE option will make it so that if the user runs the script multiple times the file
# will be loaded from local files after the first run
countryOfInterest.name <- 'Kenya'

countryOfInterest.raster <- raster::getData('GADM'
                                            ,country=country.codes[country.codes$NAME == countryOfInterest.name,'ISO3']
                                            ,level=0
                                            ,download = TRUE
                                            ,path = 'shapefiles/')

countryOfInterest.extent <- raster::extent(countryOfInterest.raster)

# create the base map 

sbbox <- ggmap::make_bbox(lon = c(countryOfInterest.extent[1]
                                  ,countryOfInterest.extent[2])
                          ,lat = c(countryOfInterest.extent[3]
                                   ,countryOfInterest.extent[4]))

base.map <- ggmap::get_map(location = sbbox, maptype = "toner", source = "stamen")

gg.map <- ggmap(base.map)
gg.map


# set thresholds for different variables during mapping
thresh.precip.max <- 300 # [mm]
thresh.precip.min <- 0 # [mm]


# processing steps --------------------------------------------------------

# query the aWhere API to get forecast data and write to file 
forecast.all <- GetForecastData(template.place, days, years, 
                                write.file = TRUE, filename.out = filename.out)

head(forecast.all %>% 
       dplyr::select(locationid, date, precipitation.amount, 
                     precipitation.average), n = 10)

# calculate the n-day forecast summaries,
# aggregate weather data over the specified number of days.
forecasts.n <- GetForecastSummary(forecast.all, n.day.forecasts, 
                                  template.place)

# take a look at the forecast summaries
head(forecasts.n %>%
       dplyr::select(locationid, n.day, precip.amount.sum, 
                     precip.avg.sum))



# map forecast summaries --------------------------------------------------

# create a data frame for the variable thresholds.
# access the variable by referencing the column (thresholds$precip)
# the minimum values is in position 1, maximum value in position 2
thresholds <- as.data.frame(c(thresh.precip.min, thresh.precip.max))
colnames(thresholds) <- "precip"
row.names(thresholds) <- (c("min","max"))

# map the 7-day forecast summary

# keep only the 7-day forecast summary values
# clip the precip values to be between 0 and 300mm
forecast.7day <- forecasts.n %>% 
  dplyr::filter(n.day == 7) %>% 
  dplyr::mutate(aPre = ClipValues(precip.amount.sum,
                                  max.thresh = thresholds$precip[2]))

# add geometry information for mapping with ggplot
polygon.df = tibble::as.tibble(wicket::wkt_coords(forecast.7day$shapewkt))
polygon.df$aPre <- forecast.7day$aPre[polygon.df$object] 

# add n-day forecast to output filename
map.title.7 <- paste(filename.out, "7-day_forecast", sep="_")

# create the precip map
precip.map.7 <- ggmap(base.map) +
  geom_polygon( aes( x = lng, y = lat, 
                     group = object, fill = aPre),
                data = polygon.df, alpha = 0.7) +
  scale_fill_gradient2(breaks = seq(0,300, by = 50), 
                       low = "red", mid = "green", high = "blue", 
                       midpoint = 150, limits = c(0,300),
                       name="Precipitation (mm)") +
  ggtitle(map.title.7)

precip.map.7

# save map to file 
ggsave(filename = paste0('figures/',map.title.7, ".png"), 
       precip.map.7, width = 6.02, height = 3.38, units = "in")




# map the 3-day forecasted precip summary

# keep only the 3-day forecast summary values
# clip the precip values to be between 0 and 300mm
forecast.3day <- forecasts.n %>% 
  dplyr::filter(n.day == 3) %>% 
  dplyr::mutate(aPre = ClipValues(precip.amount.sum,
                                  max.thresh = thresholds$precip[2]))

# add geometry information for mapping with ggplot
polygon.df = tibble::as.tibble(wicket::wkt_coords(forecast.3day$shapewkt))
polygon.df$aPre <- forecast.3day$aPre[polygon.df$object] 

# add n-day forecast to output filename
map.title.3 <- paste(filename.out, "3-day_forecast", sep="_")

# create the precip map
precip.map.3 <- ggmap(base.map) +
  geom_polygon( aes( x = lng, y = lat, 
                     group = object, fill = aPre),
                data = polygon.df, alpha = 0.7) +
  scale_fill_gradient2(breaks = seq(0,300, by = 50), 
                       low = "red", mid = "green", high = "blue", 
                       midpoint = 150, limits = c(0,300),
                       name="Precipitation (mm)") +
  ggtitle(map.title.3)

precip.map.3

# save map to file 
ggsave(filename = paste0('figures/',map.title.3, ".png"), 
       precip.map.3, width = 6.02, height = 3.38, units = "in")
