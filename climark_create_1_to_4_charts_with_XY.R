# this script generates a series of charts for a specific location

# install / load required packages ----------------------------------------

# install aWhere R packages
#devtools::install_github("aWhereAPI/aWhere-R-Library")
#devtools::install_github("aWhereAPI/aWhere-R-ChartLibrary")

# load required packages 
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(aWhereAPI)
library(zoo)
library(aWhereCharts)
library(curl)

# define input paths and variables ----------------------------------------

# working directory - where input files are located and outputs will be saved.
working.dir <- "~/Documents/aWhere/"
setwd(working.dir)

# load external R functions in local file
source("supporting_functions.R")

# filename containing your aWhere credientials (key and secret),
# a text file where line 1 is the Consumer Key,
# line 2 is the Consumer Secret, and line 3 is a blank line. 
credentials.file <- "credentials.txt"

# load the aWhere API credentials file 
aWhereAPI::load_credentials(paste0(working.dir, credentials.file)) 

# latitude, longitude, and name of location used for the chart names 
# and output file names
lat <- 3.2
lon <- 37.0
location.name <- paste("Shamba1") 

# starting and ending years and days for the time series. each is a vector with 
# the starting value in position 1 and the ending value in position 2. 
years <- c(2010, 2017)

#starting and ending days with format "YYYY-MM-DD"
day.start <- "2018-03-01"
day.end <- "2018-03-08" # specific date
day.end <- as.character(Sys.Date() + 7) # today plus n

# combine the days into a single vector 
days <- c(day.start, day.end)

# effective precip amount for consistency across plots
eP <- 30    

# size of rolling average window for consistency across plots
roll.avg <- 30

# processing steps ----------------------------------------------------------

# For charting, variable choices are: 
# precip, accprecip, maxTemp, minTemp, pet, accpet, ppet or rollingavgppet

# Check if time range is more than 365 days
duration <- as.Date(days[2]) - as.Date(days[1])

if(duration > 365) { 
  print ("Period too long")
}

# pull the datasets    
weather.df <- aWhereCharts::generateaWhereDataset(lat = lat, lon = lon, 
                                                  day_start = days[1], 
                                                  day_end = days[2], 
                                                  year_start = years[1], 
                                                  year_end = years[2])

# reorder the columns in the data frame
weather.df <- weather.df %>% 
  select(day, date, latitude, longitude, everything())

# look at the weather data.
# select the first five columns of the data frame using [,1:5]
# and show the first 10 rows using the "n" argument
utils::head(weather.df[,1:5], n = 10)

# write forecast to .csv file 
utils::write.csv(weather.df, 
                 file = paste(location.name, 
                              paste(days, collapse="_"),
                              paste(years, collapse="_"),
                              ".csv", sep="_"))

# generate charts ---------------------------------------------------------

# Within the aWhereCharts package, there are two functions used here to create
# time series charts. 
# (1) generateaWhereChart - makes line plots comparing the current and long-term 
# normals for each variable. 
# (2) generateaWhereStdDevChart - plots aWhere weather data with one standard 
# deviation's shading above and below the long-term normalline. This function 
# is meant to help users understand whether the current weather conditions are
# significantly outside the norm. 

# plots with standard deviation are named with "1" at the end of the variable,
# and those without standard deviation are named with "2". 

# Maximum temperature with standard deviation ---------------------------------

# contruct the title for this chart using the variable name, location name,
# and the latitude/longitude.
max.temp.1.title <- paste0(location.name, "Maximum Temp w StdDev ",
                           " (", lat, ", ", lon, ")")

# generate the plot 
max.temp.1 <- aWhereCharts::generateaWhereStdDevChart(data = weather.df, 
                                                      variable = "maxTemp", 
                                                      title = max.temp.1.title)

# display the plot
#max.temp.1

# Minimum temperature with standard deviation ---------------------------------
min.temp.1.title <- paste0(location.name, "Minimum Temp w StdDev ",
                           " (", lat, ", ", lon, ")")

min.temp.1 <- aWhereCharts::generateaWhereStdDevChart(weather.df, 
                                                      "minTemp", 
                                                      min.temp.1.title)

# Potential evapotranspiration (PET) with standard deviation -----------------
pet.1.title <- paste0(location.name, "PET w StdDev ",
                      " (", lat, ", ", lon, ")")

pet.1 <- aWhereCharts::generateaWhereStdDevChart(weather.df, 
                                                 "pet", 
                                                 pet.1.title)

# Daily precipitation with standard deviation  ---------------------------
precip.1.title <- paste0(location.name, "Daily Precipitation w StdDev ",
                         " (", lat, ", ", lon, ")")

precip.1 <- aWhereCharts::generateaWhereStdDevChart(weather.df, 
                                                    "precipitation",  
                                                    title = precip.1.title)


# Daily precipitation without standard deviation  ---------------------------
precip.2.title <- paste0(location.name, "Daily Precipitation ",
                         " (", lat, ", ", lon, ")")

precip.2 <- aWhereCharts::generateaWhereChart(weather.df,
                                              "precipitation",  
                                              title = precip.2.title)


# Accumulated Precipitation with StdDev but no Effective Precipitation --------
no.eprecip.1.title <- paste0(location.name, "Accumulated Precipitation w StdDev ",
                             " (", lat, ", ", lon, ")")

no.eprecip.1 <- aWhereCharts::generateaWhereStdDevChart(weather.df, 
                                                        "accumulatedPrecipitation", 
                                                        no.eprecip.1.title)


# Precipitation and Effective Precipitation, Accumulated ------------------
eprecip.1.title <- paste0(location.name, "Precipitation and Effective Precipitation, 
                          Accumulated w Std Dev", " (", 
                          lat, ", ", lon, ")")

eprecip.1 <- aWhereCharts::generateaWhereStdDevChart(weather.df, 
                                                     "accumulatedPrecipitation", 
                                                     eprecip.1.title,
                                                     e_precip = TRUE, 
                                                     e_threshold = eP)


# Accumulated Precipitation -----------------------------------------------
acc.precip.2.title <- paste0(location.name, "Accumulated Precipitation ", " (", 
                             lat, ", ", lon, ")")

acc.precip.2 <- aWhereCharts::generateaWhereChart(weather.df, 
                                                  "accumulatedPrecipitation", 
                                                  acc.precip.2.title)


# Accumulated PET ---------------------------------------------------------
acc.pet.1.title <- paste0(location.name, "Accumulated PET w StdDev ", " (", 
                          lat, ", ", lon, ")")

acc.pet.1 <- aWhereCharts::generateaWhereStdDevChart(weather.df, 
                                                     "accumulatedPet", 
                                                     acc.pet.1.title)  


# P/PET -------------------------------------------------------------------
# ppet rarely is interpretable on a daily chart 
ppet.2.title <- paste0(location.name,"P PET ", " (", 
                       lat, ", ", lon, ")")

ppet.2 <- aWhereCharts::generateaWhereChart(weather.df, 
                                            "ppet", 
                                            ppet.2.title)


# 30 day rolling average eP/PET and P/PET ---------------------------------
# no eprecip/PET shows up if all rainfall events are less than the e_threshold
rolling.avg.ppet.2.title <- paste0(location.name,
                                   "30 day rolling avg eP PET and P PET ", 
                                   " (", lat, ", ", lon, ")")

rolling.avg.ppet.2 <- aWhereCharts::generateaWhereChart(weather.df, 
                                                        "rollingavgppet",
                                                        rolling.avg.ppet.2.title,
                                                        e_precip = TRUE, 
                                                        e_threshold = eP, 
                                                        rolling_window = roll.avg)



# mulitplot ---------------------------------------------------------------

# Select any of the above charts for multiplot:
# max.temp.1, min.temp.1, pet.1, precip.1, precip.2, no.eprecip.1, eprecip.1,
# acc.precip.2, acc.pet.1, ppet.2, rolling.avg.ppet.2

# set the graphics device parameters to write a .JPEG
jpeg(paste0(location.name,"_4chart.jpeg"), 
     width = 12, height = 6, 
     units = 'in', res = 500)

# generate the multiplot & write to JPEG
aWhereCharts::generateMultiplot(eprecip.1, rolling.avg.ppet.2, max.temp.1, pet.1, 
                                cols = 2, fontsize = 15, 
                                title = paste0("Current vs LTN at ", 
                                               location.name," (", lat, ", ", lon, ")", 
                                               "   eP = ",eP,"mm"))
# close the current plot object
dev.off()


# Write charts to file ----------------------------------------------------


# Maximum temperature
#max.temp.1 # display plot

# write the plot to file using the WriteJpeg function, an external R function
# in the "supporting_functions.R" file.
WriteJpeg(plt = max.temp.1, plt.title = max.temp.1.title)


# Minimum temperature with standard deviation
min.temp.1 
WriteJpeg(plt = min.temp.1, plt.title = min.temp.1.title)


# Potential evapotranspiration (PET) with standard deviation 
pet.1 
WriteJpeg(plt = pet.1, plt.title = pet.1.title)


# Daily precipitation with standard deviation  
precip.1 
WriteJpeg(plt = precip.1 , plt.title = precip.1.title)


# Daily precipitation without standard deviation  
precip.2 
WriteJpeg(plt = precip.2, plt.title = precip.2.title)


# Accumulated Precipitation with StdDev but no Effective Precipitation
no.eprecip.1
WriteJpeg(plt = no.eprecip.1, plt.title = no.eprecip.1.title)


# Precipitation and Effective Precipitation, Accumulated 
eprecip.1
WriteJpeg(plt = eprecip.1, plt.title = eprecip.1.title)


# Accumulated Precipitation 
acc.precip.2
WriteJpeg(plt = acc.precip.2, plt.title = acc.precip.2.title)


# Accumulated PET 
acc.pet.1
WriteJpeg(plt = acc.pet.1, plt.title = acc.pet.1.title)


# P/PET 
ppet.2
WriteJpeg(plt = ppet.2, plt.title = ppet.2.title)


# 30 day rolling average eP/PET and P/PET 
rolling.avg.ppet.2
WriteJpeg(plt = rolling.avg.ppet.2, plt.title = rolling.avg.ppet.2.title)
