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

# load external R functions in local file
source("supporting_functions.R")

# define input paths and variables ----------------------------------------

# working directory - where input files are located and outputs will be saved.
working.dir <- "c:/Data/CLIMARK/Project2018"
working.dir <- "~/Documents/aWhere/"

# filename containing your aWhere credientials (key and secret),
# a text file where line 1 is the Consumer Key,
# line 2 is the Consumer Secret, and line 3 is a blank line. 
credentials.file <- "c:/2018 work/awhere/Projects/R Working Directory/JC_credentials.txt"
credentials.file <- "credentials.txt"

# Location and name to produce charts for
lat <- 3.2
lon <- 37.0
location.name <- paste("Shamba1") # base filename for outputs. 

# starting and ending years and days for the time series. each is a vector with 
# the starting value in position 1 and the ending value in position 2. 
years <- c(2010, 2017)

# start day can "today", "yesterday", "tomorrow", or "YYYY-MM-DD". 
# end day is calculated using the largest value in n.day.forecasts vector.
# to specify a different end day, add the "ending.day" argument
# to the GetDays function with a specific end date, "YYYY-MM-DD". 
day.start <- "yesterday"
n.day.forecast <- 7
days <- GetDays(starting.day = day.start, 
                forecast.days = n.day.forecast)

# effective precip amount for consistency across plots
eP <- 30    

# size of rolling average window for consistency across plots
roll.avg <- 30

# processing steps ----------------------------------------------------------

# For charting, variable choices are: 
# precip, accprecip, maxTemp, minTemp, pet, accpet, ppet or rollingavgppet

# Check if time range is more than 365 days
duration <- as.Date(days[2]) - as.Date(days[1])

if(duration > 364) { 
  print ("Period too long")
}

# pull the datasets    
weather.df <- aWhereCharts::generateaWhereDataset(lat = lat, lon = lon, 
                                                  day_start = days[1], 
                                                  day_end = days[2], 
                                                  year_start = years[1], 
                                                  year_end = years[2])

# write forecast to .csv file 
utils::write.csv(weather.df, 
                 file = paste(location.name, 
                              base::paste(lat, lon, sep="_"),
                              base::paste(days, collapse="_"),
                              base::paste(years, collapse="_"),
                              ".csv", sep="_"))

# generate charts ---------------------------------------------------------



# Maximum temperature with standard deviation ---------------------------------
max.temp.1.title <- paste0("Maximum Temp w StdDev ", location.name,
                           " (", lat, ", ", lon, ")")

max.temp.1 <- aWhereCharts::generateaWhereStdDevChart(data = weather.df, 
                                                      variable = "maxTemp", 
                                                      title = max.temp.1.title)
# display plot
max.temp.1 

# write to file
WriteJpeg(plt = max.temp.1, plt.title = max.temp.1.title)


# Minimum temperature with standard deviation ---------------------------------
min.temp.1.title <- paste0("Minimum Temp w StdDev ", location.name,
                           " (", lat, ", ", lon, ")")

min.temp.1 <- aWhereCharts::generateaWhereStdDevChart(weather.df, 
                                                      "minTemp", 
                                                      min.temp.1.title)

# display plot
min.temp.1 

# write to file
WriteJpeg(plt = min.temp.1, plt.title = min.temp.1.title)


# Potential evapotranspiration (PET) --------------------------------------
pet.1.title <- paste0("PET w StdDev ", location.name,
                      " (", lat, ", ", lon, ")")

pet.1 <- aWhereCharts::generateaWhereStdDevChart(weather.df, 
                                                 "pet", 
                                                 pet.1.title)

# display plot
pet.1 

# write to file
WriteJpeg(plt = pet.1, plt.title = pet.1.title)


# Daily precipitation with standard deviations  ---------------------------
precip.1.title <- paste0("Daily Precipitation w StdDev ", location.name,
                         " (", lat, ", ", lon, ")")

precip.1 <- aWhereCharts::generateaWhereStdDevChart(weather.df, 
                                                    "precipitation",  
                                                    title = precip.1.title)
# display plot
precip.1 

# write to file
WriteJpeg(plt = precip.1 , plt.title = precip.1.title)


# Daily precipitation without standard deviations  ---------------------------
precip.2.title <- paste0("Daily Precipitation ", location.name,
                         " (", lat, ", ", lon, ")")

precip.2 <- aWhereCharts::generateaWhereChart(weather.df,
                                              "precipitation",  
                                              title = precip.2.title)

# display plot
precip.2 

# write to file
WriteJpeg(plt = precip2, plt.title = precip.2.title)


# Accumulated Precipitation with StdDev but no Effective Precipitation --------
no.eprecip.1.title <- paste0("Accumulated Precipitation w StdDev ", location.name,
                             " (", lat, ", ", lon, ")")
no.eprecip.1 <- aWhereCharts::generateaWhereStdDevChart(weather.df, 
                                                        "accumulatedPrecipitation", 
                                                        no.eprecip.1.title)
# display plot
no.eprecip.1

# write to file
WriteJpeg(plt = no.eprecip.1, plt.title = no.eprecip.1.title)


# Precipitation and Effective Precipitation, Accumulated ------------------
eprecip.1.title <- paste0("Precipitation and Effective Precipitation, 
                          Accumulated w Std Dev", location.name, " (", 
                          lat, ", ", lon, ")")

eprecip.1 <- aWhereCharts::generateaWhereStdDevChart(weather.df, 
                                                     "accumulatedPrecipitation", 
                                                     eprecip.1.title,
                                                     e_precip = TRUE, 
                                                     e_threshold = eP)
# display plot
eprecip.1

# write to file
WriteJpeg(plt = eprecip.1, plt.title = eprecip.1.title)


# Accumulated Precipitation -----------------------------------------------

acc.precip.2.title <- paste0("Accumulated Precipitation ", location.name, " (", 
                             lat, ", ", lon, ")")

acc.precip.2 <- aWhereCharts::generateaWhereChart(weather.df, 
                                                  "accumulatedPrecipitation", 
                                                  acc.precip.2.title)

# display plot
acc.precip.2

# write to file
WriteJpeg(plt = acc.precip.2, plt.title = acc.precip.2.title)


# Accumulated PET ---------------------------------------------------------

acc.pet.1.title <- paste0("Accumulated PET w StdDev ", location.name, " (", 
                          lat, ", ", lon, ")")

acc.pet.1 <- aWhereCharts::generateaWhereStdDevChart(weather.df, 
                                                     "accumulatedPet", 
                                                     acc.pet.1.title)  
# display plot
acc.pet.1

# write to file
WriteJpeg(plt = acc.pet.1, plt.title = acc.pet.1.title)


# P/PET -------------------------------------------------------------------
# ppet rarely is interpretable on a daily chart 

ppet.2.title <- paste0("P/PET ", location.name, " (", 
                       lat, ", ", lon, ")")

ppet.2 <- aWhereCharts::generateaWhereChart(weather.df, 
                                            "ppet", 
                                            ppet.2.title)
# display plot
ppet.2

# write to file
WriteJpeg(plt = ppet.2, plt.title = ppet.2.title)


# 30 day rolling average eP/PET and P/PET ---------------------------------

# no eprecip/PET shows up if all rainfall events are less than the e_threshold
rolling.avg.ppet.2.title <- "30 day rolling avg eP/PET and P/PET "

rolling.avg.ppet.2 <- aWhereCharts::generateaWhereChart(weather.df, 
                                                        "rollingavgppet",
                                                        rolling.avg.ppet.2.title,
                                                        e_precip = TRUE, 
                                                        e_threshold = eP, 
                                                        rolling_window = roll.avg)
# display plot
rolling.avg.ppet.2

# write to file
WriteJpeg(plt = rolling.avg.ppet.2, plt.title = rolling.avg.ppet.2.title)



# mulitplot ---------------------------------------------------------------

# Select any of the above charts for multiplot:
# max.temp.1, min.temp.1, pet.1, precip.1, precip.2, no.eprecip.1, eprecip.1,
# acc.precip.2, acc.pet.1, ppet.2, rolling.avg.ppet.2

# set the graphics device parameters to write a .JPEG
jpeg(paste0(location.name,"4chart.jpeg", sep = "_"), 
     width = 12, height = 6, 
     units = 'in', res = 500)

# generate the multiplot 
generateMultiplot(eprecip1, rollingavgppet2, maxtemp1, pet1, 
                  cols = 2, fontsize = 15, 
                  title = paste0("Current vs LTN at ", 
                                 location.name," (", lat, ", ", lon, ")", 
                                 "   eP = ",eP,"mm"))

# close the current plot object
dev.off()



















# Incorporate into above, then delete --------------------------------

cheprecip1 <- generateaWhereStdDevChart(weather.df, 
                                        "accumulatedPrecipitation",
                                        title = paste("Precipitation and Effective Precipitation, Accumulated,", 
                                                      placename," (", lat, ", ", lon, ")"),
                                        e_precip = TRUE, 
                                        e_threshold = eP)
jpeg(paste0(placename,  
            "Accprecip.jpeg"), 
     width = 10, 
     height = 6, 
     units = 'in', 
     res = 500)

print(cheprecip1)
dev.off() 


# Plot: Rolling average P/PET  --------------------------------------------

chrollingavgppet2 <- generateaWhereChart(weather.df, 
                                         "rollingavgppet",  
                                         title = paste("30 day rolling avg eP/PET and P/PET,",
                                                       placename," (", lat, ", ", lon, ")"),
                                         e_precip = TRUE, 
                                         e_threshold = eP, 
                                         rolling_window = 30)

jpeg(paste0(placename, 
            "rollingavgppet.jpeg"), 
     width = 10, 
     height = 6, 
     units = 'in', 
     res = 500)

print(chrollingavgppet2)
dev.off()


# Maximum Temperature with StDev ------------------------------------------

chmaxtemp1 <- generateaWhereStdDevChart(weather.df, 
                                        "maxTemp",
                                        title = paste("Maximum Temperature,", 
                                                      placename," (", lat, ", ", lon, ")"))

jpeg(paste0(placename, 
            "MaxTemp.jpeg"), 
     width = 10, 
     height = 6, 
     units = 'in', 
     res = 500)

print(chmaxtemp1)
dev.off()


# Plot: Minimum Temperature with StdDev ------------------------------------

chmintemp1 <- generateaWhereStdDevChart(weather.df, 
                                        "minTemp",
                                        title = paste("Minimum Temperature,", 
                                                      placename," (", lat, ", ", lon, ")"))  

jpeg(paste0(placename, 
            "MinTemp.jpeg"), 
     width = 10, 
     height = 6, 
     units = 'in', 
     res = 500)

print(chmintemp1)
dev.off()


# Plot: PET with StdDev ---------------------------------------------------

chpet1 <- generateaWhereStdDevChart(weather.df, 
                                    "pet",
                                    title = paste("Potential EvapoTranspiration,", 
                                                  placename," (", lat, ", ", lon, ")"))    

jpeg(paste0(placename, "PET.jpeg"), 
     width = 10, 
     height = 6, 
     units = 'in', 
     res = 500)

print(chpet1)
dev.off()

