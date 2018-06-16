# load required packages 
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(aWhereAPI)
library(zoo)
library(aWhereCharts)
library(curl)


# user defines inputs -----------------------------------------------------

# Use this script to create a CSV file with obs, ag, and LTN of both - if desired
# - then it  charts from a selection of possibilities i.e., 
# The resultant current pre vs. LTN pren

# Authenticate yourself to aWhere API & set working directory
# use the "load_credentials" function from tthe aWhereAPI package
# specify the path and filename containing the user credentials,
# a text file where line 1 is the Consumer Key,
# line 2 is the Consumer Secret, and line 3 is a blank line. 
load_credentials("c:/2018 work/awhere/Projects/R Working Directory/JC_credentials.txt")
load_credentials("~/Documents/aWhere/awhere_credentials_VS.txt")

#setwd("c:/Data/CLIMARK/Project2018/")
setwd("~/Documents/aWhere/")

filename.out <- "Shamba 1" 

year.start <- 2008
year.end <- 2017    #LTN is defined as 2008-2017
write.file <- TRUE

# Must run this next part! Keep the period less than 365 days
# Day to start your charting. MUST provide a good day to start
day.start <- "2018-02-01"        

# If you want day.end to be 7 days from now and thus include forecast
day.end <- as.character(Sys.Date()+7)

# If you want day.end to be yesterday
# day.end <- as.character(Sys.Date()-1)

# If you want a fixed end date
#day.end <- "2017-10-31"

# Set your effective precip amount so all runs are same
eP <- 30    

# For charting, variable choices are: 
# precip, accprecip, maxTemp, minTemp, pet, accpet, ppet or rollingavgppet

# START HERE To produce chart of a MANUALLY entered LOCATION by lat long and name
  
placename <- " Shamba 1 "  # this name will be on the title of the chart
lat <- 3.2          # lat and lon will also be on the title of the chart
lon <- 37.0

# read data -----------------------------------------------------

# Checks if time range is more than 365 days
dd1 <- as.Date(day.end)
dd2 <- as.Date(day.start)
numdays <- dd1-dd2

if(numdays > 364){ 
  print ("Period too long")
}
  
# clear/reset the weather.df data frame
weather.df <- data.frame()    
  
# pull the datasets    
weather.df <- generateaWhereDataset(lat = lat, lon = lon, 
                                      day_start = day.start, 
                                      day_end = day.end, 
                                      year_start = year.start, 
                                      year_end = year.end)
 

# generate charts ---------------------------------------------------------

# Maximum temperature
maxtemp1 <- generateaWhereStdDevChart(data = weather.df, 
                                        variable = "maxTemp", 
                                        title = "Maximum Temp")
# Minimum temperature
mintemp1 <- generateaWhereStdDevChart(weather.df, 
                                        "minTemp", 
                                        "Minimum Temp")

# Potential evapotranspiration (PET)
pet1 <- generateaWhereStdDevChart(weather.df, 
                                  "pet", 
                                  "PET")

# Daily precipitation with standard deviations 
precip1 <- generateaWhereStdDevChart(weather.df, 
                                     "precipitation",  
                                     "Daily Precipitation")

# Daily Precipitation without standard deviations 
precip2 <- generateaWhereChart(weather.df,
                               "precipitation",  
                               "Daily Precipitation")
  
# Accumulated Precipitation with StdDev but no Effective Precipitation
noeprecip1 <- generateaWhereStdDevChart(weather.df, 
                                        "accumulatedPrecipitation", 
                                        "Accumulated Precipitation")

eprecip1 <- generateaWhereStdDevChart(weather.df, 
                                      "accumulatedPrecipitation", 
                                      "Precipitation and Effective Precipitation, Accumulated",
                                      e_precip = TRUE, 
                                      e_threshold = eP)
# Accumulated Precipitation 
aprecip2 <- generateaWhereChart(weather.df, 
                                "accumulatedPrecipitation", 
                                "Accumulated Precipitation")
# Accumulated PET
accpet1 <- generateaWhereStdDevChart(weather.df, 
                                     "accumulatedPet", 
                                      "Accumulated PET")  

# ppet rarely is interpretable on a daily chart 
ppet2 <- generateaWhereChart(weather.df, 
                             "ppet", 
                             "P/PET")
  
# this chart works - note that no eprecip/PET shows up if all rainfall 
# events are less than the e_threshold
rollingavgppet2 <- generateaWhereChart(weather.df, 
                                       "rollingavgppet",
                                       "30 day rolling avg eP/PET and P/PET",
                                       e_precip = TRUE, 
                                       e_threshold = eP, 
                                       rolling_window = 30)

# Mulitplot ---------------------------------------------------------------
# Select any of the above charts for multiplot:
# rollingavgppet, AND with StDev::: accpet, eprecip, newprecip, precip1, pet, mintemp, maxtemp

generateMultiplot(aprecip2, 
                  rollingavgppet2, 
                  maxtemp1, 
                  pet1, 
                  cols = 2, 
                  fontsize = 12, 
                  title = paste0("Current vs LTN at ", 
                                 placename, "(", lat, ", ", lon, ")", 
                                 "   eP = ", eP,"mm"))


# Write data and Multiplot to file --------------------------------------------

# for a spreadsheet of the charted data 
write.csv(weather.df, 
          file = paste0(filename.out,
                        ".csv"))

# write multiplot to a jpg...
jpeg(paste0(placename,"4chart.jpeg"), 
     width = 12, 
     height = 6, 
     units = 'in', 
     res = 500)

generateMultiplot(eprecip1, 
                  rollingavgppet2, 
                  maxtemp1, 
                  pet1, 
                  cols = 2, 
                  fontsize = 15, 
                  title = paste0("Current vs LTN at ", 
                                 placename," (", lat, ", ", lon, ")", 
                                 "   eP = ",eP,"mm"))

# close the current plot object
dev.off()
  

# Write individual plots to separate JPEG.
# '1' =stdev    '2' = regular



# Plot: Precipitation with StDev ------------------------------------------------

chprecip1 <- generateaWhereStdDevChart(weather.df, 
                                       "precipitation", 
                                       title = paste0("Daily Precipitation   ", 
                                                      placename," (", lat, ", ", lon, ")"))
jpeg(paste0(placename,  
            "DailyprecipStDev.jpeg"), 
     width = 10, 
     height = 6, 
     units = 'in', 
     res = 500)

print(chprecip1)
dev.off()
  


# Plot: Precipitation  --------------------------------------------------------

chprecip2 <- generateaWhereChart(weather.df, 
                                 "precipitation",
                                 title = paste0("Daily Precipitation   ", 
                                                placename," (", lat, ", ", lon, ")"))

jpeg(paste0(placename,  
            "Dailyprecip.jpeg"), 
     width = 10, 
     height = 6, 
     units = 'in', 
     res = 500)

print(chprecip2)
dev.off()


# Plot: Effective Precipitation with StDev --------------------------------

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



