library(tidyr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(aWhereAPI)
library(zoo)

#use this script to create a CSV file with obs, ag, and LTN of both - if desired
#- then it  charts from a selection of possibilities i.e., 
#the resultant current pre vs. LTN pren

#authenticate yourself to aWhere API & set working directory
#load_credentials()
load_credentials("c:/2018 work/awhere/Projects/R Working Directory/JC_credentials.txt")
#load_credentials("c:/aWhere/credentials/credentials.txt")


#setwd("c:/Data/CLIMARK/Project2018/")


#Use the package
#
library(aWhereCharts)
#devtools::install_github("aWhereAPI/aWhere-R-Charts")



outputfilename <- "Shamba 1" 

year_start <- 2008
year_end <- 2017    #LTN is defined as 2008-2017  (in 2018, change this to 2017)
writeFile <- TRUE

#run everything above this line 1x to get things moving
# _______________________


#Must run this next part!     Keep the period less than 365 days
    # Day to start your charting
     day_start <- "2018-02-01"        #MUST provide a good day to start


     #If you want day_end to be 7 days from now and thus include forecast
          day_end <- as.character(Sys.Date()+7)
     #if you want day_end to be yesterday
        # day_end <- as.character(Sys.Date()-1)
#if you want a fixed end date
#day_end <- "2017-10-31"


#set your effective precip amount so all runs are same
eP <- 30    



# if day_start to day_end >365 -  calls this out!!
#_______________________
# the MUST run ends here

#for charting, variable choices are: 
        #precip, accprecip, maxTemp, minTemp, pet, accpet, ppet or rollingavgppet

  #START HERE To produce chart of a MANUALLY entered LOCATION by lat long and name
  
  placename <- " Shamba 1 "      #this name will be on the title of the chart
  lat <-    3.2          #lat and lon will also be on the title of the chart
  lon <-    37.0
  #_____________

  
  #periodlength = day_end - day_start  -- checks if more than 365 days
  dd1 = as.Date(day_end)
  dd2 = as.Date(day_start)
  numdays <- dd1-dd2
  if(numdays > 364) print ("Period to long")
  
  
    
  weather_df <- data.frame()    #clears weather_df dataframe - resets it
  
  #pull the datasets    
  weather_df <- generateaWhereDataset(lat = lat, lon = lon, 
                                      day_start = day_start, 
                                      day_end = day_end, 
                                      year_start = year_start, 
                                      year_end = year_end)
 
   #generate the chart set   variables with a '1' include stddev, '2' does not
  #maxtemp2 <- generateaWhereChart(weather_df, "maxTemp", 
  #                            day_end = day_end, "Maximum Temp")
  maxtemp1 <- generateaWhereStdDevChart(weather_df, "maxTemp", "MaxTemp")
  
  # mintemp2 <- generateaWhereChart(weather_df, "minTemp", 
  #              day_end = day_end, "Minimum Temp")
  mintemp1 <- generateaWhereStdDevChart(weather_df, "minTemp", "Minimum Temp")
 
   pet1 <- generateaWhereStdDevChart(weather_df, "pet", "PET")
  
  precip1 <- generateaWhereStdDevChart(weather_df, "precipitation",  "Daily Precipitation")
  precip2 <- generateaWhereChart(weather_df,"precipitation",  "Daily Precipitation")
  
  #accprecip with StdDev but no e_precip
  noeprecip1 <- generateaWhereStdDevChart(weather_df, "accumulatedPrecipitation", "Accumulated Precipitation")
  
  eprecip1 <- generateaWhereStdDevChart(weather_df, "accumulatedPrecipitation", 
             "Precipitation and Effective Precipitation, Accumulated",
             e_precip = TRUE, e_threshold = eP)
  
  aprecip2 <- generateaWhereChart(weather_df, "accumulatedPrecipitation", "Accumulated Precipitation")
  
  
  accpet1 <- generateaWhereStdDevChart(weather_df, "accumulatedPet", "Accumulated PET")  
  
  #  accpet <- generateaWhereChart(weather_df, "accpet", 
  #            day_end = day_end, "Accumulated PET")
  
  #ppet rarely is interpretable on a daily chart 
  ppet2 <- generateaWhereChart(weather_df, "ppet", "P/PET")
  
  #this chart works - note that no eprecip/PET shows up if all rainfall 
  #  events are less than the e_threshold
  rollingavgppet2 <- generateaWhereChart(weather_df, "rollingavgppet",
       "30 day rolling avg eP/PET and P/PET",
      e_precip = TRUE, e_threshold = eP, rolling_window = 30)
  
  #  select any of the above charts for multiplot:
  # rollingavgppet, AND with StDev:::  accpet, eprecip, newprecip, precip1, pet, mintemp, maxtemp
  #___________________________________________________________________________________________________________________ 
  #this line will write the above objects to a chart for viewing - 
  
  #THIS NEEDS TO BE RENAMED FOR USE WITH THE PACKAGE
  generateMultiplot(aprecip2, rollingavgppet2, maxtemp1, pet1, cols = 2, fontsize = 12, 
            title = paste0("Current vs LTN at ", placename,"(", lat, ", ", lon, ")", "   eP = ",eP,"mm"))
  #_________________________________________________________________________________________________________________    

 
   
   
  #for a spreadsheet of the charted data 
  write.csv(weather_df, file = paste0("C:/Data/CLIMARK/Project2018/",outputfilename,".csv"))

  
  
  
  
  #write multiplot to a jpg...
  jpeg(paste0(placename,"4chart.jpeg"), width = 12, height = 6, units = 'in', res = 500)
  multiplot(eprecip1, rollingavgppet2, maxtemp1, pet1, cols = 2, fontsize = 15, 
            title = paste0("Current vs LTN at ", placename," (", lat, ", ", lon, ")", "   eP = ",eP,"mm"))
  dev.off()
  

#______________________________________
    #create a stack of individual jpeg -   '1' =stdev    '2' = regular
  #   chart set of jpeg all start with 'placename' as above
  
  chprecip1 <- generateaWhereStdDevChart(weather_df, "precipitation", 
  title = paste0("Daily Precipitation   ", placename," (", lat, ", ", lon, ")"))
  jpeg(paste0(placename,  "DailyprecipStDev.jpeg"), width = 10, height = 6, units = 'in', res = 500)
  print(chprecip1)
  dev.off()
  
  chprecip2 <- generateaWhereChart(weather_df, "precipitation", 
  title = paste0("Daily Precipitation   ", placename," (", lat, ", ", lon, ")"))
  jpeg(paste0(placename,  "Dailyprecip.jpeg"), width = 10, height = 6, units = 'in', res = 500)
  print(chprecip2)
  dev.off()
  
  cheprecip1 <- generateaWhereStdDevChart(weather_df, "accumulatedPrecipitation", 
  title = paste("Precipitation and Effective Precipitation, Accumulated,", placename," (", lat, ", ", lon, ")"),
  e_precip = TRUE, e_threshold = eP)
  jpeg(paste0(placename,  "Accprecip.jpeg"), width = 10, height = 6, units = 'in', res = 500)
  print(cheprecip1)
  dev.off() 
  
  
  chrollingavgppet2 <- generateaWhereChart(weather_df, "rollingavgppet",  
  title = paste("30 day rolling avg eP/PET and P/PET,",placename," (", lat, ", ", lon, ")"),
  e_precip = TRUE, e_threshold = eP, rolling_window = 30)
  jpeg(paste0(placename, "rollingavgppet.jpeg"), width = 10, height = 6, units = 'in', res = 500)
  print(chrollingavgppet2)
  dev.off()
  
  chmaxtemp1 <- generateaWhereStdDevChart(weather_df, "maxTemp",
  title = paste("Maximum Temperature,", placename," (", lat, ", ", lon, ")"))
  jpeg(paste0(placename, "MaxTemp.jpeg"), width = 10, height = 6, units = 'in', res = 500)
  print(chmaxtemp1)
  dev.off()
  
  chmintemp1 <- generateaWhereStdDevChart(weather_df, "minTemp",
  title = paste("Minimum Temperature,", placename," (", lat, ", ", lon, ")"))  
  jpeg(paste0(placename, "MinTemp.jpeg"), width = 10, height = 6, units = 'in', res = 500)
  print(chmintemp1)
  dev.off()
  
  
  chpet1 <- generateaWhereStdDevChart(weather_df, "pet",
  title = paste("Potential EvapoTranspiration,", placename," (", lat, ", ", lon, ")"))    
  jpeg(paste0(placename, "PET.jpeg"), width = 10, height = 6, units = 'in', res = 500)
  print(chpet1)
  dev.off()
  
  #this one rarely makes sense   
  #  jpeg(paste0(locations$Country[i]," ", locations$Town[i]," ", "PPET.jpeg"), width = 10, height = 6, units = 'in', res = 500)
  #  print(ppet)
  #  dev.off() 
  



