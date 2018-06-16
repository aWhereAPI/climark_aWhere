# load required packages 
library(aWhereAPI)
library(dplyr)
#library(tmap)
#library(sf)

# get a forecast for a region - 1 to 7 day aggregate
# read in the region then using the lat, long - pull the forecast 

#authenticate yourself to aWhere API & set working directory
#setwd("c:/Data/CLIMARK/Project2018/")  
setwd("~/Documents/aWhere/") #VS

load_credentials("c:/2018 work/awhere/Projects/R Working Directory/JC_credentials.txt") #JC
load_credentials("~/Documents/aWhere/awhere_credentials_VS.txt") #VS

# define the starting and ending year to calculate the LTN
# in the generateaWhereDataset function
year.start <- 2010  
year.end <- 2017

# Read the template for which a forecast will be mapped
template.file <- "CLIMARKonlyWardTemplate.csv"
template.place <- read.csv(template.file) %>% 
  select( c(locationid, 
             latitude,
             longitude,
             shapewkt))

# Define the output filename for the forecast
filename.out <- "AOI_Forecast"

# specify the start day
day.start <- as.character(Sys.Date()-1)   # yesterday
#day.start <- as.character((Sys.Date()))  # today - for forecast
#day.start <- "2018-06-05" #              specific date 

# specify the end day
#day.end <- as.character(Sys.Date()-1) # yesterday
day.end <- as.character(Sys.Date()+7) # 7 days from now
#day.end <- "2018-06-10" # fixed end date


# This works! Grabs a nice forecast df
# forecast2 pulls only the forecast 
# it seems odd, but forecasts_latlng can only pull forecast but weirdly generateaWheredataset
# appears to require you grab at least 1 day observed...
# foredat <- as.character((Sys.Date()))
# forecast2 <- forecasts_latlng(lat, lon, day_start = foredat, day_end = day_end, block_size = 24)
# agronomic_values_latlng() 
# ag2 <- agronomic_values_latlng(24.74628, 68.4232, day_start = "2016-03-15", day_end = "2016-04-01")
# obs2 <- daily_observed_latlng(latpt, longpt, day_start = day_start, day_end = day_end)
  

start.time <- Sys.time()    

# loops through all location ID's in the template file
for (i in 1:nrow(template.place)){
  
  # get the current lat, lon, and location ID 
  lat <- template.place$latitude[i]
  lon <- template.place$longitude[i]
  loc.ID <- template.place$locationid[i]

  # clear/reset the forecast data frame
  forecast1 <- data.frame()    
  
  # pull forecast and LTN for the period year_start to year_end
  forecast1 <- generateaWhereDataset(lat = lat, 
                                 lon = lon, 
                                 day_start = day.start, 
                                 day_end = day.end, 
                                 year_start = year.start, 
                                 year_end = year.end)# %>% 
#select( -c(accumulatedPrecipitation.amount,gdd,gdd.average,ppet, accumulatedPpet)) %>%
#  select( -c(minTemp, maxTemp, precip, pet.amount, accumulatedPrecipitation.average)) %>%
#  select( -c( accumulatedPet.amount, accumulatedPet.average, accumulatedPpet.average))

  forecast1 = forecast1[-1,]  #this removes the first row - which is observed
  forecast1$locationid <- loc.ID

  # combine forecast data into a single data frame: forecast.all
  if(i==1) {
    forecast.all <- forecast1
  } else {
    forecast.all <- rbind(forecast1, forecast.all)
  }

}  # End of looping through TemplateCore

# check the elapsed time
end.time <- Sys.time()
run.time <- end.time - start.time
print(run.time)
    
# to rebuild forecast.all, renames the forecast variables
   
#    names(forecast.all)[grep("ppet.average", names(forecast.all))] <- "LTNppet"
#    names(forecast.all)[grep("precipitation.average", names(forecast.all))] <- "LTN_Pre"
#    names(forecast.all)[grep("precip.forecast", names(forecast.all))] <- "forePre"
#    names(forecast.all)[grep("maxTemp.forecast", names(forecast.all))] <- "foremaxT"
#    names(forecast.all)[grep("minTemp.forecast", names(forecast.all))] <- "foreminT"
#    names(forecast.all)[grep("maxTemp.average", names(forecast.all))] <- "LTNmaxT"
#    names(forecast.all)[grep("minTemp.average", names(forecast.all))] <- "LTNminT"
#    names(forecast.all)[grep("pet.average", names(forecast.all))] <- "LTNpet"
    
# this is crazy - if I write the forecast.all(df) to .csv, the READ the same .csv
# with the stringsAsFactors = FALSE THEN i can find the max and min dates, and add
# a few days to it so I can map say today +3 days as opposed to the whole 7 days forecast

write.csv(forecast.all, 
          file = paste0(filename.out,
                        ".csv"))

forecast.all <- read.csv(file = paste0(filename.out, 
                                       ".csv"), 
                         stringsAsFactors=FALSE)    
 
  
  
    
    
#  ********************* STOP HERE  - the df forecast.all  will write your daily output to .csv
    
    
    
      
    
    
       
    #with locationid in forecast.all - merge with TemplateCore anytime to get shapewkt, MAP!
        # then write code to add up precip and LTN precip for any number of the days (1 to 7)
    # write that df to csv - map the forecast

    #trying some mappable info using forecast.all -     

#get total Pre by locid
        full7dayf <- aggregate(forecast.all$forePre, by=list(locationid=forecast.all$locationid), FUN=sum)
#find min date - 
        maxdate <- max(forecast.all$date)
        mindate <- min(forecast.all$date)
        targetdate <- as.Date(mindate)+4
    day3forecast <- filter(forecast.all, forecast.all$date < targetdate)
    #da3forecast now contains 'today' + 3 days  forecast.all has all 7 days
    only4dayf <- aggregate(day3forecast$forePre, by=list(locationid=day3forecast$locationid), FUN=sum)
#add shapewkt to full7dayf and map it    
    #subsetdata <- full7dayf[which(full7dayf$locationid %in% TemplateCore$locationid),] 
    MapForecast7 <- merge(full7dayf, TemplateCore)
    MapForecast4 <- merge(only4dayf, TemplateCore)
    
    
    AOItemplate <- st_as_sf(MapForecast7, wkt = "WKT")
    st_crs(AOItemplate) = 4326   #this is the code to WGS84 projection
maptitle <- "Precip Forecast 8-14 April Odisha"
    tmap_mode("view")
    tm_shape(AOItemplate) +
      tm_fill("x", palette = (RColorBrewer::brewer.pal(9, "RdYlGn")), auto.palette.mapping = FALSE,
              style = "cont",
              breaks = seq(from = 0, to = 150, by = 20),
              id = "locationid", title = maptitle)
 
       AOItemplate <- st_as_sf(MapForecast4, wkt = "WKT")
    st_crs(AOItemplate) = 4326   #this is the code to WGS84 projection
    maptitle <- "Precip (mm) Forecast 8-11 April Odisha"
    tmap_mode("view")
    tm_shape(AOItemplate) +
      tm_fill("x", palette = (RColorBrewer::brewer.pal(9, "RdYlGn")), auto.palette.mapping = FALSE,
              style = "cont",
              breaks = seq(from = 0, to = 150, by = 20),
              id = "locationid", title = maptitle)    
           
rm(forecast1)
rm(forecast.all)

#select( -c(latitude.y, longitude.y, day.x, day.y, accumulatedGdd, accumulatedPpet )) %>%
# select( -c(accumulatedPrecipitation.amount,accumulatedPet.amount )) 






obs <- daily_observed_latlng(lat, lon, day_start, day_end) %>% 
  cbind(., data.frame(do.call(rbind, strsplit(.$date, "-")))) %>% 
  mutate(day = paste0(X2, "-", X3))%>% 
  select(-c(X1,X2,X3))
#can control which columns and what order in final dataframe using select

names(obs)[grep("location.latitude", names(obs))] <- "latitude"
names(obs)[grep("location.longitude", names(obs))] <- "longitude"
names(obs)[grep("temperatures.max", names(obs))] <- "maxTemp"
names(obs)[grep("temperatures.min", names(obs))] <- "minTemp"
names(obs)[grep("precipitation.amount", names(obs))] <- "Precip"
names(obs)[grep("relativeHumidity.max", names(obs))] <- "RHmax"
names(obs)[grep("relativeHumidity.min", names(obs))] <- "RHmin"
names(obs)[grep("wind.morningMax", names(obs))] <- "windmornmax"
names(obs)[grep("wind.average", names(obs))] <- "windavg"
names(obs)[grep("wind.dayMax", names(obs))] <- "winddaymax"

ag <- agronomic_values_latlng(lat, lon, day_start, day_end) %>% 
  cbind(., data.frame(do.call(rbind, strsplit(.$date, "-")))) %>% 
  mutate(day = paste0(X2, "-", X3)) %>% 
  select(-c(X1,X2,X3))

names(ag)[grep("pet.amount", names(ag))] <- "PET"

forecast.all <- merge(obs, ag, by = c("date")) %>% 
  
  select( -c(latitude.y, longitude.y, day.x, day.y, accumulatedGdd, accumulatedPpet )) %>%
  select( -c(accumulatedPrecipitation.amount,accumulatedPet.amount )) 

#  select(day, latitude.x, longitude.x, accumulatedPrecipitation.amount)
#colnames(weather_full) <- c("monthday", "lat", "long", "accPrecipLTN")


write.csv(forecast.all, file = paste0("C:/Data/R output/",outputfilename,".csv"))








#outputfilename <- "rainfall vs LTN Chart"  #this is the .csv tile - if unchanged it may overwrite an existing file
#year_start <- 2008
#year_end <- 2016     #LTN is defined as 2008-2016  (in 2018, change this to 2017)
#writeFile <- FALSE

#if (writeFile == TRUE) {
#  write.csv(weather_df, file = paste0("C:/Data/R output/",outputfilename,".csv"))
#} else {
#  cat("Caution: Weather dataset not written to disk")
#}
