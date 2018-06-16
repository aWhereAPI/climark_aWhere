
# get a forecast for a region - 1 to 7 day aggregate
# read in the region then using the lat, long - pull the forecast 

library(aWhereAPI)
library(dplyr)
#library(tmap)
#library(sf)



#authenticate yourself to aWhere API & set working directory
#setwd("c:/Data/CLIMARK/Project2018/")  #set this as this makes all run if all files are here
load_credentials("c:/2018 work/awhere/Projects/R Working Directory/JC_credentials.txt")

#source("C:/Users/johncorbett/Documents/R Working Directory/Analysis/function_pullAPIData.R")




year_start <- 2010  #this to calc the LTN in the generateaWhereDataset function
year_end <- 2017


#the template for which a forecast will be mapped...
#TemplatefromCSV <- read.csv("c:/Data/CLIMARK/Project2018/Wardclip_next7_June04.csv") 
TemplatefromCSV <- read.csv("Wardclip_next7_June04.csv") 
TemplateCore <- select(TemplatefromCSV, locationid, latitude, longitude, shapewkt)

outputfilename <- "AOI_Forecast"

day_start <- as.character(Sys.Date()-1)   #start is yesterday
#day_start <- as.character((Sys.Date()))   #start is today - for forecast
#day_start <- "2018-06-05"
   #If you want day_end to be yesterday
   #day_end <- as.character(Sys.Date()-1)
   #If you want day_end to be 7 days from now
    day_end <- as.character(Sys.Date()+7)
    
   #if you want a fixed end date
   #day_end <- "2018-06-10"

    #This works!  grabs a nice forecast df
#fcst2 pulls only the forecast 
     #it seems odd, but forecasts_latlng can only pull forecast but weirdly generateaWheredataset
     #appears to require you grab at least 1 day observed...
#    foredat <- as.character((Sys.Date()))
#  fcst2 <- forecasts_latlng(lat, lon, day_start = foredat, day_end = day_end, block_size = 24)
    #agronomic_values_latlng() 
#ag2 <- agronomic_values_latlng(24.74628, 68.4232, day_start = "2016-03-15", day_end = "2016-04-01")
#obs2 <- daily_observed_latlng(latpt, longpt, day_start = day_start, day_end = day_end)
  
    start_time <- Sys.time()    
    for (i in 1:nrow(TemplateCore)){
   # for (i in 1:2) {   
  #for (i in 1:10) {
      lat <- TemplatefromCSV$latitude[i]
      lon <- TemplatefromCSV$longitude[i]
      locID <- TemplatefromCSV$locationid[i]
fcst1 <- data.frame()    #clears weather_df dataframe - resets it
#  fcst1 pull forecast and LTN for the period year_start to year_end
fcst1 <- generateaWhereDataset(lat = lat, lon = lon, 
                                day_start = day_start, 
                                day_end = day_end, 
                                year_start = year_start, 
                                year_end = year_end)# %>% 
#select( -c(accumulatedPrecipitation.amount,gdd,gdd.average,ppet, accumulatedPpet)) %>%
#  select( -c(minTemp, maxTemp, precip, pet.amount, accumulatedPrecipitation.average)) %>%
#  select( -c( accumulatedPet.amount, accumulatedPet.average, accumulatedPpet.average))

fcst1 = fcst1[-1,]  #this removes the first row - which is observed
fcst1$locationid <- locID
#at this point, ONE location has forecast data - need to put that into another df
print(i)

if(i==1) {
manydata <- fcst1

} else {
  manydata <- rbind(fcst1, manydata)
}

}  # End of looping through TemplateCore
 
    end_time <- Sys.time()
    RunTime <- end_time - start_time
    print(RunTime)
    
    
    
       
    #to rebuild manydata, renames the forecast variables
   
    
 #   names(manydata)[grep("ppet.average", names(manydata))] <- "LTNppet"
#    names(manydata)[grep("precipitation.average", names(manydata))] <- "LTN_Pre"
#    names(manydata)[grep("precip.forecast", names(manydata))] <- "forePre"
#    names(manydata)[grep("maxTemp.forecast", names(manydata))] <- "foremaxT"
#    names(manydata)[grep("minTemp.forecast", names(manydata))] <- "foreminT"
#    names(manydata)[grep("maxTemp.average", names(manydata))] <- "LTNmaxT"
#    names(manydata)[grep("minTemp.average", names(manydata))] <- "LTNminT"
#    names(manydata)[grep("pet.average", names(manydata))] <- "LTNpet"
    
       #this is crazy - if I write the manydata(df) to .csv, the READ the same .csv
      #with the stringsAsFactors = FALSE THEN i can find the max and min dates, and add
     #a few days to it so I can map say today +3 days as opposed to the whole 7 days forecast
   # write.csv(manydata, file = paste0("C:/Data/R output/",outputfilename,".csv"))
    write.csv(manydata, file = paste0(outputfilename,".csv"))
   # manydata <- read.csv(file = paste0("C:/Data/R output/",outputfilename,".csv"), stringsAsFactors=FALSE)
    manydata <- read.csv(file = paste0(outputfilename,".csv"), stringsAsFactors=FALSE)    
 
  
  
    
    
    #  ********************* STOP HERE  - the df manydata  will write your daily output to .csv
    
    
    
      
    
    
       
    #with locationid in manydata - merge with TemplateCore anytime to get shapewkt, MAP!
        # then write code to add up precip and LTN precip for any number of the days (1 to 7)
    # write that df to csv - map the forecast

    #trying some mappable info using manydata -     

#get total Pre by locid
        full7dayf <- aggregate(manydata$forePre, by=list(locationid=manydata$locationid), FUN=sum)
#find min date - 
        maxdate <- max(manydata$date)
        mindate <- min(manydata$date)
        targetdate <- as.Date(mindate)+4
    day3forecast <- filter(manydata, manydata$date < targetdate)
    #da3forecast now contains 'today' + 3 days  manydata has all 7 days
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
           
rm(fcst1)
rm(manydata)

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

manydata <- merge(obs, ag, by = c("date")) %>% 
  
  select( -c(latitude.y, longitude.y, day.x, day.y, accumulatedGdd, accumulatedPpet )) %>%
  select( -c(accumulatedPrecipitation.amount,accumulatedPet.amount )) 

#  select(day, latitude.x, longitude.x, accumulatedPrecipitation.amount)
#colnames(weather_full) <- c("monthday", "lat", "long", "accPrecipLTN")


write.csv(manydata, file = paste0("C:/Data/R output/",outputfilename,".csv"))








#outputfilename <- "rainfall vs LTN Chart"  #this is the .csv tile - if unchanged it may overwrite an existing file
#year_start <- 2008
#year_end <- 2016     #LTN is defined as 2008-2016  (in 2018, change this to 2017)
#writeFile <- FALSE

#if (writeFile == TRUE) {
#  write.csv(weather_df, file = paste0("C:/Data/R output/",outputfilename,".csv"))
#} else {
#  cat("Caution: Weather dataset not written to disk")
#}
