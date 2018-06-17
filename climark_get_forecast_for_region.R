# load required packages 
library(dplyr)
library(ggmap)
library(data.table)
library(tibble)
library(aWhereAPI)
library(aWhereCharts)

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

start.time <- Sys.time()    

# loops through all location ID's in the template file
for (i in 1:nrow(template.place)){
  
  # get the current lat, lon, and location ID 
  lat <- template.place$latitude[i]
  lon <- template.place$longitude[i]
  loc.ID <- template.place$locationid[i]

  # clear/reset the forecast data frame
  forecast1 <- data.frame()    
  
  # pull forecast and LTN for the period year.start to year.end
  forecast1 <- generateaWhereDataset(lat = lat, 
                                 lon = lon, 
                                 day_start = day.start, 
                                 day_end = day.end, 
                                 year_start = year.start, 
                                 year_end = year.end)
  
  forecast1 = forecast1[-1,]  #this removes the first row - which is observed
  forecast1$locationid <- loc.ID

  # combine forecast data into a single data frame: forecast.all
  if(i==1) {
    forecast.all <- forecast1
  } else {
    forecast.all <- rbind(forecast1, forecast.all)
  }

}  # End of looping through template data

# check the elapsed time
end.time <- Sys.time()
run.time <- end.time - start.time
print(run.time)
    
write.csv(forecast.all, 
          file = paste0(filename.out,
                        ".csv"))

forecast.all <- read.csv(file = paste0(filename.out, 
                                       ".csv"), 
                         stringsAsFactors=FALSE)    


# n-day forecast summaries ------------------------------------

# Aggregate the 7-day forecasted total precipitation 
# (column "x" in the data frame created below)
day7.forecast <- aggregate(forecast.all$precipitation.amount, 
            by=list(locationid=forecast.all$locationid), 
            FUN=sum)

# find min/max dates within forecast 
max.date <- max(forecast.all$date)
min.date <- min(forecast.all$date)
target.date <- as.Date(min.date)+4

# filter forecast data to only contain "today" + 3 days
day3.forecast <- filter(forecast.all, 
                        forecast.all$date < target.date) 

day3.forecast <- aggregate(day3.forecast$precipitation.amount, 
                       by=list(locationid=day3.forecast$locationid), 
                       FUN=sum)

# Add shapewkt to full.7day.forecast and map it    
map.forecast.day7 <- merge(day7.forecast, template.place)
map.forecast.day3 <- merge(day3.forecast, template.place)


# Mapping the forecast summary ----------------------------------------------------

# specify lat/long coordinates. 38 and 2 are good for CLIMARK, zoom 7
lon.x = 38  # center of get_map longitude 
lat.y = 2.5   # center of get_map latitude 
base.map.climark = get_map(location = c(lon = lon.x, lat = lat.y), 
                           zoom = 7, 
                           color = "bw")

# display map of CLIMARK region
gg.map <- ggmap(base.map.climark)
gg.map

# ggmap method - specify which forecast (4-day or 7-day in this case) to map
ggmap.df <- map.forecast.4 

# convert to data table. clip sum precip values
# greater than 299 to 300. convert back to data frame. 
dt2 <- as.data.table(ggmap.df)
dt2[,aPre := ggmap.df$x]
dt2[aPre > 299, aPre := 300]
ggmap.df <- as.data.frame(dt2)

# Expand wkt to format usable by ggplot
polygon.df = as.tibble(wicket::wkt_coords(ggmap.df$shapewkt))
polygon.df$aPre <- ggmap.df$aPre[polygon.df$object]  

# Create precipitation map -------> use the function created in the climark_stats_hist script! 
climark_map(df = polygon.df, 
            v = "pre", 
            paste("Forecast", min.date, "to", max.date, sep = " "), 
            base.map.climark)





# manually creating plot --------------------------------------------------

# define the main and legend titles for the plot 
climark.map.title.precip <- paste0("Precipitation ", 
                         min.date,"  ", 
                         max.date) 
climark.map.var.precip <- "Precipitation (mm)"

map.precip = ggmap(base.map.climark) +
  geom_polygon( aes( x = lng, 
                     y = lat, 
                     group = object, 
                     fill = aPre),
                data = polygon.df, 
                alpha = 0.7) +
  scale_fill_gradient2(breaks = seq(0,300, by = 50), 
                       low = "red", 
                       mid = "green",
                       high = "blue", 
                       midpoint = 150, name=climark.map.var.precip ) +
  ggtitle(climark.map.title.precip)

map.precip

# save map to file 
ggsave(filename = paste0(climark.map.title.precip,
                         ".png"), 
       map.precip, 
       width = 6.02, 
       height = 3.38, 
       units = "in")


