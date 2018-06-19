# install / load required packages ----------------------------------------

# install aWhere R packages
#devtools::install_github("aWhereAPI/aWhere-R-Library")
#devtools::install_github("aWhereAPI/aWhere-R-ChartLibrary")

# load required packages
library(tidyverse) 
library(data.table)
library(ggmap)
library(ggplot2)
library(tidyr) 
library(dplyr)  
library(ggthemes) 
library(wicket) 
library(aWhereAPI)
library(aWhereCharts)

# define input paths and variables ----------------------------------------

# working directory - where input files are located and outputs will be saved
wd <- "~/Documents/aWhere/" 
setwd(wd)

# load external functions 
source("supporting_functions.R")

# specify the weather data directory and file name
weather.dir <- "climark_work_csvs/" 
weather.name <- "180609_past30.csv"

# define template data filename
template.file <- "CLIMARKonlyWardTemplate.csv"

# write histograms to image files 
write.hist = TRUE

# to select subarea(s) of interest, list their names in this vector.
# for now, these subareas are limited to ward names. To generate a forecast
# for the entire region instead, set subarea.select to ENTIRE_REGION. 
subarea.select <- "ENTIRE_REGION"
#subarea.select <- "KARARE" 
#subarea.select <- c("KARARE", "GOLBO")


# bins for tabular summaries of histogram data
# precipitation
bins.precip <- c(seq(from = 0, to = 300, by = 5), Inf)
# P/PET
bins.ppet <- c(0, 0.4, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.4, 1.6, 2.0, Inf)   



# processing steps ----------------------------------------------------------

# combine the directory and file name
weather.file <- paste(weather.dir, weather.name, sep="")

# read the weather data 
weather.df <- read.csv(weather.file)

# read the template data. remove columns that are not necessary.
template.df <- read.csv(template.file) %>% 
  dplyr::select( -c(shapewkt, longitude, latitude ))

# filter weather data for only the grid locations within the template data 
get.wards.area <- weather.df %>% 
  dplyr::filter(locationid %in% template.df$locationid)

# merge the weather data with and template data (wards/constituen)
weather.template.df <- merge(get.wards.area, 
                          template.df, by = "locationid")

# take a look at the combined data set
head(weather.template.df %>% 
       dplyr::select(locationid, latitude, longitude, CSUMPRE, CPOVRPR, WARD_NAME))

# construct output filename for weather + template data
weather.template.df.file <- paste("weather+template",
                                  weather.name,
                                  sep = "_")

# write the combined weather and template data to .csv file
write.csv(weather.template.df, 
          file = weather.template.df.file)


# filter the data set for subarea(s) of interest
# and write this clipped data set to file. It can become a template
# for a forecast. 
if (!identical(subarea.select, "ENTIRE_REGION")){ 
  
  weather.template.df <- weather.template.df %>% 
    dplyr::filter(WARD_NAME %in% subarea.select) 
  
  write.csv(weather.template.df, file = paste("weather+template_clip_",
                                              weather.name,
                                              sep = "_"))
  
} 

# take a look at the combined data set after the clip
head(weather.template.df %>% 
       select(locationid, latitude, longitude, CSUMPRE, CPOVRPR, WARD_NAME))



# calculate stats across subareas
subarea.stats <- weather.template.df %>%
  dplyr::group_by(WARD_NAME) %>% 
  dplyr::summarise(avg_CSUMPRE = mean(CSUMPRE),
                   max_CSUMPRE = max(CSUMPRE),
                   sd_CSUMPRE = sd(CSUMPRE),
                   avg_LTNsumPre = mean(LTNSUMP),
                   max_LTNsumPre = max(LTNSUMP),
                   sd_LTNsumPre = sd(LTNSUMP),
                   avg_D_CLTNSUMPRE = mean(DFLTSUM),
                   max_D_CLTNSUMPRE = max(DFLTSUM),
                   sd_D_CLTNSUMPRE = sd(DFLTSUM),
                   avg_CP_PET = mean(CPOVRPR),
                   max_CP_PET = max(CPOVRPR),
                   sd_CP_PET = sd(CPOVRPR),
                   avg_LTNP_PET = mean(LTNASPO),
                   max_LTNP_PET = max(LTNASPO),
                   sd_LTNPPET = sd(LTNASPO),
                   avg_D_CLTNP_PET = mean(DFLTPVP),
                   max_D_CLTNP_PET = max(DFLTPVP),
                   sd_D_CLTNP_PET = sd(DFLTPVP),
                   avg_CAvgMinT = mean(CAvgMinT),
                   max_CAvgMinT = max(CAvgMinT),
                   sd_CAvgMinT = sd(CAvgMinT),
                   avg_CAvgMaxT = mean(CAvgMaxT),
                   max_CAvgMaxT = max(CAvgMaxT),
                   sd_CAvgMaxT = sd(CAvgMaxT),
                   n_grids = n())

# calculate the stats across the entire region as a single entry in the table
# this serves as a summary across the entire region
region.stats <- weather.template.df %>%
  dplyr::summarise(avg_CSUMPRE = mean(CSUMPRE),
                   max_CSUMPRE = max(CSUMPRE),
                   sd_CSUMPRE = sd(CSUMPRE),
                   avg_LTNsumPre = mean(LTNSUMP),
                   max_LTNsumPre = max(LTNSUMP),
                   sd_LTNsumPre = sd(LTNSUMP),
                   avg_D_CLTNSUMPRE = mean(DFLTSUM),
                   max_D_CLTNSUMPRE = max(DFLTSUM),
                   sd_D_CLTNSUMPRE = sd(DFLTSUM),
                   avg_CP_PET = mean(CPOVRPR),
                   max_CP_PET = max(CPOVRPR),
                   sd_CP_PET = sd(CPOVRPR),
                   avg_LTNP_PET = mean(LTNASPO),
                   max_LTNP_PET = max(LTNASPO),
                   sd_LTNPPET = sd(LTNASPO),
                   avg_D_CLTNP_PET = mean(DFLTPVP),
                   max_D_CLTNP_PET = max(DFLTPVP),
                   sd_D_CLTNP_PET = sd(DFLTPVP),
                   avg_CAvgMinT = mean(CAvgMinT),
                   max_CAvgMinT = max(CAvgMinT),
                   sd_CAvgMinT = sd(CAvgMinT),
                   avg_CAvgMaxT = mean(CAvgMaxT),
                   max_CAvgMaxT = max(CAvgMaxT),
                   sd_CAvgMaxT = sd(CAvgMaxT),
                   n_grids = n()) %>% 
  dplyr::mutate(WARD_NAME = "ENTIRE REGION") %>%
  dplyr::select(WARD_NAME, n_grids, everything())


# combine the ward-specific stats with the overall region calculation
stats.out <- rbind(region.stats,
                   subarea.stats)

# take a look at the statistics data 
head(stats.out[,1:5], n = 10)

# write ward statistics to file 
write.csv(stats.out,
          paste("stats_by_subarea",
                weather.name,
                sep="_"))

# visualize data using the aWhereCharts::generateaWhereHistogram function

# Histogram of precip compared to LTN
# create a descriptive title that includes the current weather file name
hist.title <- paste("Histogram Precipitation", 
                    tools::file_path_sans_ext(weather.name), sep = " ")

aWhereCharts::generateaWhereHistogram(data = weather.template.df, 
                        variable = "CSUMPRE", 
                        title = hist.title, 
                        xlabel = "mm", 
                        compare = TRUE, 
                        compare_var = "LTNSUMP")

# write histogram to file 
if (write.hist == TRUE) {
  ggsave(paste0(filename = hist.title, ".png"),
       device = "png")
}


# Histogram of minT compared to LTN
hist.title <- paste("Histogram Min Temp", 
                    tools::file_path_sans_ext(weather.name), sep = " ")

generateaWhereHistogram(data = weather.template.df, 
                        variable = "CAvgMinT", 
                        title = hist.title, 
                        xlabel = "Deg C", 
                        compare = TRUE, 
                        compare_var = "LTAvgMnT")

# write histogram to file 
if (write.hist == TRUE) {
  ggsave(paste0(filename = hist.title, ".png"),
         device = "png")
}


# Histogram of maxT compared to LTN
hist.title <- paste("Histogram Max Temp", 
                    tools::file_path_sans_ext(weather.name), sep = " ")

generateaWhereHistogram(data = weather.template.df, 
                        variable = "CAvgMaxT", 
                        title = hist.title, 
                        xlabel = "Deg C", 
                        compare = TRUE, 
                        compare_var = "LTAvgMxT")

# write histogram to file 
if (write.hist == TRUE) {
  ggsave(paste0(filename = hist.title, ".png"),
         device = "png")
}


# Histogram of P/PET compared to LTN P/PET

# clip the extreme values of CPOVRPR and LTNASPO and #
# place these clipped values in new columns, "ctemp" and "LTNtemp"
weather.template.df$ctemp <- ClipValues(weather.template.df$CPOVRPR, 
                                        max.thresh = 2)
weather.template.df$LTNtemp <- ClipValues(weather.template.df$LTNASPO, 
                                        max.thresh = 2)

# use this separate histogram function for now to plot P/PET
source("function_generateaWhereHistogramPET.R")
hist.title <- paste("Histogram P div PET", 
                    tools::file_path_sans_ext(weather.name), sep = " ")
  
generateaWhereHistogramPET(data = weather.template.df, 
                           "ctemp", 
                           title = hist.title, 
                           xlabel = "P/PET", 
                           compare = TRUE, 
                           compare_var = "LTNtemp")

# write histogram to file 
if (write.hist == TRUE) {
  ggsave(paste0(filename = hist.title, ".png"),
         device = "png")
}



# tabular summaries of histogram data ---------------------------------------

# precipitation

# if the initial precipitation bins extend beyond the actual range of precip
# data, remove the extra bins
bins.precip <- bins.precip[bins.precip < (max(weather.template.df$CSUMPRE) + 5)] 

# create a column to populate with the numeric range per bin 
bin.range.precip <- vector(mode="character", length(bins.precip))

# add a column for which bin/ bin range each grid falls into
weather.template.df$bin.precip <- NA
weather.template.df$bin.range.precip <- NA

# loop through each bin and populate the appropriate values
for(b in 1:(length(bins.precip)-1)){
  
  # indices of entries that fall in the current bin
  idx <- weather.template.df$CSUMPRE >= bins.precip[b] & 
                   weather.template.df$CSUMPRE < bins.precip[b+1]
  
  # add the bin number to each row
  weather.template.df$bin.precip[idx] <- b
  
  # add the bin range to each row 
  weather.template.df$bin.range.precip[idx] <- paste(as.character(bins.precip[b]),
                                      " - ",
                                      as.character(bins.precip[b+1]),
                                      sep="")
}


# P/PET table summary ---------------------------------------------------

# take a look at the P/PET bins
bins.ppet

# add a column for which bin the grid falls into
weather.template.df$bin.ppet <- NA
weather.template.df$bin.range.ppet <- NA

# loop through each bin and populate the appropriate values
for(b in 1:(length(bins.ppet)-1)){
  
  # indices of entries that fall in the current bin
  idx <- weather.template.df$CPOVRPR >= bins.ppet[b] & 
    weather.template.df$CPOVRPR < bins.ppet[b+1]           
  
  # add the bin number to each row
  weather.template.df$bin.ppet[idx] <- b
  
  # add the bin range to each row 
  weather.template.df$bin.range.ppet[idx] <- paste(as.character(bins.ppet[b]),
                                           " - ",
                                           as.character(bins.ppet[b+1]),
                                           sep="")
}

# narrative ---------------------------------------------------------------

# To create a narrative about the percentage of a ward receiving 
# a given precip or p/pet level, we can subset the larger data frame 
# and see the distribution of grids cells at different precipitation / ppet levels
ward.select = "TURBI"
ward.select = "LOIYANGALANI"

ward.df <- weather.template.df %>% 
  #dplyr::filter(WARD_NAME %in% ward.select) %>%
  dplyr::group_by(bin.precip, 
                  WARD_NAME) %>% 
  dplyr::mutate(grid.count = n()) %>% 
  select(locationid, latitude, longitude, WARD_NAME, bin.range.precip, grid.count, everything()) %>%
  arrange(WARD_NAME, bin.range.precip)



ward.df <- weather.template.df %>% 
  dplyr::filter(WARD_NAME %in% ward.select) %>%  # filter for a single ward
  dplyr::mutate(ward.grids = n()) %>%
  dplyr::group_by(bin.precip, bin.range.precip) %>% # group all grids into their precip bin
  dplyr::mutate(grid.count = n(),
                grid.percent = 100 * n() / ward.grids) %>%
  tidyr::nest()
  









# mapping -----------------------------------------------------------------

# map the CLIMARK region

# specify lat/long coordinates. 38 and 2 are good for CLIMARK, zoom 7
lon.x = 38  # center of get_map longitude 
lat.y = 2.5   # center of get_map latitude 
base.map.climark = get_map(location = c(lon = lon.x, lat = lat.y), 
                           zoom = 7, 
                           color = "bw")

# display map of CLIMARK region
gg.map <- ggmap(base.map.climark)
gg.map


# Adjust the CLIMARK data extreme values

# convert CLIMARK data to data.table
dt2 <- as.data.table(weather.template.df)

# copy the CPOVRPR column to new column, cPovPET
dt2[,cPovPET := weather.template.df$CPOVRPR]
# for all P/PET values greater than 2, set equal to 2
dt2[cPovPET > 2.00, cPovPET := 2.00]

# copy the LTNASPO column to new column, cLTNPPET
dt2[,cLTNPPET := weather.template.df$LTNASPO]
# for all values > 2.49, set equal to 2.5
dt2[cLTNPPET > 2.49, cLTNPPET := 2.5]

# copy the CSUMPRE column to a new column, aPre
dt2[,aPre := weather.template.df$CSUMPRE]
# clip all values >299 to 300
dt2[aPre > 299, aPre := 300]

dt2[,aLTNPRE := weather.template.df$LTNSUMP]
dt2[aLTNPRE > 399, aLTNPRE := 400]

dt2[,aDinPre := weather.template.df$DFLTSUM]
dt2[aDinPre > 250, aDinPre := 250]
dt2[aDinPre < -250, aDinPre := -250]

# convert from data.table to data.frame
ggmap.df <- as.data.frame(dt2)

# Expand wkt to format usable by ggplot
polygon.df = as.tibble(wicket::wkt_coords(ggmap.df$shapewkt))
polygon.df$aPre <- ggmap.df$aPre[polygon.df$object]
polygon.df$cPovPET = ggmap.df$cPovPET[polygon.df$object]
polygon.df$aDinPre = ggmap.df$aDinPre[polygon.df$object]



climark_map <- function(df, v = "pre", climark.filename, base.map){
  # this function creates a climark map using CLIMARK data 
  # 
  # Args (input arguments to function)
  #
  #   df 
  #     Data frame ("df") containing CLIMARK data
  # 
  #   v 
  #     Character string specifying the variable to map. Default "pre".
  #     Acceptable values for "v" are: 
  #         "pre" = precipitation
  #         "ppet" = P / PET 
  #         "pltn" = recip vrs. LTN precip
  #
  #   climark.filename
  #     character string with the name of the CLIMARK file
  #     to label the plot accordingly. For example: 180609_past30.csv"
  #
  #   base.map
  #     terrain map image from Google Maps to overlay the mapped variable
  #
  # 
  
  
  # set the main title, legend titles, and other map parameters 
  # based on the variable 
  if(v=="pre") { # precipitation
    title.main <- paste0("Precipitation ", 
                         climark.filename)
    title.legend <- "Precipitation (mm)"
    fill.var <- "aPre"
    gradient.breaks <- seq(0,300, by = 50)
    c.low <- "red"
    c.mid <- "green"
    c.high <- "blue"
    mid.point <- 150
    
  } else if(v=="ppet") { # P / PET 
      title.main <- paste0("P/PET ", 
                      climark.filename) 
      title.legend <- "P/PET"
      fill.var <- "cPovPET"
      gradient.breaks <- seq(0,2.0, by = 0.2)
      c.low <- "red"
      c.mid <- "green"
      c.high <- "blue"
      mid.point <- 1.0
  
      } else if(v=="pltn") { # LTN Pre
        title.main <- paste0("Precip vrs. LTN Precip ", 
                         climark.filename)
        title.legend <- "Pre vrs LTN Pre (mm)"
        fill.var <- "aDinPre"
        gradient.breaks <- seq(-250,250, by = 50)
        c.low <- "red"
        c.mid <- "white"
        c.high <- "blue"
        mid.point <- 0
    
      } else {
        print("Unknown variable type provided. Please use 'pre', 'ppet', or 'pltn' ")
      }
  
  # print the titles to make sure they are correct
  print(paste0("Main title: ", title.main))
  print(paste0("Legend title: ", title.legend))

  # map the variable 
  climark.map = ggmap(base.map) +
    geom_polygon( aes( x = lng, 
                       y = lat, 
                       group = object, 
                       fill = get(fill.var)),
                  data = df, 
                  alpha = 0.7) +
    scale_fill_gradient2(breaks = gradient.breaks, 
                         low = c.low, 
                         mid = c.mid,
                         high = c.high, 
                         midpoint = mid.point, 
                         name = title.legend ) +
    ggtitle(title.main)
  climark.map
  
  # write the map to file 
  climark.map.filename <- paste0("report_countries_file_map_", 
                                 v, 
                                 ".png")
  ggsave(filename = climark.map.filename, 
         climark.map, 
         width = 6.02, 
         height = 3.38, 
         units = "in" )
  
  return(climark.map)
  
}


# call the climark_map function to generate each plot in just 1 line of code

# Precipitation map
climark_map(df = polygon.df, v = "pre", weather.name, base.map.climark)
# P/PET map
climark_map(df = polygon.df, v = "ppet", weather.name, base.map.climark)
# P LTN map
climark_map(df = polygon.df, v = "pltn", weather.name, base.map.climark)






# delete



# get a count of the number of grids per bin, % of grids belonging to bin.
# once population and yield data are incorporated,
# we can also add columns for those metrics.  
weather.template.precip.summary <- weather.template.df %>% 
  dplyr::group_by(bin.precip) %>%
  dplyr::mutate(grid.count = n(),
                grid.percent = 100 * n() / nrow(weather.template.df)) 


# write to.CSV - use "select" to remove the "data" column, 
# and write the remaining columns of data to a .csv file 
weather.template.precip.summary %>% 
  write.csv(file = paste("precip_summary_table",
                         weather.name,
                         sep=""))




