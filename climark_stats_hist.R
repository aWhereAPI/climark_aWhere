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

# to select subarea(s) of interest, list their names in this vector.
# for now, these subareas are limited to ward names. To generate a forecast
# for the entire region instead, set subarea.select to ENTIRE_REGION. 
subarea.select <- "ENTIRE_REGION"
#subarea.select <- "KARARE" 
#subarea.select <- c("KARARE", "GOLBO")

# processing steps ----------------------------------------------------------

# combine the directory and file name
weather.file <- paste(weather.dir, weather.name, sep="")

# read the weather data 
weather.df <- read.csv(weather.file)

# read the template data. remove columns that are not necessary.
template.df <- read.csv(template.file) %>% 
  select( -c(shapewkt, longitude, latitude ))

# filter weather data for only the grid locations within the template data 
get.wards.area <- weather.df %>% 
  filter(locationid %in% template.df$locationid)

# merge the weather data with and template data (wards/constituen)
weather.template.df <- merge(get.wards.area, 
                          template.df, by = "locationid")

# construct output filename for weather + template data
weather.template.df.file <- paste("weather+template",
                               weather.name,
                               sep = "_")

# take a look at the combined data set
head(weather.template.df %>% 
       select(locationid, latitude, longitude, CSUMPRE, CPOVRPR, WARD_NAME))


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
  group_by(WARD_NAME) %>% 
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
      mutate(WARD_NAME = "ENTIRE REGION") %>%
      select(WARD_NAME, n_grids, everything())

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

# visualize data  --> which R package contains the aWhereHistogram functions? 
hist.title <- paste("CLIMARK area", 
                     weather.name,
                    sep = " ")

# Histogram of precip compared to LTN
generateaWhereHistogram(data = weather.template.df, 
                        variable = "CSUMPRE", 
                        title = paste("Histogram: Precipitation", 
                                      weather.name, sep = " "), 
                        xlabel = "mm", 
                        compare = TRUE, 
                        compare_var = "LTNSUMP")

# Histogram of minT compared to LTN
generateaWhereHistogram(data = weather.template.df, 
                        variable = "CAvgMinT", 
                        title = paste("Histogram: Min Temp", 
                                      weather.name, sep = " "), 
                        xlabel = "Deg C", 
                        compare = TRUE, 
                        compare_var = "LTAvgMnT")

#Histogram of maxT compared to LTN
generateaWhereHistogram(data = weather.template.df, 
                        variable = "CAvgMaxT", 
                        title = paste("Histogram: Max Temp", 
                                      weather.name, sep = " "), 
                        xlabel = "Deg C", 
                        compare = TRUE, 
                        compare_var = "LTAvgMxT")


# Histogram of P/PET compared to LTN P/PET

# clip the extreme values of CPOVRPR and LTNASPO and #
# place these clipped values in new columns, "ctemp" and "LTNtemp"
weather.template.df$ctemp <- ClipValues(weather.template.df$CPOVRPR, 
                                        max.thresh = 2)
weather.template.df$LTNtemp <- ClipValues(weather.template.df$LTNASPO, 
                                        max.thresh = 2)

# use this separate histogram function for now to plot P/PET
source("function_generateaWhereHistogramPET.R")
generateaWhereHistogramPET(data = weather.template.df, 
                           "ctemp", 
                           title = paste("Histogram: P / PET", 
                                         weather.name, sep = " "), 
                           xlabel = "P/PET", 
                           compare = TRUE, 
                           compare_var = "LTNtemp")












# Histogram of P/PET compared to LTN P/PET
# on subsetdata, convert to datatable, make 2 new col, histo on 
# those cols for P/PET vs LTN P/PET
dt1 <- as.data.table(weather.template.df) # copy weather.template.df into data table
dt1[,ctemp := weather.template.df$CPOVRPR] # make new column called "ctemp"
dt1[ctemp > 2, ctemp := 2] # subset
dt1[,LTNtemp := weather.template.df$LTNASPO] # make new column called "LTNtemp"
dt1[LTNtemp > 2, LTNtemp := 2] # subset
df1 <- as.data.frame(dt1)

# use this separate function just for now 
source("function_generateaWhereHistogramPET.R")
generateaWhereHistogramPET(data = df1, 
                           "ctemp", 
                           title = hist.title, 
                           xlabel = "P/PET", 
                           compare = TRUE, 
                           compare_var = "LTNtemp")

# Task 2 ------------------------------------------------------------------

######################## PRECIP
# Create a vector with counts of # grids for each hist bin 
bin.size <- 5 # increment size [units of mm for the precip data]
bins <- seq(from = min(weather.template.df$CSUMPRE),
            to = max(weather.template.df$CSUMPRE) + bin.size,
            by = bin.size)

# create a column to populate with the numeric range per bin 
bin_range <- vector(mode="character", length(bins))

# add a column for which bin/ bin range each grid falls into
weather.template.df$bin <- NA
weather.template.df$bin_range <- NA

# loop through each bin and populate the appropriate values
for(b in 1:(length(bins)-1)){
  
  # indices of entries that fall in the current bin
  idx <- weather.template.df$CSUMPRE >= bins[b] & 
                   weather.template.df$CSUMPRE < bins[b+1]
  
  # add the bin number to each row
  weather.template.df$bin[idx] <- b
  
  # add the bin range to each row 
  weather.template.df$bin_range[idx] <- paste(as.character(bins[b]),
                                      " - ",
                                      as.character(bins[b+1]),
                                      sep="")
}

# get a count of the number of grids per bin,
# percentage of grids belonging to bin.
# once population and yield data are incorporated,
# we can also add columns for those metrics 
weather.template.df <- weather.template.df %>% 
  group_by(bin) %>%
  mutate(grid_count = n(),
         grid_percent = 100 * n() / nrow(weather.template.df))
  

# create nested data frame with all observations per bin.
# display the bin number, grid count, percent of total grids
# within each bin. 
weather.template.df.nested <- weather.template.df %>% 
  group_by(bin, 
           bin_range,
           grid_count, 
           grid_percent) %>%
  nest() %>% 
  arrange(bin)

weather.template.df.nested

# write to.CSV - use "select" to remove the "data" column, 
# and write the remaining columns of data to a .csv file 
weather.template.df.nested %>% 
  select(-data) %>% 
  write.csv(file = paste("precip_summary_table",
                         weather.name,
                         sep=""))

# P/PET table ---------------------------------------------------

# bins for this could utilize evenly-spaced 0.1 increments
bins <- seq(from = 0,
            to = 2,
            by = 0.1)

# OR the bins could be based on ecological thresholds 
bins <- c(0, 0.4, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.4, 1.6, 2.0)    #added 0.7
  
# either way, add an additional bin to capture all values > 2
bins <- c(bins, Inf)

# create a column to populate with the numeric range per bin 
bin_range <- vector(mode="character", length(bins))

# add a column for which bin the grid falls into
weather.template.df$bin <- NA
weather.template.df$bin_range <- NA

# loop through each bin and populate the appropriate values
for(b in 1:(length(bins)-1)){
  
  # indices of entries that fall in the current bin
  idx <- weather.template.df$CPOVRPR >= bins[b] & 
    weather.template.df$CPOVRPR < bins[b+1]           #weather.template.df$CSUMPRE < bins[b+1]
  
  # add the bin number to each row
  weather.template.df$bin[idx] <- b
  
  # add the bin range to each row 
  weather.template.df$bin_range[idx] <- paste(as.character(bins[b]),
                                           " - ",
                                           as.character(bins[b+1]),
                                           sep="")
}

# get a count of the number of grids per bin,
# percentage of grids belonging to bin.
# once population and yield data are incorporated,
# we can also add columns for those metrics 
weather.template.df <- weather.template.df %>% 
  group_by(bin) %>%
  mutate(grid_count = n(),
         grid_percent = 100 * n() / nrow(weather.template.df))


# create nested data frame with all observations per bin.
# display the bin number, grid count, percent of total grids
# within each bin. 
weather.template.df.nested <- weather.template.df %>% 
  group_by(bin, 
           bin_range,
           grid_count, 
           grid_percent) %>%
  nest() %>% 
  arrange(bin)

weather.template.df.nested

# write to.CSV - use "select" to remove the "data" column, 
# and write the remaining columns of data to a .csv file 
weather.template.df.nested %>% 
  select(-data) %>% 
  write.csv(file = paste("P-PET_summary_table",
                         weather.name,
                         sep=""))

# narrative ---------------------------------------------------------------

# To create a narrative about the percentage of a ward receiving 
# a given precip level, we can use the dplyr::filter command to 
# subset the larger weather.template.df data frame, and then apply all of 
# the steps previously used to get overall statistics. I'd like to make 
# it into a function, but for now I'll paste that code below here. 
ward.select = "TURBI"
ward.select = "LOIYANGALANI"

ward.df <- weather.template.df %>% 
  filter(WARD_NAME %in% ward.select) 
ward.df$bin <- NA
ward.df$bin_range <- NA

# define P/PET bins 
bins <- c(0, 0.4, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.4, 1.6, 2.0)
# add an additional bin to capture all values > 2
bins <- c(bins, Inf)

for(b in 1:(length(bins)-1)){
  # indices of entries that fall in the current bin
  idx <- ward.df$CPOVRPR >= bins[b] & 
         ward.df$CPOVRPR < bins[b+1]
  # add the bin number to each row
  ward.df$bin[idx] <- b
  
  # add the bin range to each row 
  ward.df$bin_range[idx] <- paste(as.character(bins[b]),
                                                     " - ",
                                                     as.character(bins[b+1]),
                                                     sep="")
}

ward.df <- ward.df %>% 
  dplyr::group_by(bin) %>%
  dplyr::mutate(grid_count = n(),
         grid_percent = 100 * n() / nrow(ward.df))
# create nested data frame with all observations per bin.
# display the bin number, grid count, percent of total grids
# within each bin. 
ward.df.nested <- ward.df %>% 
  dplyr::group_by(bin, 
           bin_range,
           grid_count, 
           grid_percent) %>%
  nest() %>% 
  arrange(bin)

ward.df.nested  




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


