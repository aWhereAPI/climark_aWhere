# install / load required packages ----------------------------------------

# install aWhere R packages
#library(devtools)
#devtools::install_github("aWhereAPI/aWhere-R-Library")
#devtools::install_github("aWhereAPI/aWhere-R-Charts")

# load required packages
library(tidyverse) 
library(data.table)
library(ggmap)
library(ggplot2)
library(tidyr) 
library(dplyr)  
library(wicket) 
library(aWhereAPI)
library(aWhereCharts)

# define input paths and variables ----------------------------------------

# working directory - where input files are located and outputs will be saved
working.dir <- "~/Documents/aWhere/" 
setwd(working.dir)

# load external functions 
source("0-supporting_functions.R")

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
subarea.select <- "KARARE" 
subarea.select <- c("KARARE", "GOLBO")
subarea.select <- "ENTIRE_REGION"


# bins for tabular summaries of histogram data

# precipitation
bins.precip <- c(seq(from = 0, to = 300, by = 5), Inf)

# P/PET
bins.ppet <- c(0, 0.4, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.4, 1.6, 2.0, Inf)   


# base map location and zoom values for mapping the forecast data. 
map.lat <- 2.5
map.lon <- 38
map.zoom <- 7


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
       dplyr::select(locationid, latitude, longitude, CSUMPRE, 
                     CPOVRPR, WARD_NAME))

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
       dplyr::select(locationid, latitude, longitude, CSUMPRE, 
                     CPOVRPR, WARD_NAME))



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
  ggplot2::ggsave(paste0(filename = hist.title, ".png"),
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
  ggplot2::ggsave(paste0(filename = hist.title, ".png"),
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
  ggplot2::ggsave(paste0(filename = hist.title, ".png"),
         device = "png")
}


# Histogram of P/PET compared to LTN P/PET

# clip the extreme values of CPOVRPR (current P/PET) and LTNASPO 
# long-term average P/PET and place these clipped values in new columns,
# "ctemp" and "LTNtemp"
weather.template.df$ctemp <- ClipValues(weather.template.df$CPOVRPR, 
                                        max.thresh = 2)
weather.template.df$LTNtemp <- ClipValues(weather.template.df$LTNASPO, 
                                        max.thresh = 2)

# use this separate histogram function for now to plot P/PET
source("function_generateaWhereHistogramPET.R")

# construct a descriptive title
hist.title <- paste("Histogram PPET", 
                    tools::file_path_sans_ext(weather.name), sep = " ")
  
generateaWhereHistogramPET(data = weather.template.df, 
                           "ctemp", 
                           title = hist.title, 
                           xlabel = "P/PET", 
                           compare = TRUE, 
                           compare_var = "LTNtemp")

# write histogram to file 
if (write.hist == TRUE) {
  ggplot2::ggsave(paste0(filename = hist.title, ".png"),
         device = "png")
}



# tabular summaries of histogram data ---------------------------------------

# precipitation

# take a look at the bins
bins.precip

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

# add columns for number of grids per precip level
weather.template.df <- weather.template.df %>% 
  dplyr::group_by(bin.precip) %>%
  dplyr::mutate(grid.count.precip = n(),
         grid.percent.precip = 100 * n() / nrow(weather.template.df))

# nest the data within each bin and show a summary
weather.template.nested.precip <- weather.template.df %>% 
  dplyr::group_by(bin.precip, 
           bin.range.precip,
           grid.count.precip, 
           grid.percent.precip) %>%
  tidyr::nest() %>% 
  dplyr::arrange(bin.precip)

# take a look at the nested data. 
# this is essentially the histogram in tabular form.
# get the nested data out of the way,
grids.per.precip.level <- weather.template.nested.precip %>% 
  dplyr::select(-data)

# and use the "head" function to take a look at the first 6 rows
head(grids.per.precip.level)

# write number of grids per precip level to .csv
write.csv(grids.per.precip.level,
          "grids_per_precip_level.csv")


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

# add columns for number of grids per ppet level
weather.template.df <- weather.template.df %>% 
  dplyr::group_by(bin.ppet) %>%
  dplyr::mutate(grid.count.ppet = n(),
         grid.percent.ppet = 100 * n() / nrow(weather.template.df))

weather.template.nested.ppet <- weather.template.df %>% 
  dplyr::group_by(bin.ppet, 
           bin.range.ppet,
           grid.count.ppet, 
           grid.percent.ppet) %>%
  tidyr::nest() %>% 
  dplyr::arrange(bin.ppet)

# take a look at the nested data
# this is essentially the P/PET histogram in tabular form.
# get the nested data out of the way,
grids.per.ppet.level <- weather.template.nested.ppet %>% 
                           dplyr::select(-data)

# and use the "head" function to take a look at the first 6 rows
head(grids.per.ppet.level)

# write number of grids per ppet level to .csv
write.csv(grids.per.ppet.level,
          "grids_per_ppet_level.csv")




# ward narrative --------------------------------------------------------------

# To create a narrative about the percentage of a ward receiving 
# a given precip or p/pet level, we can subset the larger data frame 
# and see the distribution of grids cells at different precipitation / ppet levels
ward.select = "TURBI"
ward.select = "KARARE"

# filter the data for the ward of interest
ward.df <- weather.template.df %>% 
  dplyr::filter(WARD_NAME %in% ward.select) 

# count total number of grids in ward 
ward.grid.count <- nrow(ward.df)

ward.df <- ward.df %>%
  dplyr::group_by(bin.precip) %>% 
  dplyr::mutate(bin.grid.count.ward = n(),
                bin.grid.percent.ward = 100 * (bin.grid.count.ward / 
                                                 ward.grid.count)) %>%
  dplyr::group_by(bin.precip,
                  bin.range.precip,
                  bin.grid.count.ward,
                  bin.grid.percent.ward) %>%
  tidyr::nest() %>% 
    dplyr::arrange(bin.precip)

# take a look at the grid count and percentages per precip level
head(ward.df %>% dplyr::select(-data))


# mapping -----------------------------------------------------------------

# create the base map using the parameters defined earlier
base.map = ggmap::get_map(location = c(lon = map.lon, 
                                lat = map.lat), 
                   zoom = map.zoom, 
                   color = "bw")

# display map of region
gg.map <- ggmap(base.map)
gg.map


# clip the extreme values of selected variables to map
weather.template.df$cPovPET <- ClipValues(weather.template.df$CPOVRPR, 
                                       max.thresh = 2)
weather.template.df$cLTNPPET <- ClipValues(weather.template.df$LTNASPO, 
                                          max.thresh = 2)
weather.template.df$aPre <- ClipValues(weather.template.df$CSUMPRE, 
                                           max.thresh = 300)
weather.template.df$aLTNPRE <- ClipValues(weather.template.df$LTNSUMP, 
                                       max.thresh = 400)
weather.template.df$aDinPre <- ClipValues(weather.template.df$DFLTSUM, 
                                          max.thresh = 250,
                                          min.thresh = -250)

ggmap.df <- weather.template.df

# Expand wkt to format usable by ggplot
polygon.df = as.tibble(wicket::wkt_coords(ggmap.df$shapewkt))
polygon.df$aPre <- ggmap.df$aPre[polygon.df$object]
polygon.df$cPovPET = ggmap.df$cPovPET[polygon.df$object]
polygon.df$aDinPre = ggmap.df$aDinPre[polygon.df$object]

# call the MakeMap function from the supporting_functions script to create a map
# for each of the specified variables

# Precipitation map
MakeMap(df = polygon.df, v = "pre", base.map, 
        tools::file_path_sans_ext(weather.name), write.file = TRUE)

# P/PET map
MakeMap(df = polygon.df, v = "ppet", base.map, 
        tools::file_path_sans_ext(weather.name), write.file = TRUE)

# P LTN map
MakeMap(df = polygon.df, v = "pltn", base.map, 
        tools::file_path_sans_ext(weather.name), write.file = TRUE)

