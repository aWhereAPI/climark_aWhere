GetDays <- function(starting.day, forecast.days, ending.day=FALSE){
  # Generate the starting and ending days for a forecast. 
  #
  # Args: 
  #   starting.day
  #     (string) the first day of the forecast. Acceptable values are "today",
  #     "yesterday", "tomorrow", or a specific date with format "YYYY-MM-DD"
  #
  #   forecast.days 
  #     (vector of numeric value(s)) duration of the forecast(s).
  #     For a 7- and 3-day forecast: c(7,3). 
  #
  #   ending.day
  #     (optional, string with format "YYYY-MM-DD") if supplied, this will be
  #     the last day of the forecast. Otherwise, the ending day is calculated
  #     by adding the greatest value in forecast.days to the starting date.
  #
  # Returns
  #
  #   days
  #     (list of characters) starting day in position 1; ending day in position 2
  #
  
  # get starting day from input string
  if (base::identical(starting.day,"today")) {
    
    day.start <- base::as.character(Sys.Date())
    
  } else if (base::identical(starting.day, "yesterday")) { 
    
    day.start <- base::as.character(Sys.Date() - 1)
    
  } else if (base::identical(starting.day, "tomorrow")) { 
    
    day.start <- base::as.character(Sys.Date() + 1)
    
    } else { # date is already specific YYYY-MM-DD 
    
    day.start <- starting.day
    
    }
  
  if (ending.day!=FALSE) { # if specific end day is defined
    
    day.end <- ending.day
    
  } else { # calculate ending day, starting day + largest forecast duration
    
    day.end <- base::as.character(base::as.Date(day.start) + 
                                  base::max(forecast.days))
  
  }
  
  # check if the forecast duration is greater than 7 days 
  if (base::as.Date(day.end) - base::as.Date(day.start) > 7) {
    print("Forecast duration cannot exceed 7 days.")
  }
  
  # combine starting and ending days into single variable
  days <- base::c(day.start, day.end)
  
  return(days)
  
  
}




GetForecastData <- function(template.place, days, years, 
                        write.file = FALSE, filename.out = "forecast"){
  # pulls forecast data from the aWhere API for the specified days/years.
  #
  # Args
  #   template.place 
  #     (data.frame) contains the geographic data for each grid cell
  #
  #   days
  #     (list of characters) starting day in position 1; ending day in position 2
  #     each with the format "YYYY-MM-DD"
  #
  #   years 
  #     (list of numerics) starting year in position 1; ending year in position 2
  #     used to calculate long term values
  #
  #   write.file
  #     (optional, boolean with default value FALSE) when true, the forecast
  #     data is written to a .csv file 
  #
  #   filename.out
  #     (optional, character string with default value "forecast") used to name
  #     the output file 
  #
  # Returns 
  #   forecast.all
  #     (data.frame) containing all of the n-day forecast data.
  # 
  
  # start timer to record how long pulling forecast data takes
  start.time <- base::Sys.time() 
  
  # loops through all location ID's in the template file
  for (i in 1:(base::nrow(template.place))){
    
    # print progress for the user
    base::print(base::paste("Getting forecast", 
                      base::as.character(i), "/", 
                      base::as.character(base::nrow(template.place)), 
                      sep = " "))
    
    # get the current lat, lon, and location ID 
    lat <- template.place$latitude[i]
    lon <- template.place$longitude[i]
    loc.ID <- template.place$locationid[i]
    
    # pull forecast and LTN for the specified years and date range
    forecast1 <- aWhereCharts::generateaWhereDataset(lat = lat, 
                                                     lon = lon, 
                                                     day_start = days[1], 
                                                     day_end = days[2], 
                                                     year_start = years[1], 
                                                     year_end = years[2])
    
    # this removes the first row - which is observed
    forecast1 = forecast1[-1,]  
    
    # assign location ID to the forecast data entries
    forecast1$locationid <- loc.ID
    
    # combine forecast data into a single data frame: forecast.all
    if(i==1) {
      forecast.all <- forecast1
    } else {
      forecast.all <- base::rbind(forecast1, forecast.all)
    }
    
  }  
  
  # check the elapsed time
  end.time <- base::Sys.time()
  run.time <- end.time - start.time
  base::print(run.time)
  
  # write the forecast data to .csv 
  if (write.file == TRUE){
    
    # create the output filename using the function argument along with
    # the days and years of the forecast
    utils::write.csv(forecast.all, 
            file = base::paste0(base::paste(filename.out,
                          base::paste(days, collapse="_"),
                          base::paste(years, collapse="_"),
                          sep="_"),".csv"))
  }
  
  
  return(forecast.all)
  
}



GetForecastSummary <- function(forecast.all, n.day.forecasts,
                               template.place){
  # Sum the n-day forecast values at every location.
  #
  # Args: 
  #   forecast.all
  #     (data.frame) containing all of the n-day forecast data.
  #
  #   n.day.forecasts
  #     (vector of numeric value(s)) duration of the forecast(s).
  #     For a 7- and 3-day forecast: c(7,3).
  #
  #   template.place
  #     (data.frame) contains the geographic data for each grid cell
  #
  
  # loop through the requested n-day forecast summaries 
  for (i in 1:(base::length(n.day.forecasts))){

    # get the current forecast duration 
    n <- n.day.forecasts[i]
    
    # calculate the end date of the forecast
    min.date <- base::min(forecast.all$date)
    target.date <- base::as.Date(min.date) + n
    
    # remove forecast data beyond the n-day duration.
    # for each location ID, calculate cumulative forecasted precipitation
    # and cumulative average precipitation over the long term.
    # add a column for which n-day forecast these values are for.
    forecast.summary <- forecast.all %>% 
      dplyr::filter(date < target.date) %>% 
      dplyr::group_by(locationid) %>% 
      dplyr::summarise(precip.amount.sum = sum(precipitation.amount),
                       precip.avg.sum = sum(precipitation.average)) %>%
      dplyr::mutate(n.day = n)
    
    # combine forecast summary data into a single data frame
    if(i==1) {
      forecast.summaries <- forecast.summary
    } else {
      forecast.summaries <- base::rbind(forecast.summary, forecast.summaries)
    }
    
    # add geometry data to each location for mapping 
    forecast.summaries.geo <- base::merge(forecast.summaries,
                                          template.place)
    
  }
  
  return(forecast.summaries.geo)
  
}




ClipValues <- function(values, max.thresh, min.thresh = 0){
  # Clips the extremely large or small values of an input vector. 
  #
  # Args: 
  #   values
  #     (vector) input variables to clip.
  #
  #   max.thresh
  #     (numeric) values above this value will be assigned to this value.
  #   
  #   min.thresh
  #     (numeric) values below this value will be assigned to this value.
  #
  # Returns: 
  #   values
  #     input vector, altered to have new minimum/maximum values. 
  #
  
  values[values < min.thresh] <- min.thresh
  values[values > max.thresh] <- max.thresh

  return(values)
    
}


MapForecast <- function(forecasts.n, base.map, thresholds){
  
  forecast.maps <- list()
  
  n.days <- unique(forecasts.n$n.day)
  
  # loop through the requested n-day forecast summaries 
  for (i in 1:(base::length(n.days))){
    
    # get the current forecast duration 
    n <- n.days[i]
    print(n)
    
    # filter forecast summary data for the n-day forecast
    # add a new column of "clipped" data based on the threshold provided
    forecast <- forecasts.n %>% 
      dplyr::filter(n.day == n) %>% 
      dplyr::mutate(aPre = ClipValues(precip.amount.sum,
                                      max.thresh = thresholds$precip[2]))
    
    # add geometry information for mapping with ggplot
    polygon.df = tibble::as.tibble(wicket::wkt_coords(forecast$shapewkt))
    polygon.df$aPre <- forecast$aPre[polygon.df$object] 
    
    # add n-day forecast to output filename
    filename.out.current <- base::paste0(filename.out, "_",
                                base::as.character(n), "-day_forecast")
    
    # create the map, save to file 
    forecast.map <- MakeMap(df = polygon.df, v = "pre", 
                    map.name = filename.out.current, base.map = base.map,
                    write.file = TRUE)

    forecast.maps[[i]] <- forecast.map
    
  } 
  
  # give a label to each of the list entries so the user can access 
  # the plot data easily
  names(forecast.maps) <- c(paste0(n.days,"-day"))
  
  return(forecast.maps)
  
}

MakeMap <- function(df, v = "pre", base.map, map.name,
                    write.file = FALSE){
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
  #   base.map
  #     terrain map image from Google Maps to overlay the mapped variable
  #
  #   map.name
  #     character string to name the map and output accordingly
  #
  #   write.file 
  #     (optional, boolean with default value FALSE) when true, the forecast
  #     data is written to a .csv file
  #
  
  
  # set the main title, legend titles, and other map parameters 
  # based on the variable 
  if(v=="pre") { # precipitation
    title.main <- paste0("Precipitation ", 
                         map.name)
    title.legend <- "Precipitation (mm)"
    fill.var <- "aPre"
    gradient.breaks <- base::seq(0,300, by = 50)
    c.low <- "red"
    c.mid <- "green"
    c.high <- "blue"
    mid.point <- 150
    
  } else if(v=="ppet") { # P / PET 
    title.main <- paste0("P/PET ", 
                         map.name) 
    title.legend <- "P/PET"
    fill.var <- "cPovPET"
    gradient.breaks <- base::seq(0,2.0, by = 0.2)
    c.low <- "red"
    c.mid <- "green"
    c.high <- "blue"
    mid.point <- 1.0
    
  } else if(v=="pltn") { # LTN Pre
    title.main <- paste0("Precip vrs. LTN Precip ", 
                         map.name)
    title.legend <- "Pre vrs LTN Pre (mm)"
    fill.var <- "aDinPre"
    gradient.breaks <- base::seq(-250,250, by = 50)
    c.low <- "red"
    c.mid <- "white"
    c.high <- "blue"
    mid.point <- 0
    
  } else {
    print("Unknown variable type provided. Please use 'pre', 'ppet', or 'pltn' ")
  }
  
  # print the titles to make sure they are correct
  print(base::paste0("Main title: ", title.main))
  print(base::paste0("Legend title: ", title.legend))
  
  # map the variable 
  climark.map = ggmap::ggmap(base.map) +
    ggplot2::geom_polygon( aes( x = lng, 
                       y = lat, 
                       group = object, 
                       fill = get(fill.var)),
                  data = df, 
                  alpha = 0.7) +
    ggplot2::scale_fill_gradient2(breaks = gradient.breaks, 
                         low = c.low, 
                         mid = c.mid,
                         high = c.high, 
                         midpoint = mid.point,
                         name = title.legend ) +
    ggplot2::ggtitle(title.main)
  climark.map
  
  if (write.file == TRUE){ 
    # write the map to file 
    map.name.out <- base::paste0(paste(map.name, v, sep = "_"),
                           ".png")
    ggplot2::ggsave(filename = map.name.out, 
           climark.map, 
           width = 6.02, 
           height = 3.38, 
           units = "in" )
  }
  
  return(climark.map)
  
}


# WriteJpeg ---------------------------------------------------------------

WriteJpeg <- function(plt, plt.title, w=10, h=6, u="in", r=500){
  # Opens the jpeg graphics device with the specified settings
  # and saves the plot to an image file (.jpeg)
  #
  # Args
  #   plt
  #     (ggplot plot object) plot to write to file
  #
  #   plt.title
  #     (character string) title of the plot, used to name the output file
  #
  #   w 
  #     (integer, optional with default value of 10) width of graphics device
  #
  #   h
  #     (integer, optional with default value of 6) height of graphics device 
  #
  #   u
  #     (character, optional with default value of "in") the units in which 
  #     height and weight are given. Can be "in", "px", "cm", or "mm".
  #
  #   r
  #     (integer, optional with default value of 500) nominal resolution in ppi
  
  # set graphics device
  jpeg(paste0(plt.title, ".jpeg"), 
       width = w, 
       height = h, 
       units = u, 
       res = r)
  
  # write plot to image file
  print(plt)
  
  # close graphics device 
  dev.off()
  
}
