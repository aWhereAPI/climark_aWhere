##inputs to function:
#data: (data frame) must be in format like that resulting from aWhere API query
#variable: (character string) Variable to chart - must be spelled exactly the same as in the data
#compare: (logical) Set to true to chart a second variable (compare_var)
#compare_var: (character string) Name of the second variable to chart
#xlabel: (character string) Label to give the the x axis of the chart (optional)
#title: (character string) Title to assign to the plot - default is the date span for the Current data (optional)
###dependencies: ggplot2, ggthemes, tidyr, zoo

list.of.packages = c("ggplot2", "ggthemes", "tidyr", "zoo")

new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## load necessary libraries
library(ggplot2)
library(ggthemes)
library(tidyr)
library(zoo)

#data <- subsetdata

#generate charts
generateaWhereHistogramPET <- function(data, variable, 
                                    compare = FALSE,
                                    compare_var = NULL,
                                    xlabel = NULL,
                                    title = NULL) {
  
  
    #if title is not given by user, set it to date range + variable
    if (is.null(title)) {
      title <- paste0("Aggregated aWhere Data - ", variable)
    }
  
    #filter out relevant data
  
    if (compare == TRUE) {
      chart_data <- data[, c("latitude", "longitude", variable, compare_var)]
      chart_data$place <- paste0(chart_data$latitude, ", ", chart_data$longitude)
      chart_data <- chart_data[, c("place", variable, compare_var)]
      chart_data <- setNames(chart_data, c("place", "Current", "LTN"))
    } else {
      chart_data <- data[, c("latitude", "longitude", variable)]
      chart_data$place <- paste0(chart_data$latitude, ", ", chart_data$longitude)
      chart_data <- chart_data[, c("place", variable)]
      chart_data <- setNames(chart_data, c("place", "Current"))
    }
  
  
 

#     for(k in 1:nrow(chart_data)) {
#          if(chart_data$measure[k] >= 1.3) {
#            chart_data$measure[k] <- 1.3  
#          } else {
#    chart_data$measure[k] <- chart_data$measure[k]
#          }
#      }  
    
  #set data format as long
    chart_data <- tidyr::gather(chart_data, 
                                key = Variable, 
                                value = measure, 
                                2:ncol(chart_data))
    
    #set common names of columns and xlabel (if not specified)
    if (is.null(xlabel)) {
      xlabel <- variable
    }

    
    # #if e_precip is set to true, bring in daily precip data and calculate accumulated
    # #daily precipitation using either default or user-defined threshold
    # 
    # if(e_precip == TRUE) {
    #   chart_data$Effective <- chart_data$Variable
    #   chart_data$Effective[chart_data$Effective > e_threshold ] <- e_threshold
    # }
    # 
    # 
    # #do row accumulations or averages, depending on variable
    # chart_data <- tidyr::gather(chart_data, 
    #                             key = Variable, 
    #                             value = measure, 
    #                             2:ncol(chart_data))
    # 
    # if (length(unique(chart_data$place)) > 1 & variable %in% c("maxTemp", "minTemp", "ppet")) {
    #   
    #   chart_data <- chart_data %>% group_by(place, Variable) %>% 
    #     summarise(mean(measure, na.rm = TRUE))
    # 
    #   } else if (length(unique(chart_data$place)) > 1 & variable %in% c("precip", "pet")) {
    #   
    #   chart_data <- chart_data %>% group_by(place, Variable) %>% 
    #     summarise(sum(measure, na.rm = TRUE))
    #   
    # }
    # 
    # names(chart_data)[3] <- "measure"
    
    #set color scale based on # of vars to chart
    if(length(unique(chart_data$Variable)) == 2) {
      colorScaleToUse <- scale_colour_manual(values = c("#1696AC", "#FF810E")) 
    } else {
      colorScaleToUse <- scale_colour_manual(values = c("#1696AC")) 
    } 
  
    if(length(unique(chart_data$Variable)) == 2) {
      colorFillToUse <- scale_fill_manual(values = c("#1F83B4", "#FF810E")) 
    } else {
      colorFillToUse <- scale_fill_manual(values = c("#1F83B4")) 
    } 
  
    #set x axis scale
    minXX <- 0
    maxXX <- 20 #max(chart_data$measure)*10
 
    
    xScale <- scale_x_continuous(breaks = seq(from = minXX, #(chart_data$measure), 
                                              to = maxXX, #(chart_data$measure),
                                              by = as.integer((as.integer(maxXX)  
                                                               - as.integer(minXX)))/100))
    
    #make chart

    chart <- ggplot() + 
      theme_igray() + 
      geom_histogram(data = chart_data, 
                     aes(measure,
                         col = Variable, 
                         fill = Variable),
                     position = 'identity',
                     bins = 40,
                     alpha = 0.4) +
      xScale +
      theme(legend.position="bottom", legend.direction="horizontal",
             legend.title = element_blank()) +
      labs(x=xlabel, y = "Count of Locations") +
      ggtitle(title)
    
    return(chart)
}
