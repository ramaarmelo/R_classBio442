#' @title Homework 
#' @description Transform coordinate from decimal minutes into decimal degrees 
#' @param Longitude The coordinate longitude to transform
#' @param Latitude The coordinate Latitude to transform
#' @param ... Provides opportunity use different argument to affect Longitude and Latitude
#' @keywords Longitude Latitude Transformation decimal degree minute
#' @export
#' @examples 
#' homework_function()

homework_function <- function(Longitude, Latitude, ...) {
  gw_data$Date <- as.Date(gw_data$Date, format = "%d-%b-%y")
  gw_data$Latitude <- gsub("'N", "", gw_data$Latitude)
  gw_data$Longitude <- gsub("'W", "", gw_data$Longitude)
  
  data2 <- gsub("°", "", gw_data$Latitude) 
  gw_data$Latitude <- gsub("°", "", gw_data$Latitude)
  
  data3 <- gsub("°", "", gw_data$Longitude) 
  gw_data$Longitude <- gsub("°", "", gw_data$Longitude)
  
  gw_data$Longitude<-as.numeric(gw_data$Longitude)
  gw_data$Latitude<-as.numeric(gw_data$Latitude) 
  
  library(tidyverse)
  
  data4 <- separate(gw_data,
                    col = Longitude,
                    into = c("minute", "second"),
                    convert = TRUE)
  
  data5 <- separate(data4,
                    col = Latitude,
                    into = c("minute1", "second1"),
                    convert = TRUE)
  
  data5$second <- data5$second/60
  data5$second1 <- data5$second1/60
  data5$Longitude<-rowSums(data5[, 2:3])
  data5$Latitude<-rowSums(data5[, 4:5]) 
  
  data6 <- data5[, ! names(data5) %in% c("minute", "second", "minute1", "second1")] 
  str(data6)
  
  data7<-data6[, c("Longitude", "Latitude", "Date")]
  gw_data <-data7
  
  return(gw_data)
}



