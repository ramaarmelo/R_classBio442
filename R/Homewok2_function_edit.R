#' @title Homework 
#' @description Transform coordinate from decimal minutes into decimal degrees 
#' @param Longitude The coordinate longitude to transform
#' @param Latitude The coordinate Latitude to transform
#' @param ... Provides opportunity use different argument to affect Longitude and Latitude
#' @keywords Longitude Latitude Transformation
#' @export
#' @examples 
#' homework_function <- function(Longitude = "Longitude", Latitude = "Latitude", mean = FALSE)

homework_function <- function(Longitude = "Longitude", Latitude = "Latitude", mean = FALSE){
  if (mean==TRUE){
    return(mean(Longitude))
  }
  
  gw$Date <- as.Date(gw$Date, format = "%d-%b-%y")
  
  long2 <- sapply(strsplit(gw$Longitude, "°"), "[[", 2)
  long2 <- gsub("'W", "", long2)
  long2 <- as.numeric(long2)
  long2 <- long2 / 60
  
  gw$long <- -77 - round(long2, 5)
  
  lat2 <- sapply(strsplit(gw$Latitude, "°"), "[[", 2)
  lat2 <- gsub("'N", "", lat2)
  lat2 <- as.numeric(lat2)
  lat2 <- lat2 / 60
  
  gw$lat <- 38 + round(lat2, 5)
  
  gw <- gw[, c("long", "lat", "Date", "Survey_Type")]
  names(gw)[1:2] <- c("Longitude", "Latitude") 
  return(gw)
  }
 



