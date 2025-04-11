library(tidyverse)
library(leaflet)
library(leaflet.minicharts)

# look for csv ----
csvs <- list.files("./csv", pattern = ".csv", full.names = T)

# read wardrive log ----
# We need to do some tidying due to some people having a comma
# in the name of their AP

read_log <- function(file) {
  # get raw stream 
  raw <- readLines(file)
  
  # get header
  walk <- read.csv(text = paste(raw[[2]], raw[[3]], sep = "\n"))
  walk$SSID <- as.character(walk$SSID)
  
  # iterate over the remaining rows, if we find a higher than
  # expected number of columns, we paste together the SSID column
  # (column 2) and the following column (column 3)
  for(i in 3:length(raw)) {
    temp_row <- read.csv(text = raw[[i]], header = F)
    
    if(ncol(temp_row) == 15) {
      temp_row[2] <- paste0(temp_row[2], temp_row[3])
      temp_row <- select(temp_row, -V3)
      
    }
    
    names(temp_row) <- names(walk)
    
    # coerce SSID to character
    temp_row$SSID <- as.character(temp_row$SSID)
    
    walk <- bind_rows(walk, temp_row)
    
  }
  
  
  # clean raw file further ----
  colnames(walk)[colnames(walk) == "CurrentLatitude"]  <- "lat"
  colnames(walk)[colnames(walk) == "CurrentLongitude"] <- "lng"
  
  walk$lat <- sub("\\.", ",", walk$lat)
  walk$lat <- gsub("\\.", "", walk$lat)
  walk$lat <- as.numeric(sub("\\,", ".", walk$lat))
  
  walk$lng <- sub("\\.", ",", walk$lng)
  walk$lng <- gsub("\\.", "", walk$lng)
  walk$lng <- as.numeric(sub("\\,", ".", walk$lng))
  
  palette <- data.frame(
    AuthMode = c("[OPEN]", "[WEP]", "[WPA_WPA2_PSK]", "[WPA2_PSK]", "[WPA2_ENTERPRISE]", "[WPA2_WPA3_PSK]"),
    color    = c("red",    "orange", "yellow",        "green",         "green",           "blue")
  )
  
  walk <- left_join(walk, palette, by = "AuthMode")
  walk$color[walk$color == NA] <- "purple"
  
  
  return(walk)
}

plotdata <- data.frame()

for(filename in seq_along(csvs)) {
  plotdata <- bind_rows(plotdata, read_log(csvs[filename]))
}

# optionally filter ----
plotdata <- filter(plotdata, AuthMode == "[OPEN]")


# Generate map ----
leaflet() %>%
addProviderTiles(providers$CartoDB.Positron) %>%
setView(lng = 4.35, lat = 52.08, zoom = 14) %>%
addMarkers(data = plotdata, 
  group = "Markers", 
  popup = paste("<h3>", plotdata$SSID, "</h3></br>Channel:", plotdata$Channel ,"</br>Security:" , plotdata$AuthMode))