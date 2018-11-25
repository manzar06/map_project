library(leaflet)
library(rgdal)
library(dplyr)
library(ggmap)
library(htmltools)
library(mapview)
library(htmlwidgets)
library(googleway)
library(xlsx)
#library(RCurl)
#library(RJSONIO)

 

key <- 'AIzaSyBaSO25t_xcCSUj8aR7bZopQL5xABL051Q'




  


m <- leaflet()
m
m <- leaflet() %>%
  addTiles() %>%
  setView(lng = -153.369141, lat=66.160507, zoom = 4)
m

ak_counties <- readOGR("data/tl_2013_02_cousub.shx")
ak_counties

m <- leaflet() %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  setView(lng = -153.369141, lat=66.160507, zoom = 4) %>%
  addPolygons(data = ak_counties, color = "#660000", weight = 1, smoothFactor = 0.5)
  
m



fbi_data <- read.csv("database.csv")
fbi_data
ak <- filter(fbi_data, State == "Alaska")
ak
#write.csv(ak, "ak.csv")

#missing lat and lon.  We can use geocode to get them, but first need a more
#complete address.
#k<-c("Bhopal")# Alaska United States)
#k
#google_geocode(address = "bhopal", key = "AIzaSyBaSO25t_xcCSUj8aR7bZopQL5xABL051Q")
#key<-"AIzaSyCdCfWLRX0fhHpnByraPEj6GXgfW1ZOR3g"
#ak <- mutate(ak, address = paste(City, State, "United States"))
ak <- mutate(ak, address = paste(City, "United States"))
#ak <- mutate(ak, address = paste(City))
addresses <- unique(ak$address)
#addresses
#key="AIzaSyBaSO25t_xcCSUj8aR7bZopQL5xABL051Q"
#set_key(key = key)
#addresses2<-paste(addresses,collapse=", ")









dt <- data.frame(address = addresses,
                 stringsAsFactors = FALSE)

key <- 'AIzaSyBaSO25t_xcCSUj8aR7bZopQL5xABL051Q'

res <- apply(dt, 1, function(x){
  google_geocode(address = x[['address']],
                 key = key)
})

str(res)
coords <- lapply(res, function(x){
  x$results$geometry$location
})

coords <- lapply(seq_along(res), function(x){
  coords <- res[[x]]$results$geometry$location
  address <- dt[x, 'address']
  res_df <- data.frame(lat = coords[, 'lat'],
                       lon = coords[, 'lng'], 
                       address = address
  )
})

df_coords <- do.call(rbind, coords)
df_coords










#geocode<- google_geocode(address = "addresses",
 #                    key = key,
  #                   simplify = TRUE)
#geocode_coordinates(geocode)

#geocode <- google_geocode(address = "addresses", key = "AIzaSyBaSO25t_xcCSUj8aR7bZopQL5xABL051Q")
#google_keys()
#geocodes <- geocode(k)
#distinctss<-distinct(ak, group_by="city" )
#distinctss
#rm("geocodes")
#library(readxl)
#options(digits=20)
#geocodes <- read_excel("geocode2.xlsx")
#View(geocodes)
#geocodes
#library(readr)
#geocode3 <- read_csv("geocode3.csv", col_types = cols(X3 = col_skip(), 
                                                      #X4 = col_skip()))
#View(geocode3)
#require(reshape2)
#combine geocodes with address and join to ak data
addresses_and_coords <- data.frame(address = addresses, 
                                   lon = df_coords$lon,
                                   lat = df_coords$lat)
#use a loop to find missing values
counter <- 0
while (sum(is.na(addresses_and_coords$lon)) > 0 && counter < 10) {
  
  #filter for missing address
  missing_addresses <- addresses_and_coords %>%
    filter(is.na(lon)==TRUE)
  
  #search again
  addresses <- missing_addresses$address
  #geocodes <- geocode(as.character(addresses), source = "google")
  
  #remove missing addresess from data frame
  addresses_and_coords <- addresses_and_coords %>%
    filter(is.na(lon)==FALSE)
  
  #create a new data frame with the new addresses
  new_addresses <- data.frame(address = addresses, 
                              lon = df_coords$lon,
                              lat = df_coords$lat)
  
 
  
  #combine the new and missing data
  addresses_and_coords <- rbind(addresses_and_coords, new_addresses)
  counter <- counter + 1 
}
 
  rm(df_coords)
  rm(addresses)
  #rm(missing_addresses)
  #rm(new_addresses)
  #rm(counter)
  #add lon and lat to ak data
  ak <- left_join(ak, addresses_and_coords, by = "address")
  #abslat<-abs(ak_unsolved$lat)
  ak_unsolved <- ak %>%
    filter(Crime.Solved == "No") %>%
    filter(Crime.Type == "Murder or Manslaughter")
  m <- leaflet() %>%
    setView(lng = -149.4937, lat = 64.2008, zoom = 3) %>%
    addProviderTiles(providers$Stamen.Toner) %>%
    addPolygons(data = ak_counties,
                color = "#660000", 
                weight = 1, 
                smoothFactor = 0.5) %>%
    addCircleMarkers(lng = ak_unsolved$lon,lat=ak_unsolved$lat)
  m

  
  ak$lon <- jitter(ak$lon, factor = 1)
  ak$lat <- jitter(ak$lat, factor = 1)
  
  
  m <- leaflet() %>%
    setView(lng = -149.4937, lat = 64.2008, zoom = 3) %>%
    addProviderTiles(providers$Stamen.Toner) %>%
    addPolygons(data = ak_counties,
                color = "#660000", 
                weight = 1, 
                smoothFactor = 0.5) %>%
    addCircleMarkers(lng = ak_unsolved$lon, 
                     lat = ak_unsolved$lat,
                     color = "ffffff",
                     weight = 1,
                     radius = 5,
                     label = ak_unsolved$Year)
  m
  
  #note that the label appears blank. this is because Year is an integer, needs to
  #be text.
  
  m <- leaflet() %>%
    setView(lng = -149.4937, lat = 64.2008, zoom = 3) %>%
    addProviderTiles(providers$Stamen.Toner) %>%
    addPolygons(data = ak_counties,
                color = "#660000", 
                weight = 1, 
                smoothFactor = 0.5) %>%
    addCircleMarkers(lng = ak_unsolved$lon, 
                     lat = ak_unsolved$lat,
                     color = "ffffff",
                     weight = 1,
                     radius = 5,
                     label = as.character(ak_unsolved$Year))
  m
  
  
  
  #create character vectors in HTML that contain what we want to show in the
  #labels
  
  ak_unsolved$label <- paste("<p>", ak_unsolved$City, "</p>",
                             "<p>", ak_unsolved$Month, " ", ak_unsolved$Year, "</p>",
                             "<p>", ak_unsolved$Victim.Sex, ", ", ak_unsolved$Victim.Age, "</p>",
                             "<p>", ak_unsolved$Victim.Race, "</p>",
                             "<p>", ak_unsolved$Weapon, "</p>",
                             sep="")
  
  
  #map has HTML label but it reads it as plain text.  Need to treat text like HTML code.
  m <- leaflet() %>%
    setView(lng = -149.4937, lat = 64.2008, zoom = 3) %>%
    addProviderTiles(providers$Stamen.Toner) %>%
    addPolygons(data = ak_counties,
                color = "#660000", 
                weight = 1, 
                smoothFactor = 0.5) %>%
    addCircleMarkers(lng = ak_unsolved$lon, 
                     lat = ak_unsolved$lat,
                     color = "gold",
                     weight = 1,
                     radius = 5, 
                     opacity = 0.75,
                     label = ak_unsolved$label)
  m
  
  
  #map has HTML label but it reads it as plain text.  Need to treat text like HTML
  #code.  To do this, we use the HTML function from the htmltools package.
  m <- leaflet() %>%
    setView(lng = -149.4937, lat = 64.2008, zoom = 3) %>%
    addProviderTiles(providers$Stamen.Toner) %>%
    addPolygons(data = ak_counties,
                color = "#660000", 
                weight = 1, 
                smoothFactor = 0.5) %>%
    addCircleMarkers(lng = ak_unsolved$lon, 
                     lat = ak_unsolved$lat,
                     color = "gold",
                     weight = 1,
                     radius = 5, 
                     opacity = 0.75,
                     label = lapply(ak_unsolved$label, HTML))
  m
  
  #add clustering
  m <- leaflet() %>%
    setView(lng = -149.4937, lat = 64.2008, zoom = 3) %>%
    addProviderTiles(providers$Stamen.Toner) %>%
    addPolygons(data = ak_counties,
                color = "#660000", 
                weight = 1, 
                smoothFactor = 0.5) %>%
    addCircleMarkers(lng = ak_unsolved$lon, 
                     lat = ak_unsolved$lat,
                     color = "gold",
                     weight = 1,
                     radius = 5, 
                     opacity = 0.75,
                     label = lapply(ak_unsolved$label, HTML),
                     clusterOptions =  markerClusterOptions())
  m
  
  saveWidget(m, file = "dynamic_map.html")
  
  
  

 
  
  
  
  
  
  
 