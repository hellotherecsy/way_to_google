
library('ggplot2')
library('ggmap')
library('leaflet')
 

geo_info <- data.frame(city=NA, lon=NA, lat=NA)
 

for ( i in jobs$city ) {
  
  info <- geocode(i , source ='dsk')
  info$lon <- as.factor(info$lon)
  info$lat <- as.factor(info$lat)
  info$city <- i 
  geo_info <- rbind(geo_info, info)
}


geo_info  %>% filter(., is.na(lat ))


geo_info <- filter(geo_info, !is.na(lat ))




continent = c('North America', 'South America','Europe','Asia', 'Africa', 'Australia', 'india')
continent_info <- data.frame(location=NA, lon=NA, lat=NA)

for ( i in continent ) {
  
  info <- geocode(i , source ='dsk')
  info$location <- i 
  continent_info <- rbind(continent_info, info)
}

continent_info <- filter(continent_info, !is.na(lat ))
continent_info


# Data correction
geo_info$lon <- ifelse(geo_info$city == 'Gurugram', 77.030370 ,geo_info$lon )
geo_info$lat <- ifelse(geo_info$city == 'Gurugram', 28.460814 ,geo_info$lat)

# 미국  -98.433774 38.518349,
# 남미 , -60.151014 -8.277305
# 아프리카  23.198569 -2.422458
continent_info$lon <- ifelse(continent_info$location == 'North America',-98.433774 ,continent_info$lon )
continent_info$lat <- ifelse(continent_info$location == 'North America', 38.518349 ,continent_info$lat)
continent_info$lon <- ifelse(continent_info$location == 'South America', -60.151014 ,continent_info$lon )
continent_info$lat <- ifelse(continent_info$location == 'South America', -8.277305 ,continent_info$lat)
continent_info$lon <- ifelse(continent_info$location == 'Africa', 23.198569 ,continent_info$lon )
continent_info$lat <- ifelse(continent_info$location == 'Africa',-2.422458 ,continent_info$lat)

## Save the data 
write.csv(geo_info, file = "../data/geo_info.csv")
write.csv(continent_info, file = "../data/continent_info.csv")



