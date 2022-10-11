fed1<-read.csv("https://raw.githubusercontent.com/BuzzFeedNews/2016-04-federal-surveillance-planes/master/data/feds/feds1.csv")
sum(fed1$other_names1=="")
sum(fed1$other_names2=="")
fed1<-fed1[,-c(12:13)]
unique(fed1$flight_id)
table(fed2_sur$agency,fed2_sur$model)

library(tidyverse)
fed1['name'][fed1['name']=="U S DEPT OF HOMELAND SECURITY"|fed1['name']=="DEPARTMENT OF HOMELAND SECURITY"|
               fed1['name']=="U S DEPARTMENT OF HOMELAND SECURITY"|
               fed1['name']=="DEPT OF HOMELAND SECURITY"] <-"US DEPARTMENT OF HOMELAND SECURITY"
fed1['name'][fed1['name']=="NATIONAL AIRCRAFT LEASING CORP"|
               fed1['name']=="NATIONAL AIRCRAFT LEASING" ]<-"NATIONAL AIRCRAFT LEASING CORPORATION"    

fed2<-fed1%>%filter(latitude>=35&latitude<=40.21&longitude>=-80&longitude<=-75)
write.csv(fed2_sur,"fed_washington.csv")
count(fed2_sur,flight_id)

fed2_sur<-fed2%>%
  filter(flight_id%in%b$flight_id)
unique(fed2_sur$flight_id)
fed2_sur$timestamp<-gsub("T"," ",fed2_sur$timestamp)
fed2_sur$timestamp<-gsub("Z"," ",fed2_sur$timestamp)
fed2_sur$timestamp<-as.POSIXct(fed2_sur$timestamp)
str(fed2_sur$timestamp)

fed3<-fed1%>%
  filter(flight_id%in%b$flight_id)
unique(fed3$flight_id)
write.csv(fed3,"surveillance_plane.csv") 


a<-count(fed1,flight_id)
b<-a%>%filter(n>500)


library(lubridate)
fed2_sur$timestamp <- ymd_hms(fed2_sur$timestamp)

#change the time to date

fed2_sur$Date <- as.Date(fed2_sur$timestamp)
fed2_sur$Year <- as.numeric(format(fed2_sur$timestamp, "%Y"))
fed2_sur$Month <- as.numeric(format(fed2_sur$timestamp, "%m"))
fed2_sur$Day <- as.numeric(day(fed2_sur$timestamp))
fed2_sur$Weekday <- weekdays(fed2_sur$timestamp)
fed2_sur$Hour <- as.numeric(hour(fed2_sur$timestamp))

library(writexl)
write_xlsx(fed2_sur,"C:/Users/ACER/Downloads/Final_fed.xlsx")


# animation ---------------------------------------------------------------
library(ggplot2)
library(gganimate)
library(gifski)
library(ggmap)

# county_info1 <- map_data("county", region="District of Columbia")
# county_info2 <- map_data("county", region="Virginia")
# county_info<-rbind(county_info1,county_info2)
# 
# # base_map <- ggplot(data = county_info, mapping = aes(x = long, y = lat, group = group)) +
# #   geom_polygon(color = "black", fill = "white") +
# #   coord_quickmap() +
# #   theme_void() 
# # base_map

base_map<-ggmap(get_stamenmap(
  bbox = c(left=-77.4442,bottom=38.57671,right=-76.98019,top=39.0459),
  maptype= "terrain",
  zoom =12
))
base_map

plane1<-fed2_sur%>%filter(flight_id=="73e2cf4")

min_long <- min(plane1$longitude)
max_long <- max(plane1$longitude)
min_lat <- min(plane1$latitude)
max_lat <- max(plane1$latitude)

map_with_data <- base_map +
  geom_point(data = plane1, aes(x = longitude, y = latitude, color=flight_id, group="flight"),shape=8,size=3) +
  coord_quickmap(xlim = c(min_long, max_long),  ylim = c(min_lat, max_lat))
map_with_data

map_with_animation <- map_with_data +
  transition_time(timestamp)
animate(map_with_animation, fps = 15,nframes = 250,renderer = gifski_renderer())
anim_save("73e2cf4.gif")

plane2<-fed2_sur%>%filter(flight_id=="74aad81")

base_map<-ggmap(get_stamenmap(
  bbox = c(left=min(plane2$longitude)-0.05,bottom=min(plane2$latitude)-0.05,right=max(plane2$longitude)+0.05,top=max(plane2$latitude)+0.05),
  maptype= "terrain",
  zoom =12
))
base_map
min_long <- min(plane2$longitude)-0.05
max_long <- max(plane2$longitude)+0.05
min_lat <- min(plane2$latitude)-0.05
max_lat <- max(plane2$latitude)+0.05

map_with_data <- base_map +
  geom_point(data = plane2, aes(x = longitude, y = latitude, color=flight_id, group="flight"),shape=8,size=3) +
  coord_quickmap(xlim = c(min_long, max_long),  ylim = c(min_lat, max_lat))
map_with_data

map_with_animation <- map_with_data +
  transition_time(timestamp)
animate(map_with_animation, fps = 15,nframes = 250,renderer = gifski_renderer())
anim_save("74aad81.gif")


