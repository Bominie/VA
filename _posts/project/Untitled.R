
packages = c('sf', 'tmap', 'tidyverse', 
             'lubridate', 'clock', 
             'sftime', 'rmarkdown', "binr")
for (p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p,character.only = T)
}
```
pubs <- read_sf("_posts/project/Pubs.csv", 
                options = "GEOM_POSSIBLE_NAMES=location")
buildings <- read_sf("_posts/project/Buildings.csv", 
                     options = "GEOM_POSSIBLE_NAMES=location")
restaurants <- read_sf("_posts/project/Restaurants.csv", 
                       options = "GEOM_POSSIBLE_NAMES=location")
schools <- read_sf("_posts/project/Schools.csv", 
                   options = "GEOM_POSSIBLE_NAMES=location")
apartments <- read_sf("_posts/project/Apartments.csv", 
                      options = "GEOM_POSSIBLE_NAMES=location")
employers <- read_sf("_posts/project/Employers.csv", 
                     options = "GEOM_POSSIBLE_NAMES=location")
check_in <- read.csv("_posts/project/CheckinJournal.csv")
participants <- read_rds("_posts/project/hw2_participants.rds")
```
#Building Type (Q1. How will show the %) - bar graph
tm_shape(buildings)+
  tm_polygons(col = "buildingType",
              size = 1,
              title = "Building Type",
              border.col = "black",
              border.lwd = 1) +
  tm_layout(main.title= 'Building Type in Ohio', 
            main.title.position = c('left'),
            main.title.size = 1.3, legend.outside = TRUE,)
split<-buildings%>%group_by(buildingType)%>%summarise(n=n())

```
# apartment occupancy, population desnity
apartments <- apartments %>%
  select(c("apartmentId", "buildingId")) %>%
  rename("venueId" = "apartmentId") 
apartments$venueId <- as.numeric(as.character(apartments$venueId))
apartments$buildingtype <- "Apartment"

check_in_apartment <- check_in %>%
  filter(venueType == "Apartment")
apartments_1 <- full_join(check_in_apartment,apartments, by = "venueId")
apartments_1<-na.omit(apartments_1)
apartments_1<-apartments_1[!duplicated(apartments_1$participantId), ]

apartments_2<-apartments_1%>%
  group_by(buildingId)%>%
  summarise(density=n())

apartmentoccupancy<-left_join(buildings,
                              apartments_2%>%
                               mutate(buildingId=as.character(buildingId)),
                             by=c("buildingId"="buildingId"))


tmap_mode("plot")
tm_shape(apartmentoccupancy)+
  tm_polygons(col = "density",
              border.col = "grey",
              style="cont",
              palette = "Blues",
              border.lwd = 1,
              border.alpha = 0.5,
              colorNA = "white")+
  tm_layout(frame=F,
            main.title = "Population density",
            main.title.size = 2,
            legend.position = c("right","top"),
            legend.height = 0.2,
            legend.width = 0.2)+
  tm_compass()
```
#agegroup occupancy (Q. how to combine 4 age groups)
apartments_age<-merge(apartments_1,participants[,c("participantId","agegroup")],by="participantId",all.x=TRUE)
apartments_age<-apartments_age%>%
  group_by(agegroup,buildingId)%>%
  summarise(density=n())

apartments_age_30below<-apartments_age%>%filter(agegroup=="30 below")
apartments_age_30to39<-apartments_age%>%filter(agegroup=="30-39")
apartments_age_40to49<-apartments_age%>%filter(agegroup=="40-49")
apartments_age_50above<-apartments_age%>%filter(agegroup=="50 and above")

apartmentoccupancy30below<-left_join(buildings,
                              apartments_age_30below%>%
                                mutate(buildingId=as.character(buildingId)),
                              by=c("buildingId"="buildingId"))
apartmentoccupancy30to39<-left_join(buildings,
                                     apartments_age_30to39%>%
                                       mutate(buildingId=as.character(buildingId)),
                                     by=c("buildingId"="buildingId"))
apartmentoccupancy40to49<-left_join(buildings,
                                     apartments_age_40to49%>%
                                       mutate(buildingId=as.character(buildingId)),
                                     by=c("buildingId"="buildingId"))
apartmentoccupancy50above<-left_join(buildings,
                                     apartments_age_50above%>%
                                       mutate(buildingId=as.character(buildingId)),
                                     by=c("buildingId"="buildingId"))

tmap_mode("plot")
tm_shape(apartmentoccupancy30below)+
  tm_polygons(col = "density",
              border.col = "grey",
              style="cont",
              palette = "Blues",
              border.lwd = 1,
              border.alpha = 0.5,
              colorNA = "white")+
  tm_layout(frame=F,
            main.title = "Population density",
            main.title.size = 2,
            legend.position = c("right","top"),
            legend.height = 0.2,
            legend.width = 0.2)+
  tm_compass()
tmap_mode("plot")
tm_shape(apartmentoccupancy30to39)+
  tm_polygons(col = "density",
              border.col = "grey",
              style="cont",
              palette = "Blues",
              border.lwd = 1,
              border.alpha = 0.5,
              colorNA = "white")+
  tm_layout(frame=F,
            main.title = "Population density",
            main.title.size = 2,
            legend.position = c("right","top"),
            legend.height = 0.2,
            legend.width = 0.2)+
  tm_compass()
tmap_mode("plot")
tm_shape(apartmentoccupancy40to49)+
  tm_polygons(col = "density",
              border.col = "grey",
              style="cont",
              palette = "Blues",
              border.lwd = 1,
              border.alpha = 0.5,
              colorNA = "white")+
  tm_layout(frame=F,
            main.title = "Population density",
            main.title.size = 2,
            legend.position = c("right","top"),
            legend.height = 0.2,
            legend.width = 0.2)+
  tm_compass()
tmap_mode("plot")
tm_shape(apartmentoccupancy50above)+
  tm_polygons(col = "density",
              border.col = "grey",
              style="cont",
              palette = "Blues",
              border.lwd = 1,
              border.alpha = 0.5,
              colorNA = "white")+
  tm_layout(frame=F,
            main.title = "Population density",
            main.title.size = 2,
            legend.position = c("right","top"),
            legend.height = 0.2,
            legend.width = 0.2)+
  tm_compass()
```
st_read("_posts/project/buildings.shp")
