## Function for extracting species specific observations from OBIS 
#https://ropensci.org/blog/2017/01/25/obis/

#load libraries --------------
library(sf)
library(ggplot2)
library(dplyr)
library(robis)
library(rnaturalearth)

#Projections ------------
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

#load polygons -------------
bioregion <- read_sf("data/Shapefiles/MaritimesPlanningArea.shp")%>%st_transform(latlong)
bioregion_box <- bioregion%>%st_bbox()%>%st_as_sfc()

#create a polygon for the Atlantic Region
nw_atlantic <- st_bbox(c(xmin = -72 , xmax = -47.9, ymax = 61, ymin = 39.8 ),crs=latlong)%>%st_as_sfc()%>%st_bbox()%>%st_as_sfc()

#basemaps

#full extent - this is a basemap of Canada and the USA
basemap_atlantic <- rbind(ne_states(country = "Canada",returnclass = "sf")%>%
                            dplyr::select(name_en,geometry)%>%
                            st_as_sf()%>%
                            st_union()%>%
                            st_transform(latlong)%>%
                            st_as_sf()%>%
                            mutate(country="Canada"),
                          ne_states(country = "United States of America",returnclass = "sf")%>%
                            dplyr::select(name_en,geometry)%>%
                            st_as_sf()%>%
                            st_union()%>%
                            st_transform(latlong)%>%
                            st_as_sf()%>%
                            mutate(country="USA"))

#Trimmed to the domain of our robis search area
basemap_full <- basemap_atlantic%>% 
   st_intersection(.,nw_atlantic) #will give some warnings but these aren't anything to worry about

#Trimmed to the domain of our study area
basemap_focus <- basemap_atlantic%>% 
  st_intersection(.,bioregion_box)

#map of the domains
p1 <- ggplot()+
  geom_sf(data=basemap_full)+
  geom_sf(data=nw_alantic,fill=NA)+
  geom_sf(data=bioregion)+
  theme_bw()+
  coord_sf(expand=0)#this makes it so there is no buffer around the extent of the nw_atlantic (zoomed out range)

#load the target species
target_species <- read.csv("data/input_data_ryan.csv")

#now extract data for each species from obis

obis_extract <- target_species%>%
                filter(scientific %in% c("Homarus americanus","Gadus morhua"))%>%
                group_by(scientific)%>% #so dplyr will apply the function 'occurance' using the 'do' function. so it 'does' 'occurance' for each 'scientific' name
                do(occurrence(.$scientific,# flags that you want this column 
                              geometry=nw_atlantic%>%st_as_text(), #this is the text based syntax required of robis for the polygon
                              startdate = "2000-01-01",enddate = "2021-09-01")) #all observations as of September
                ungroup()%>% #this will collapse the information down with a column corresponding to each row for each 'scientific' or species
                data.frame() #this just converts it to a data.frame 
                
depth_extract <- 
                
tt <- occurrence("gadus morhua",geometry=bioregion_box%>%st_as_text(),startdate = "2000-01-01",enddate = "2020-01-01") #st_as_text

ggplot()+geom_histogram(data=tt,aes(x=depth))+geom_vline(xintercept = quantile(tt$depth,c(0.1,0.9),na.rm=T))

