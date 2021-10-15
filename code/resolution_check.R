##Map set up to visualize how the resolution of the maps will look like in the MPAs

#load libraries --------------
library(sf)
library(ggplot2)
library(dplyr)

#Projections ------------
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

#load polygons -------------
bioregion <- read_sf("data/Shapefiles/MaritimesPlanningArea.shp")%>%st_transform(latlong)
draft_network <- read_sf("data/Shapefiles/networksites_proposed_OEM_MPA_v2.shp")%>%
  st_transform(latlong)%>%
  mutate(area=round((st_area(geometry)/1000/1000),2)%>%as.numeric()) #area in km2 converted from m2

#create a grid at 0.25 degrees which fits the downscale, scale that Dan Boyce set up. This is for visualization only
study_grid <- bioregion%>%st_make_grid(cellsize=0.25)
bioregion_grid <- bioregion%>%st_intersection(study_grid)
sites_grid <- draft_network%>%
              st_intersection(study_grid)

#Show it in a plot
p1 <- ggplot()+
  geom_sf(data=bioregion,fill=NA)+
  geom_sf(data=sites_grid,fill=NA,lwd=0.25)+
  geom_sf(data=draft_network,fill=NA,lwd=0.8)+
  theme_bw()

ggsave("inst/network_grid.png",p1,width=6,height=6,dpi=600,units="in") #this is used by the readme

#count the number of grid cells occupied by each site
grid_count <- sites_grid%>%
              group_by(NAME)%>%
              summarise(count = n())%>%
              right_join(.,draft_network%>%data.frame()%>%dplyr::select(NAME,area))

#shows as expected there is a correlation between the site size and the amount of overlap with the grids
ggplot()+
  geom_point(data=grid_count,aes(area,count))+
  stat_smooth(method="lm")+
  theme_bw()

#what is the distribution of sites
ggplot()+
  geom_histogram(data=grid_count,aes(x=count))+
  theme_bw()+
  labs(x="Number of grid cells within a network site")
