##Map set up to visualize how the resolution of the maps will look like in the MPAs

#load libraries --------------
library(sf)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(raster)
library(stars)
library(ggspatial)

sf_use_s2(FALSE)

#Projections ------------
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

#load polygons -------------
bioregion <- read_sf("data/Shapefiles/MaritimesPlanningArea.shp")%>%st_transform(latlong)

network_initial <- read_sf("data/shapefiles/networksites_proposed_OEM_MPA_v2.shp")%>%
  st_transform(latlong)%>%
  filter(NAME != "Bras d’Or Lakes EBSA") # given this is a very esturarine system, I am not sure we want to include for things maybe other than lobster

#some sites have two separate polygons entered as individual identities. This will merge them and then bring back in the other columns using left_join() - otherwise lost in group_by
network <- network_initial%>%
  group_by(NAME)%>%
  summarise(geometry=st_combine(geometry)%>%st_make_valid(),
            area=as.numeric(st_area(geometry)/1000/1000))%>%
  ungroup()%>%
  st_make_valid()%>% #why sf now requires this so much is beyond me
  left_join(.,network_initial%>%
              data.frame()%>%
              dplyr::select(-geometry)%>%
              distinct(NAME,.keep_all=TRUE))%>%
  mutate(species=NA,network="Draft Network")%>% #this is for later plotting
  dplyr::select(NAME,STATUS,TYPE,network,species,geometry)

draft_network <- network%>%
  mutate(area=round((st_area(geometry)/1000/1000),2)%>%as.numeric()) #area in km2 converted from m2

#create a grid at 0.25 degrees which fits the downscale, scale that Dan Boyce set up. This is for visualization only
study_grid <- bioregion%>%st_make_grid(cellsize=0.25)
bioregion_grid <- bioregion%>%st_intersection(study_grid)
sites_grid <- draft_network%>%
              st_make_valid()%>%
              st_intersection(study_grid)

#create a basemap for plotting (this code can be used each time you make a plot)
basemap <- rbind(ne_states(country = "Canada",returnclass = "sf")%>%
                   dplyr::select(latitude,longitude,geonunit,geometry)%>%
                   st_union()%>% #group provinces + territories
                   st_as_sf()%>%
                   st_transform(latlong),
                 ne_states(country = "United States of America",returnclass = "sf")%>%
                   dplyr::select(latitude,longitude,geonunit,geometry)%>%
                   st_union()%>% #group provinces + territories
                   st_as_sf()%>%
                   st_transform(latlong))%>%
  st_intersection(.,bioregion%>%st_buffer(0.15)%>%st_make_valid()%>%st_transform(latlong)%>%st_bbox()%>%st_as_sfc()%>%st_as_sf())# this will trim the polygon to the extent of our focal area of interest using a bounding box


#Show it in a plot
p1 <- ggplot()+
  geom_sf(data=bioregion,fill=NA)+
  geom_sf(data=sites_grid,fill=NA,lwd=0.25)+
  geom_sf(data=draft_network,fill=NA,lwd=0.8)+
  theme_bw()

ggsave("inst/network_grid.png",p1,width=6,height=6,dpi=600,units="in") #this is used by the readme

#figure 1 example 
p2 <- ggplot()+
  geom_sf(data=basemap,fill="darkolivegreen3")+
  geom_sf(data=bioregion,fill=NA)+
  geom_sf(data=sites_grid,fill=NA,lwd=0.25)+
  geom_sf(data=draft_network,fill=NA,lwd=0.8)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  coord_sf(expand=0)+
  annotation_scale(location="br")+ #location is 'bottom right'
  annotation_north_arrow(location="tl",height = unit(0.3,"in"),width=unit(0.3,"in")) #location is top left 'tl'

ggsave("output/Fig1_ex.png",p2,width=6,height=6,dpi=600,units="in")


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
