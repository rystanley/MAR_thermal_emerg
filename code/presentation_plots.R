#Atlantic cod/Gully example for presentation


#load libraries
library(sf)
library(dplyr)
library(ggplot2)
library(rnaturalearth)

#turn off the sf use of s2 objects, which for some reason seems to propagate errors. 
sf::sf_use_s2(FALSE)

#projections to use
planar <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0" #UTM in 'km'
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

#load the network polygon
bioregion <- read_sf("data/shapefiles/MaritimesPlanningArea.shp")%>%st_transform(latlong)
network <- read_sf("data/shapefiles/networksites_proposed_OEM_MPA_v2.shp")%>%st_transform(latlong)
gully <- network%>%filter(NAME=="The Gully Marine Protected Area")
bathy <- read_sf("data/shapefiles/Countour_250.shp")%>%
              st_transform(latlong)%>%
              st_intersection(.,bioregion)

#load the depth adjusted shapes for cod
cod <- read_sf("output/species_networks/Gadus_morhua_network_trim.shp")%>%
        filter(NAME =="The Gully Marine Protected Area")%>%
        st_transform(latlong)

#basemap
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
  st_intersection(.,bioregion%>%st_transform(latlong)%>%st_bbox()%>%st_as_sfc()%>%st_as_sf())# this will trim the polygon to the extent of our focal area of interest using a bounding box

#create a bounding box for the grid
bound_box <- gully%>%st_bbox()

#adjust so it falls out with the grid along the 0.25 degree grid
bound_box[1] <- -59.5
bound_box[2] <- 43.25
bound_box[3] <- -58.5
bound_box[4] <- 44.25

bound_box <- bound_box%>%st_as_sfc()%>%st_as_sf()

bound_grid <- bound_box%>%st_make_grid(cellsize=0.25)

bound_bioregion <- bioregion%>%st_bbox()

#adjust so it falls out with the grid along the 0.25 degree grid
bound_bioregion[1] <- -68
bound_bioregion[2] <- 40
bound_bioregion[3] <- -55
bound_bioregion[4] <- 48

bound_bioregion_grid <- bound_bioregion%>%
                        st_make_grid(cellsize = 0.25)%>%
                        st_intersection(bioregion)


#Gully plot

p1 <- ggplot()+
  geom_sf(data=gully,fill="white",col="black")+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

p2 <- ggplot()+
  geom_sf(data=cod,fill="red",col="black")+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

p3 <- ggplot()+
  geom_sf(data=gully,fill="white",col="black")+
  geom_sf(data=cod,fill="red",col="black")+
  geom_sf(data=bound_grid,col="black",fill=NA)+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

p4 <- ggplot()+
      geom_sf(data=network,fill="grey90",col="black")+
      geom_sf(data=gully,fill="red",col="black")+
      geom_sf(data=bound_bioregion_grid,fill=NA,col="grey80")+
      geom_sf(data=bioregion,col="black",fill=NA)+
      geom_sf(data=basemap,fill="darkolivegreen3")+
      geom_sf(data=network,col="black",fill=NA)+
      geom_sf(data=bathy,fill=NA)+
      theme(plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank())
  
#save the plots
ggsave("output/gully.png",p1,width=6,height=6,dpi=300,units="in")
ggsave("output/codgully.png",p2,width=6,height=6,dpi=300,units="in")
ggsave("output/gridgully.png",p3,width=6,height=6,dpi=300,units="in")
ggsave("output/networkgully.png",p4,width=6,height=6,dpi=300,units="in")
