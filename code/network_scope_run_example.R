#example application of the network_scope function

#Code setup-----

#load libraries
library(raster)
library(dplyr)
library(ggplot2)
library(tidyr)
library(sf)
library(stars)
library(rnaturalearth)

#projections
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

#source the function
source("code/network_scope.R")

#Load the gebco raster that is trimmed to the extent of the Canadian NW Atlantic 
bathy <- raster("data/gebco_nw_atlantic.tif")

#load the polygons of the network and convert it to the projection of the bathymetry data we will use
network <- read_sf("data/shapefiles/networksites_proposed_OEM_MPA_v2.shp")%>%mutate(network="Draft Network")

#some sites have two seperate polygons entered as individual identities. This will merge them and then bring back in the other columns using left_join() - otherwise lost in group_by
network <- network%>%
  group_by(NAME)%>%
  summarise(geometry=st_union(geometry))%>%
  left_join(.,network%>%
              data.frame()%>%
              distinct(NAME,.keep_all=TRUE)%>%
              mutate(area=as.numeric(st_area(geometry)/1000/1000))%>% #recalculate area for the split areas
              dplyr::select(NAME,STATUS,area,network,TYPE))%>%
  mutate(species=NA)%>% #this is a dummy variable for ggplot long-form data assembly below
  dplyr::select(NAME,STATUS,TYPE,network,area,species,geometry)

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
  st_intersection(.,network%>%st_transform(latlong)%>%st_bbox()%>%st_as_sfc()%>%st_as_sf())# this will trim the polygon to the extent of our focal area of interest using a bounding box

#read in the species niche data (last version sent by Shaylyn)
species_niche <- read.csv("data/species_niche_final.csv")

#do a test run of the function using wolffish and winter skate
#** look at the function for some other variables in the network_adjust that are held to their defaults and some are changed
  
  network_adjust(species="Anarhichas lupus",
                 network=network%>%filter(NAME != "Bras d’Or Lakes EBSA"),# given this is a very esturarine system, I am not sure we want to include for things maybe other than lobster
                 bathy=bathy,
                 lower=-1, #note these are 'hard' coded in. Ideally you would use the data in 'species_niche' to populate these inputs
                 upper=-500,
                 buffer=25, #set a buffer to the default of 25km
                 exclusion_trim=TRUE,
                 dsn = "output/species_networks/",
                 return_points = TRUE)
  
  network_adjust(species="Leucoraja ocellata",
                 network=network%>%filter(NAME != "Bras d’Or Lakes EBSA"),# given this is a very esturarine system, I am not sure we want to include for things maybe other than lobster
                 bathy=bathy,
                 lower=-12, #note these are 'hard' coded in. Ideally you would use the data in 'species_niche' to populate these inputs
                 upper=-138,
                 buffer=25, #set a buffer to the default of 25km
                 exclusion_trim=TRUE,
                 dsn = "output/species_networks/",
                 return_points = TRUE)
  
##now lets view the output
  
  wolffish_network <- st_read("output/species_networks/Anarhichas_lupus_network_trim.shp")%>%
                      mutate(species="Atlantic wolffish",
                             network="Niche-adjusted Network")%>%
                      st_transform(latlong)%>%
                      dplyr::select(NAME,STATUS,TYPE,network,area,species,geometry)
    
  wolffish_points <- read.csv("output/robis_extractions/Anarhichas_lupus_obis_overlaps.csv")%>%
                     filter(!is.na(X),!is.na(Y))%>%
                     st_as_sf(coords=c("X","Y"),crs=latlong)%>% #converts the xy data output from the function to a 'sf' object
                    mutate(species="Atlantic wolffish")
  
  winter_network <- st_read("output/species_networks/Leucoraja_ocellata_network_trim.shp")%>%
                    mutate(species="Winter skate",
                           network="Niche-adjusted Network")%>%
                    st_transform(latlong)%>%
                    dplyr::select(NAME,STATUS,TYPE,network,area,species,geometry)
  
  winter_points <- read.csv("output/robis_extractions/Leucoraja_ocellata_obis_overlaps.csv")%>%
                  filter(!is.na(X),!is.na(Y))%>%
                  st_as_sf(coords=c("X","Y"),crs=latlong)%>% #converts the xy data output from the function to a 'sf' object
                  mutate(species="Winter skate")
  
  
  networks <- rbind(wolffish_network,
                    winter_network,
                    network%>%st_transform(latlong)%>%mutate(species="Atlantic wolffish"),
                    network%>%st_transform(latlong)%>%mutate(species="Winter skate")) #this is a dummy variable so that ggplot will include a mention on the plot
  
  obis_points <- rbind(wolffish_points,winter_points)
  
  #assemble the plot
  p1 <- ggplot()+
    geom_sf(data=basemap,fill="darkolivegreen3")+ #this is the land
    #geom_sf(data=network,aes(fill=network))+ #plot the original network
    geom_sf(data=networks,aes(fill=network))+ #add the buffered polygons on the original network
    geom_sf(data=obis_points,size=0.25)+ #add the points on top of the shapes
    coord_sf(expand=0)+# this just gets rid of a plotting buffer that ggplot defaults to
        theme_bw()+
    theme(legend.position="bottom",
          strip.background = element_rect(fill="white"))+
    facet_wrap(~species,nrow=2)+
    labs(fill="")+
    scale_fill_manual(values=c("grey85","coral2"))
  
  ggsave("inst/comparison_trim_plot.jpg",p1,height=8,width=5,units="in",dpi=300)
