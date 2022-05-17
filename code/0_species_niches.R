#Code to create the species networks

#Code setup-----

  #load libraries
    library(raster)
    library(dplyr)
    library(ggplot2)
    library(tidyr)
    library(sf)
    library(stars)
    library(rnaturalearth)
  
  # sf version 1.0 + s2 (Google) vs GEOS in the back engine (e.g., rgeos). This leads to slightly more accurate calculations but also causes a tonne of error when using polygons. 
  # this can be switched by toggling off the s2 backend using this function as it defaults to s2 when the package is initialized. 
  #  r-spatial.github.io/sf/articles/sf7.html
  
    sf::sf_use_s2(FALSE) 
  
  #source the function
    source("code/network_scope.R")
  
  #projections
    latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
  
  #load the GEBCO raster that is trimmed to the extent of the Canadian NW Atlantic 
    bathy <- raster("data/gebco_nw_atlantic.tif")

#load polygons-----
  
  #load the polygons of the network and convert it to the projection of the bathymetry data we will use
    network_initial <- read_sf("data/shapefiles/networksites_proposed_OEM_MPA_v2.shp")%>%
                       st_transform(latlong)%>%
                       filter(NAME != "Bras d’Or Lakes EBSA") # given this is a very esturarine system, I am not sure we want to include for things maybe other than lobster
           
  #some sites have two separate polygons entered as individual identities. This will merge them and then bring back in the other columns using left_join() - otherwise lost in group_by
    network <- rbind(network_initial%>%
                     filter(NAME == "Western Emerald Bank Conservation Area")%>%
                     st_combine()%>%
                     st_as_sf()%>%
                     mutate(area=as.numeric(st_area(.)/1000/1000),
                            NAME="Western Emerald Bank Conservation Area")%>%
                     dplyr::rename(geometry=x)%>%
                     dplyr::select(NAME,area,geometry),
                     network_initial%>%
                     filter(NAME !="Western Emerald Bank Conservation Area")%>%
                     dplyr::rename(area=AreaKM2)%>%
                     dplyr::select(NAME,area,geometry))%>%
              left_join(.,network_initial%>%
                          data.frame()%>%
                          dplyr::select(-geometry)%>%
                          distinct(NAME,.keep_all=TRUE))%>%
              mutate(species=NA,network="Draft Network")%>% #this is for later plotting
              dplyr::select(NAME,STATUS,TYPE,network,species,geometry)
  
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
    species_niche_all <- read.csv("data/species_niche_final.csv")
    
    #Porbeagle shark - Lamna nasus - has very few observaitons. So the OBIS check doesn't really work well. 
    #so we need to take it out and do a network adjust that doesn't seach obis. 
    porbeagle <- species_niche_all[species_niche_all$SciName == "Lamna nasus",]
    species_niche <- species_niche_all[!species_niche_all$SciName == "Lamna nasus",]

#now create the species niche networks -----

#loop this function to create each niche iteratively 
for(i in 1:nrow(species_niche)){
  
  network_adjust(species=species_niche[i,"SciName"],
                 network=network,# given this is a very estuarine system, I am not sure we want to include for things maybe other than lobster
                 bathy=bathy,
                 lower=species_niche[i,"LwrDepth10"]*-1, #have to be in negative numbers to work with the code
                 upper=species_niche[i,"UprDepth90"]*-1,
                 buffer=25, #set a buffer to the default of 25km
                 exclusion_trim=TRUE,
                 dsn = "output/species_networks/",
                 return_points = TRUE)
  
}
    

#now run it for porbeagle
    network_adjust(species=porbeagle[1,"SciName"],
                   network=network,# given this is a very estuarine system, I am not sure we want to include for things maybe other than lobster
                   bathy=bathy,
                   lower=porbeagle[1,"LwrDepth10"]*-1, #have to be in negative numbers to work with the code
                   upper=porbeagle[1,"UprDepth90"]*-1,
                   buffer=25, #set a buffer to the default of 25km
                   exclusion_trim=FALSE, #this means we are not going to constrain the network based on obis observaitons. 
                   dsn = "output/species_networks/",
                   return_points = TRUE)
    