#load libraries
library(raster)
library(dplyr)
library(ggplot2)
library(tidyr)
library(sf)
library(stars)
library(rnaturalearth)
sf::sf_use_s2(FALSE) 

#projections
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

#load the GEBCO raster that is trimmed to the extent of the Canadian NW Atlantic 
bathy <- raster("data/gebco_nw_atlantic.tif")

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

# call in ToEs
df<-read.csv("output/ToEs/ToEs_cells_allspp&models.csv")
df2 <- toe%>%
  filter(species=="Gadus morhua")%>%
  group_by(climate_proj,NAME)%>%
  summarise(toe=mean(ToE), toe.sd=sd(ToE))%>%
  ungroup()%>%
  data.frame()
df26<-df2[df2$climate_proj=="2-6",]
df85<-df2[df2$climate_proj=="8-5",]

#merge with network polygons
require(sp) #need this package to merge dataframe to shapefile
cod_toes26<- merge(network,df26,by="NAME")
cod_toes85<-merge(network,df85,by="NAME")

#assemble the plot
p1 <- ggplot()+
  geom_sf(data=basemap,fill="darkolivegreen3")+ #this is the land
  geom_sf(data=cod_toes26,aes(fill=toe))+ #plot the original network
  #geom_sf(data=cod_toes26,aes(fill=toe))+ #add the buffered polygons on the original network
  coord_sf(expand=0)+# this just gets rid of a plotting buffer that ggplot defaults to
  theme_bw()+
  scale_fill_gradient(low = "red", high = "blue",limits=c(2015,2100))+
  theme(legend.position="bottom",
        legend.key.width = unit(2, 'cm'), #change legend key width
        legend.title = element_text(size=14), #change legend title font size
        legend.text = element_text(size=10),
        strip.background = element_rect(fill="white"))+
  #facet_wrap(~species,nrow=2)+
  labs(fill="")

ggsave("output/cod_toes2_rcp26.jpg",p1,height=8,width=5,units="in",dpi=300)

