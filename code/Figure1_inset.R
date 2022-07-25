#Figure 1 inset code

library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(ggspatial)

st_use_s2(FALSE)

#Projections ------------
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"


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

nw_atlantic <- st_bbox(c(xmin = -72 , xmax = -47.9, ymax = 61, ymin = 39.8 ),crs=latlong)%>%
  st_as_sfc()%>%st_bbox()%>%st_as_sfc() #this is for ~ the Canadian NW Atlantic

nw_box <- st_bbox(nw_atlantic)

#Trimmed to the domain of our robis search area
basemap_full <- basemap_atlantic%>% 
  st_intersection(.,nw_atlantic) #will give some warnings but these aren't anything to worry about


p1 <- ggplot()+
  geom_sf(data=basemap_full,fill="darkolivegreen3")+
  coord_sf(expand=0,xlim=nw_box[c(1,3)],ylim=nw_box[c(2,4)])+
  theme_bw()+
  annotation_north_arrow(location="tr")

ggsave("output/obis_range.png",p1,height=6,width=5,units="in",dpi=300)
  
  
