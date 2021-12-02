## code to extract data from CMIP netcdf files

#load libraries
library(R.matlab)
library(sf)
library(dplyr)
library(raster)
library(stars)

#load raster 
data <- readMat("data/Projections/AWI_SSP1_RCP2.6_SST_0.25deg_regrid.mat")
bdata<-stack() #need to learn how to do this
bdata<-brick(data$datout,xmn=-83,xmx=-41,ymn=38,ymx=85,crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") #this works but then need to turn into dataframe before cropping

#projections
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
cmip_proj <- bdata@crs #projection of the CMIP

#load the polygons of the network and convert it to the projection of the bathymetry data we will use
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

#create filters and mask extents that can be applied to the rasterbrik
network_sp <- network%>%st_transform(cmip_proj)%>%as_Spatial()
network_extent <- extent(network_sp)

#make a new mask that covers cells within and partially within a polygon
network_raster_mask <- rasterize(network_sp,crop(bdata[[1]],network_extent,snap="out"),getCover=TRUE) 
network_raster_mask[network_raster_mask == 0] <- NA

#process the raster brick
bdata_processed <- bdata%>%
                   crop(.,network_extent,snap="out")%>%
                   mask(.,network_raster_mask)

#show what the processing steps do 
  plot(bdata_processed[[1]])
  plot(network_sp,add=T)

#create the data.frame for the network
df <- as.data.frame(bdata_processed,xy=TRUE,long=TRUE,centroids=TRUE)%>%
      filter(!is.na(value))%>%
      mutate(month=rep(rep(1:12,each=length(layer)/86/12),86),
             year=rep(2015:2100,each=length(layer)/86))


