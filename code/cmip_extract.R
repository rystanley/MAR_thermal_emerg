#Code to estimate time of thermal emergence


#load libraries -------
library(devtools)
library(raster)# package for raster manipulation
library(sf)
library(sp)
library(stars)
library(rgdal)
library(ggplot2)
library(dplyr)
library(tidyr)
library(R.matlab) #to open Dan's MatLab files in R

sf_use_s2(FALSE)

#projection that will be used in the code
latlong <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
utm_mar <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0"

### Time of thermal emergence + 2100 

### STEP 1: Create a time series of maximum monthly average per year for each model/emissions scenario ###
#create a file path to your climate projection files. Keep in the folders (2.6,8.5) Dan had them in
fls<-c(list.files("data/climate_projections/2.6/", full.names=T),list.files("data/climate_projections/8.5/",full.names=T)) ##climate projections for RCP 2.6; run again for the 4.5 folder

#List of speices-niche constratined networks - this is the output of species_niches
network<-list.files('output/species_networks/', pattern="*.shp", full.names=T, recursive=FALSE)#put all cropped networks here with species name as the first part of the file name


#Create raster masks -----------
#for the extraction the code creates a buffered raster that is overlayed (think the networks shapes for each species at the resolution of the projections)
#because the projects have the same extent, and this step 'rasterize = network_raster_mask' takes quite a while there is no point in repeating it for each projection

# if(!file.exists("data/climate_projections/raster_masks.RData")){ #only need to do this once. But if we change the network we will have to delete the masks and re-run this

data <- readMat(fls[1]) #only do this for one projection because they are the same extent
names(data) <- "datout" #this will update the ones with 'outt'

bdata<-brick(data$datout,xmn=-83,xmx=-41,ymn=38,ymx=85,crs=latlong)
cmip_proj <- bdata@crs #projection of the CMIP

#now create the raster masks for each species and save it in a big list that can be loaded later (if  you are re-running the code)
# or if you are running this live then the next extraction will apply these masks in sequence.

out_masks <-list()
out_networks <- list()

for (j in 1:length(network)){
  
  spec <- network[j]%>%
    strsplit(.,"/")%>%
    unlist()%>%
    .[3]%>%
    strsplit(.,"_")%>%
    unlist()%>%
    .[c(1,2)]
  
  message(paste0("Working on raster mask for ",paste0(spec,collapse=" ")))
  
  network_sp <- read_sf(network[j])%>%
    st_transform(cmip_proj)%>%
    as_Spatial()
  
  network_extent <- extent(network_sp)
  
  #make a new mask that covers cells within and partially within a polygon - idea from here -https://gis.stackexchange.com/questions/255025/r-raster-masking-a-raster-by-polygon-also-remove-cells-partially-covered
  network_raster_mask <- rasterize(network_sp,crop(bdata[[1]],network_extent,snap="out"),getCover=TRUE) 
  network_raster_mask[network_raster_mask == 0] <- NA
  
  #save the outptus
  out_masks[[paste0(spec,collapse=" ")]] <- network_raster_mask
  out_networks[[paste0(spec,collapse=" ")]] <- network_sp
  
  rm(network_raster_mask,network_sp,spec)
  
}

save(out_masks,file="data/raster_masks.RData") #save this a list object that can be pulled in sequentially in the next bit of cod
save(out_networks,file="data/network_sp.RData") #conversion of these complex adjusted sites to SP takes time so why do it twice?

# }else{load("data/raster_masks.RData")
#       load("data/network_sp.RData")}


#Now do the data extractions ----------

#loop over the modeled scenarios
for(i in 1:length(fls)){
  
  #get information on the projection we are working with ----
  climate_proj <- strsplit(fls[i],"/")%>%
    unlist()%>%
    .[3]%>%#use the file name to get the projection given the folders structure where the 3rd in line from 'data' is the projection (data/climate_projections/8.5/IPSL_SSP5_RCP8.5_SST_0.25deg_regrid.mat)
    gsub("\\.","-",.) 
  
  mod <- strsplit(fls[i],"/")%>%
    unlist()%>%
    .[4]%>% #forth split is the file names
    strsplit(.,"_")%>%
    unlist()%>%
    .[1] #again use strsplit and the first element of the file name is the model
  
  #progress message
  message(paste0("Working on ",mod," ",gsub("-",".",climate_proj)))
  
  message(paste0("Loading/converting matlab CMIP output."))
  #read in the matlab file
  data <- readMat(fls[i])
  names(data) <- "datout" #this will update the ones with 'outt'
  
  bdata<-brick(data$datout,xmn=-83,xmx=-41,ymn=38,ymx=85,crs=latlong)
  cmip_proj <- bdata@crs #projection of the CMIP
  
  #loop through the network to extract the data. 
  cmip_extracts<-list()
  cmip_extracts_shapes <- list()
  
  for (j in 1:length(network)){ #loop through species specific networks
    
    spec <- gsub('_network_trim.shp','',network[j])%>%
      gsub("output/species_networks/","",.)%>%
      gsub("_"," ",.)
    
    #progress message
    message(paste0("Extracting ",spec))
  
    #load the depth-adjusted network (sf)
    network_sf <- read_sf(network[j])%>%
      st_transform(cmip_proj)
    
    #covert depth-adjusted network (sp)
    network_sp <- network_sf%>%
      as_Spatial()

    #extent of the nework
    network_extent <- extent(network_sp)
    
    #load the raster mask for species j
    network_raster_mask <- out_masks[[j]]
    
    #apply the mask to the entire raster stack using raster  
    bdata_processed <- bdata%>%
        crop(.,network_extent,snap="out")%>%
        raster::mask(.,network_raster_mask)
  
    #now use sf and stars to extract the data
      data_extract <- bdata_processed%>% #crop the raster to the extent of the PA
                       st_as_stars()%>% #convert to stars raster brick
                       st_as_sf()%>% #convert to sf dataframe
                       st_transform(utm_mar)%>% #convert to planar coordinates for a more appropriate overlay
                       st_intersection(.,network_sf%>%st_transform(utm_mar)%>%dplyr::rename(site_area=area))%>%
                       st_transform(st_crs(network_raster_mask))%>% #transform back to the raster brick projection
                       mutate(cell_area=as.vector(st_area(.)/1000/1000))%>% #calculate the area of the raster cells that are overlaid 
                       gather(.,"layer","temp",starts_with("layer."))%>% #convert to the long-form    
                       mutate(month=rep(rep(1:12,each=length(layer)/86/12),86),
                              year=rep(2015:2100,each=length(layer)/86),
                              species=spec,
                              mod=mod,
                              climate_proj=climate_proj)%>%
                       dplyr::select(mod,climate_proj,species,year,month,NAME,STATUS,TYPE,obis_count,site_area,cell_area,temp,geometry)%>%
                       suppressWarnings() #"attribute variables are assumed to be spatially constant throughout all geometries" - will clutter the output so it is suppressed
  
    #save the geometry information which is duplicated for each iteration (month x year)
      data_extract_shape <- data_extract%>%
                            filter(month==1,year==2015)

    #output only the extracted information
      data_extract_output <- data_extract%>%
                             data.frame()%>%
                             dplyr::select(-geometry)
      
    #save the output
    cmip_extracts_shapes[[j]] <- data_extract_shape
    cmip_extracts[[j]]<-data_extract_output
    
    #clean loop workspace
    rm(spec,network_sf,network_sp,network_extent,network_raster_mask,bdata_processed,data_extract,data_extract_shape,data_extract_output)
    
    } #end of species network loop
    
  if(!dir.exists("output/climate_extracts/")){dir.create("output/climate_extracts/")}
  
  message(paste0("Saving output from ",mod," ",climate_proj,". This step does take a while because the output can be large."))
  save(cmip_extracts_shapes,file=paste0("output/climate_extracts/climate_extracts_shape_",mod,"_",climate_proj,".RData")) #maybe useful for plotting
  save(cmip_extracts,file=paste0("output/climate_extracts/climate_extracts_",mod,"_",climate_proj,".RData"))
  
  message("Cleaning house.")
  rm(cmip_extracts,cmip_extracts_shapes)
  gc()
 
} #end loop through the projections




#if you want to do csvs but they will be enormous
# cmip_out <- do.call("rbind",cmip_extracts)%>%
#             data.frame()
# 
# #write the proposal
# write.csv(cmip_out,file=paste0("output/climate_extracts/climate_extracts_",mod,"_",climate_proj,".csv")) 
