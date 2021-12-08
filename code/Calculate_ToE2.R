#Code to estimate time of thermal emergence


#load libraries -------
library(devtools)
library(raster)# package for raster manipulation
library(sf)
library(sp)
library(rgdal)
library(ggplot2)
library(R.matlab) #to open Dan's MatLab files in R

#projection that will be used in the code
latlong <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

### Time of thermal emergence + 2100 ###############
####################################################

### STEP 1: Create a time series of maximum monthly average per year for each model/emissions scenario ###
#create a file path to your climate projection files. Keep in the folders (2.6,8.5) Dan had them in
fls<-c(list.files("data/climate_projections/2.6/", full.names=T),list.files("data/climate_projections/8.5/",full.names=T)) ##climate projections for RCP 2.6; run again for the 4.5 folder
l<-list()
l2<-list()

#List of speices-niche constratined networks - this is the output of species_niches
network<-list.files('output/species_networks/', pattern="*.shp", full.names=T, recursive=FALSE)#put all cropped networks here with species name as the first part of the file name


#Create raster masks -----------
    #for the extraction the code creates a buffered raster that is overlayed (think the networks shapes for each species at the resolution of the projections)
    #because the projects have the same extent, and this step 'rasterize = network_raster_mask' takes quite a while there is no point in repeating it for each projection

      if(!file.exists("data/climate_projections/raster_masks.RData")){ #only need to do this once. But if we change the network we will have to delete the masks and re-run this
        
        data <- readMat(fls[1]) #only do this for one projection because they are the same extent
        names(data) <- "datout" #this will update the ones with 'outt'
        
        bdata<-brick(data$datout,xmn=-83,xmx=-41,ymn=38,ymx=85,crs=latlong)
        cmip_proj <- bdata@crs #projection of the CMIP
        
        #now create the raster masks for each species and save it in a big list that can be loaded later (if  you are re-running the code)
          # or if you are running this live then the next extraction will apply these masks in sequence.
        out_masks<-list()
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
          st_transform(cmip_proj)%>%as_Spatial()
        network_extent <- extent(network_sp)
        
        #make a new mask that covers cells within and partially within a polygon - idea from here -https://gis.stackexchange.com/questions/255025/r-raster-masking-a-raster-by-polygon-also-remove-cells-partially-covered
        network_raster_mask <- rasterize(network_sp,crop(bdata[[1]],network_extent,snap="out"),getCover=TRUE) 
        network_raster_mask[network_raster_mask == 0] <- NA
        
        out_masks[[paste0(spec,collapse=" ")]] <- network_raster_mask
        
        }
        
        save(out_masks,file="data/climate_projections/raster_masks.RData") #save this a list object that can be pulled in sequentially in the next bit of cod
        
  }else{load("data/climate_projections/raster_masks.RData")}

#Now do the data extractions ----------
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
        
        #read in the matlab file
        data <- readMat(fls[i])
        names(data) <- "datout" #this will update the ones with 'outt'
        
        bdata<-brick(data$datout,xmn=-83,xmx=-41,ymn=38,ymx=85,crs=latlong)
        cmip_proj <- bdata@crs #projection of the CMIP
        
        #loop through the network to extract the data. 
          cmip_extracts<-list()
          
          for (j in 1:length(network)){
        
            #load the species raster mask from the previous step
            network_raster_mask <- out_masks[[j]]
            network_extent <- extent(network_sp)
          
          #process the raster brick
          bdata_processed <- bdata%>%
            crop(.,network_extent,snap="out")%>%
            raster::mask(.,network_raster_mask)
          
          df <- as.data.frame(bdata_processed,xy=TRUE,long=TRUE,centroids=TRUE)%>%
            filter(!is.na(value))%>%
            mutate(month=rep(rep(1:12,each=length(layer)/86/12),86),
                   year=rep(2015:2100,each=length(layer)/86),
                   species=substr(gsub('.shp','',paste(network[j])),25,40),
                   mod=mod,
                   climate_proj=climate_proj)
          
          cmip_extracts[[j]]<-df
          
          } #end of loop for the species networks
          
          if(!dir.exists("output/climate_extracts/")){dir.create("output/climate_extracts/")}
          
          save(cmip_extracts,file=paste0("output/climate_extracts/climate_extracts_",mod,"_",climate_proj,".RData"))
          
          #if you want to do csvs but they will be enormous
          # cmip_out <- do.call("rbind",cmip_extracts)%>%
          #             data.frame()
          # 
          # #write the proposal
          # write.csv(cmip_out,file=paste0("output/climate_extracts/climate_extracts_",mod,"_",climate_proj,".csv")) 
             
    } #end loop through the projections
        
   
