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
fls<-c(list.files("data/climate_projections/2.6/", full.names=T),list.files("data/climate_projections/8.5/",full.names=T)) ##climate projections for RCP 2.6; run again for the 8.5 folder
l<-list()

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
          st_transform(cmip_proj)%>%as_Spatial()
        
        network_extent <- extent(network_sp)
        
        #make a new mask that covers cells within and partially within a polygon - idea from here -https://gis.stackexchange.com/questions/255025/r-raster-masking-a-raster-by-polygon-also-remove-cells-partially-covered
        network_raster_mask <- rasterize(network_sp,crop(bdata[[1]],network_extent,snap="out"),getCover=TRUE) 
        network_raster_mask[network_raster_mask == 0] <- NA
        
        #save the outptus
        out_masks[[paste0(spec,collapse=" ")]] <- network_raster_mask
        out_networks[[paste0(spec,collapse=" ")]] <- network_sp
        
        rm(network_raster_mask,network_sp,spec)
        
        }
        
        save(out_masks,file="data/raster_masks.RData") #save this as list object that can be pulled in sequentially in the next bit of code
        save(out_networks,file="data/network_sp.RData") #conversion of these complex adjusted sites to SP takes time so why do it twice?
        
      }else{load("data/raster_masks.RData")
            load("data/network_sp.Rdata")}

#Now do the data extractions ---------- do once for RCP2.6, then again for fls=RCP8.5
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
          
          for (j in 19:length(network)){
            
            spec <- gsub('_network_trim.shp','',network[j])%>%
                    gsub("output/species_networks/","",.)%>%
                    gsub("_"," ",.)
            
            #progress message
            message(paste0("Extracting ",spec))
        
            #load the species raster mask from the previous step
            network_raster_mask <- out_masks[[j]]
        
            network_sp <- out_networks[[j]]%>%spTransform(.,cmip_proj)
            network_extent <- extent(network_sp)
          
          #process the raster brick
          bdata_processed <- bdata%>%
            crop(.,network_extent,snap="out")%>%
            raster::mask(.,network_raster_mask)
          
          #for some reason some of the overlays have no data when overlayed on the cmip - this is an issue for Lamna nasus and model GFDL
          df <- as.data.frame(bdata_processed,xy=TRUE,long=TRUE,centroids=TRUE)
          
          if(!sum(is.na(df$value))==length(df$value)){ 
            df <- df%>%
            filter(!is.na(value))%>%
            mutate(month=rep(rep(1:12,each=length(layer)/86/12),86),
                   year=rep(2015:2100,each=length(layer)/86),
                   species=spec,
                   mod=mod,
                   climate_proj=climate_proj)
          }else{
            df <- df%>%
              slice(1)%>% #this is just maintain an entry
              mutate(month=NA,
                     year=NA,
                     species=spec,
                     mod=mod,
                     climate_proj=climate_proj)
          }
          
          
            cmip_extracts[[j]]<-df
          
          } #end of loop for the species networks
          
          if(!dir.exists("output/climate_extracts/")){dir.create("output/climate_extracts/")}
          
          save(cmip_extracts,file=paste0("output/climate_extracts/climate_extracts_",mod,"_",climate_proj,".RData"))
          
          #if you want to do csvs but they will be enormous
          #cmip_out <- do.call("rbind",cmip_extracts)%>%data.frame()
          #
          #write.csv(cmip_out,file=paste0("output/climate_extracts/climate_extracts_",mod,"_",climate_proj,".csv")) 
             
    } #end loop through the projections
        

### Read in cmip_extracts and reduce to one value (max temp) per year for each species/site/cell/model/emissions scenario
ces<-c(list.files("output/climate_extracts/", full.names=T, pattern="*.RData", recursive=F))
l<-list()
mmfun<-function(d){ #this is a function to calculate max temp
  max<-data.frame(maxT=max(d$value,na.rm=TRUE))
  return(max)
}
require(plyr)
for(i in 1:length(ces)){
  load(ces[i])
  data=do.call("rbind",cmip_extracts)
  data$site="site name here"
  dvel<-ddply(data,.(x,y,species,mod,climate_proj,year),.fun=mmfun,.progress='text')
  l[[i]]<-dvel}
require(data.table)
max<-data.frame(rbindlist(l))

## Now merge with species list to get upper thermal limit for each species
spp<-read.csv("data/species_niche_final.csv")
names(spp)
spp$species=spp$SciName
spp<-spp[,c(4,9)]
df<-merge(max,spp,by="species")

##############################################################
findToE<-function(maxT,ProjTS,yearsProj,runLength=2){
  
  ##IF SPECIES DOES NOT EMERGE FROM ITS NICHE BY END OF PROJECTION, SET TO THIS
  ToEFirst<-max(yearsProj)+1
  
  emYears<-which(ProjTS>maxT)
  
  if(length(emYears)>0){
    
    x<-rep(0,length(yearsProj)) # create binary vector showing emergent (1) or non-emergent (0) years
    x[emYears]<-1
    
    #calculate the year when the temperature emerges for a period of at least 'runLength' years
    diffs <- x[-1L] != x[-length(x)] # find where the values change
    idx <- c(which(diffs), length(x)) # get the indexes, and then get the difference in subsequent indexes
    runs<-diff(c(0, idx))# calculate the length of the runs
    x2<-x[-which(c(NA,diff(x))==0)] # determine whether each run is a run of 1's or 0's
    yearsRunStart<-yearsProj[-which(c(NA,diff(x))==0)] # find the start year of each run
    runsTab<-data.frame(year=yearsRunStart,val=x2,run=runs) # store all this in a table
    runsTab<-runsTab[runsTab$val==1,] # remove runs of 0's
    runStart<-which(runsTab$run>=runLength)
    if(length(runStart)>0){
      ToEFirst<-min(runsTab$year[runStart]) #find the first year where there is a run of 1's equal to or greater than runLength
    }
    
  }
  return(ToEFirst)
}



network <- read_sf("data/shapefiles/networksites_proposed_OEM_MPA_v2.shp")%>%mutate(network="Draft Network")




