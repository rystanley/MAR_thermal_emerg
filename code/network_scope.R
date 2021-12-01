#Function to clip polygons based on the depth ranges## e
network_adjust <- function(species,network,bathy,lower,upper,buffer=25,exclusion_trim=TRUE,dsn = "output/species_networks/",return_points=FALSE,update_obis=FALSE,year_start=2000,year_end=2021){
  
  #this is a function that will trim the polygons based on the depth range included and will also do a check for whether the species has been observed by OBIS in that location
  
  #species - scientific name for the species (e.g., "Homarus americanus")
  #network - sf data.frame spatial object that has the network of polygons where each site is named in a column called NAME
  #bathy - is a bathymetric raster object that is overlaps the study region. Note that the analysis will be done in the project native to this raster and thus this raster must be projected. 
  #lower - is the shallowest (m) that a species would be expected. Values should be in the native direction of the raster (e.g., if depth is negative in the raster these need to be negative)
  #upper - is the deepest (m) that a species would be expected
  #buffer - is a buffer (km) around an MPA that the PA check should be conducted. Default is 25 km. If you want no buffer set to NA
  #exclusion_trim - logical (default = TRUE) that decides whether the depth-based trimming should be applied to only those sites with obis observations within the focal sites (+ buffer)
  #dsn - this is the destination folder - by default it will create and go to "output/species_networks/" ** keep in mind that this code will overwrite the shape file each time. So if you want a different buffer, for example, create a unique dsn folder to go to. 
  #return_points - this is a logical (default = FALSE) to return the points that overlapped with the buffered polygons
  #update_obis - logical (default=FALSE) stating whether you want to update obis to return a different year range. If true then the year_start and year_end will be the grouping variables
  #year_start - the year to start the search (default=2000)
  #year_end - the year to end the search range. If 2021 it will be inclusive of nov-01 (default=2021)
  
  ## required libraries
  require(sf)
  require(dplyr)
  require(stars)
  require(raster)
  require(robis)
  require(ggplot2)
  
  #turn off the sf use of s2 objects, which for some reason seems to propagate errors. 
  sf::sf_use_s2(FALSE)
  
  #projections to use
  planar <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0" #UTM in 'km'
  latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
  
  #record the projections
  network_projection <- st_crs(network)
  bathy_projection <- proj4string(bathy)
  
  #convert network to the projection of the bathy - this is needed because they need to be the same projection and it takes way longer to convert they bathy raster
  network <- network%>%st_transform(bathy_projection)%>%suppressMessages()%>%suppressWarnings()# these just give a default response that isn't of interest for the scale of analysis we are doing. 
  
  #set up the output directory. If there is a problem with the file structure it will catch here instead of after it processes the shape files and bathymetry data
  
  if(substring(dsn,nchar(dsn),nchar(dsn))!="/"){dsn=paste0(dsn,"/")} #just a check to make sure the file path was entered correctly and fix it if not
  
  if(!dir.exists(dsn)){dir.create(dsn)} # this will create the directory for the first time
  
##OBIS check ------ 
  
  if(exclusion_trim){ # only do the obis check if you want to subset the network for the depth-based trimming (boundary modification)
  
  search_extent <- network%>%
                   st_combine()%>%
                   st_boundary()%>%
                   st_convex_hull()%>%
                   st_transform(latlong)%>% # this is the extent of the network
                   suppressMessages()%>%
                   suppressWarnings()
  
      #plot it to see what this code is doing
        #ggplot()+geom_sf(data=search_extent)+geom_sf(data=network)
  
  if(update_obis){
    
    #progress message to note the start of the obis extraction
    message(paste0("Running obis extraction for ",species,"."))
    
    start <- paste0(year_start,"-01-01")
    if(year_end == 2021) {end <- "2021-09-01"} else {end <- paste0(year_end,"11-01")}
    
    temp <- occurrence(i, #'i' is iteratively assigned the value of each species 
                       geometry=search_extent%>%st_as_text(), #this is the text based syntax required of robis for the polygon
                       startdate = start,enddate = end)%>% #all observations as of September for the past 2 decades
      mutate(scientific=i)# this will make sure you know what the 'input' was. 
    
    save(temp,file=paste0("output/robis_extractions/",name,"_",year_start,"_",year_end,".RData")) #save the output for future use. 
    
  }
  
  if(!update_obis){
    #progress message to note the start of the obis check
    message(paste0("Running the obis check for ",species,"."))
    
    #this uses the data that were already extracted using the function code/robis_extract.R
    load(paste0("output/robis_extractions/",gsub(" ","_",species),".RData"))
    
  }
    #format as an 'sf' dataframe
    dat <- temp%>%
            dplyr::select(decimalLatitude,decimalLongitude,date_year,depth)%>% ## add the year
            st_as_sf(coords=c("decimalLongitude","decimalLatitude"),crs=latlong)%>%
            st_intersection(.,search_extent)%>% #the time this takes scales exponentially with time
            #st_join(.,search_extent,join=st_within)%>%
            suppressMessages()
    
    if(return_points){ #if you want the sub-setted data used to confirm the presence of a species within 25km of a site. 
      
      message("Writing the observations to a obis observations within the sites (+ buffer) to a geometric file")
      
      if(!dir.exists("output/robis_extractions/")){dir.create("output/robis_extractions/")}
      
      #data.frame for the csv output
      temp_out <- dat%>%
                  rename(year=date_year)%>%
                  mutate(long=st_coordinates(.)[,1],
                         lat=st_coordinates(.)[,2],
                         species=species)%>%
                  data.frame()%>%
                  dplyr::select(species,long,lat,year,depth)
      
      write.csv(temp_out, paste0("output/robis_extractions/",gsub(" ","_",species),"_obis_overlaps.csv"), row.names = FALSE) #will overwrite existing files
      
    }
     
    #now create a buffered network to do the overlay - if no buffer exists the code will just use the original un-buffered network       
      if(!is.na(buffer)){
        network_buffered <- network%>%
                            st_transform(planar)%>%
                            st_buffer(buffer)%>%
                            st_transform(latlong)%>%
                            st_as_sf()%>%
                            st_make_valid()
        
      }else{network_buffered=network%>%st_transform(latlong)}
    
      #view what the buffered network looks like
      #ggplot()+geom_sf(data=network_buffered,fill="cornflowerblue")+geom_sf(data=network,fill="grey20")
  
    ###### This step does an overlay analysis and says how many points fall within the buffered polygon for each site
    ## however for some reason I can't create a vector in the network_buffered sf dataframe so we will create a vector 
    # and 'fill' it in sequentially with the counts and then add that vector to the sf dataframe 'network_buffered'
    
    #progress message
    message(paste0("Running intersection for ",nrow(network)," sites with ",nrow(dat)," obis observations."))
    
    count <- NULL #start off with nothing then for each i loop grow it using concatenation 'c(..)'
    
    for(i in 1:nrow(network_buffered)){count <- c(count,lengths(st_intersects(network_buffered[i,],dat))%>%suppressMessages())}
      
    #now assign that vector of counts to the sf dataframe 
    network_buffered$count <- count  
      
        #Show how this works only works if one of the sites as a zero count - check range(network_buffered$count)
        # ggplot()+
        #   geom_sf(data=network_buffered%>%filter(count==0))+
        #   geom_sf(data=dat)+
        #   theme_bw()
        # 
  
  } # end if exclusion trim 
  
  
### Depth based trimming  -----------
    
  if(sum(count) ==0){message(paste0(species, " has not been observed in any network sites + ",buffer,"km."," A revised network will not be returned."))}
  
  if(sum(count) != 0){
  
    #progress message to note the start of the obis check
    message(paste0("Trimming the shape files for ",species," based on the shallow boundary of ",lower, "m and deep boundary of ",upper,"m"))
  
  #convert network to the bathymetric projection and remove sites that don't have any obis observations (if exclusion_trim == TRUE)
  
  
  
  if(exclusion_trim){
    
  obis_sites <- network_buffered%>%filter(count>0)%>%pull(NAME) # This doesn't seem to work when put directly into the network_trim piping 
    
  network_trim <- network%>%
                  filter(NAME %in% obis_sites)%>%
                  st_transform(bathy_projection)}
  
  if(!exclusion_trim){network_trim <- network}
    
  #For loop that will do the trim based on the bathymetric layer (derived from GEBCO)
  names_select <- c(names(network_trim)[1:length(network_trim)-1],"include",names(network_trim)[length(network_trim)])
  
  network_trim <- network_trim%>%
                  mutate(include=TRUE)%>%#this is so we can exclude those sites that happen to have observations within the buffered area but are outside of the depth range 
                  dplyr::select(all_of(names_select))
  
  network_trimmed <- NULL #this is a dataframe that will grow for each loop                
  for(i in 1:nrow(network_trim)){
    
    #Progress message I find this helps for for loops that are slow so you know if it is still working and if there is a specific site that is taking a while. (bigger sites == more time)
    message(paste0("Working on ",i," of ",nrow(network_trim)," : ",network_trim%>%data.frame()%>%slice(i)%>%pull(NAME)))
    
    #develop a cropping extent
    cr <- extent(network_trim[i,])
    
    #crop the bathy to the extent of the polygons 
    b1 <- crop(bathy,cr,snap="out") #snap was originally set to near which was cutting off the edges in some places
    
    #logical to make sure there are depths within the specified range within the MPA
    site_mask <- mask(b1,network_trim[i,])
    site_range <- values(site_mask)[!is.na(values(site_mask))]%>%range()
    depth_check <- upper<=site_range[2] & lower>=site_range[1] # https://stackoverflow.com/questions/325933/determine-whether-two-date-ranges-overlap/325964#325964
    
    if(depth_check){ #only go on for this depth range based crop if those depths are in the polygon of interest
      
      #now you want to create NA values for the cells in the raster that are out of range
      b1[b1[]>lower] <- 0
      b1[b1[]<upper] <- 0
      b1[is.na(b1[])] <- 0
      
      #this will create a polygon (b3) that is all cells that have a partial overlap with the MPA
      #now create a polygon that encompasses the non-NA values
      b2 <- rasterize(network_trim[i,],b1,getCover=TRUE)
      b2[b2[] == 0] <- NA  #all the cells of 0 were not covered or partially covered by the polygon so ar given a value of NA. These will not be included when making the polygon in the next step
      
      b3 <- st_as_stars(b2)%>%
        st_as_sf(as_points=FALSE,merge=TRUE)%>%
        st_make_valid()%>%
        st_cast("MULTIPOLYGON")%>%
        st_combine()%>%
        suppressMessages()%>% #these just print things that we don't really need. 
        suppressWarnings() 
      
      #now create a mask of the original bathymetric layer and add NA for the values that don't fit    
      b4 <- mask(b1,b3%>%as_Spatial)
      b4[b4[] == 0] <- NA #the 'zero' values here aren't within the polygon and are replaced with NAs so that the stars and sf packages can make them into polygons
      
      #now this is the final step - a polyon that wraps the proper depth range
      b5 <- st_as_stars(b4)%>%
        st_as_sf(as_points=FALSE,merge=TRUE)%>%
        st_cast("POLYGON")%>%
        st_combine()%>%
        st_make_valid()%>%
        st_intersection(network_trim[i,]%>%st_make_valid())%>% #this trims it all back to the same as the polygon.
        suppressMessages()%>% #these just print things that we don't really need. 
        suppressWarnings() 
      
      #copy over the new polygon in the sf dataset
      #st_geometry(network_trim[i,]) <- st_geometry(b5)# this is was causing errors with the sf update
      network_trimmed <- rbind(network_trimmed,b5%>% # this is slow but something changed in the sf package that makes it so we can no longer do this with on line of code and the st_geometry call :(
                                           st_as_sf()%>%
                                           st_transform(latlong)%>%
                                           cbind(.,network_trim[i,]%>%data.frame()%>%dplyr::select(-geometry))%>%
                                           rename(geometry=x)%>%
                                           dplyr::select(all_of(names(network_trim))))
   
    }else{network_trim[i,"include"] <- FALSE} #end of the depth check logical 
    
  }#end of site loop
  
  network_trimmed <- network_trimmed%>%filter(include) #keep only those that have an obis observation (if that was selected as a filter criteria) and have the depth range required
        

##write the outputs ---------------
    
    #add in the counts from OBIS (note that these are inclusive of the buffer) in case they come in valuable later. 
    output <- network_trimmed%>%
              left_join(.,network_buffered%>%data.frame()%>%dplyr::select(NAME,count))%>%
              rename(obis_count=count)%>%
              mutate(area=as.numeric(st_area(geometry)/1000/1000))%>%#calculation of the area that has habitat for that species (new polygons)
              dplyr::select(NAME,STATUS,TYPE,area,obis_count,geometry)%>%
              group_by(NAME)%>%
              summarise(geometry=st_collection_extract(geometry)%>%st_cast("POLYGON"))%>% #in some cases the polygons care coming out as Geometry collections (have no idea why -- dam you sf) but this will collect it all and transform to a polygon that can be saved
              st_make_valid()%>%
              ungroup()%>%
              st_transform(network_projection)%>%
              suppressWarnings()%>% #messages that some polygons are already in the right format so they are not converted. 
              suppressMessages()# there are messages about the grouping variable (not useful)
    
  #recast to the native projection
  delete_dsn_logic <- file.exists(paste0(dsn,gsub(" ","_",species),"_network_trim.shp")) #this just checks whether an overwrite is needed (delete_dsn variable) and prevents a message saying it couldn't delete something that doesn't exist
  
  #write the shape file
  st_write(output,paste0(dsn,gsub(" ","_",species),"_network_trim.shp"),delete_dsn=delete_dsn_logic)%>%suppressMessages() #save to the new directory -- this will overwrite the file that is there. 
  
  }# end of the catch if statement for species that haven't been observed within any buffered polygons
}



