## Function for extracting species specific observations from OBIS 
  #https://ropensci.org/blog/2017/01/25/obis/

#load libraries --------------
  library(sf)
  library(ggplot2)
  library(dplyr)
  library(robis)
  library(rnaturalearth)
  library(fasterize)
  library(raster)
  library(stars)
  library(ggspatial)

st_use_s2(FALSE)

#load data ----------
  target_species <- read.csv("data/input_data_ryan.csv")

#Projections ------------
  latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

#load polygons -------------
  bioregion <- read_sf("data/Shapefiles/MaritimesPlanningArea.shp")%>%st_transform(latlong)

#create a bounding polygons -------
    nw_atlantic <- st_bbox(c(xmin = -72 , xmax = -47.9, ymax = 61, ymin = 39.8 ),crs=latlong)%>%
                    st_as_sfc()%>%st_bbox()%>%st_as_sfc() #this is for ~ the Canadian NW Atlantic
    
    st_bbox(nw_atlantic) #this is the extent of our study area for obis extractions
    
    bioregion_box <- bioregion%>%st_bbox()%>%st_as_sfc() #bounding box for the focus region
    
    st_bbox(bioregion_box)#this is extent of our focal region 

#basemaps ----------
    #full extent - this is a basemap of Canada and the USA
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
    
    #Trimmed to the domain of our robis search area
    basemap_full <- basemap_atlantic%>% 
       st_intersection(.,nw_atlantic) #will give some warnings but these aren't anything to worry about
    
    #Trimmed to the domain of our study area
    basemap_focus <- basemap_atlantic%>% 
      st_intersection(.,bioregion_box)

#map of the domains -------------
    p1 <- ggplot()+
      geom_sf(data=basemap_full)+
      geom_sf(data=nw_alantic,fill=NA)+
      geom_sf(data=bioregion)+
      theme_bw()+
      coord_sf(expand=0);p1  #this makes it so there is no buffer around the extent of the nw_atlantic (zoomed out range)

## Depth limit extractions ------------
    
    if(!dir.exists("output/robis_extractions")){dir.create("output/robis_extractions")} #This will create the directory if it doesn't already exist that the outputs will be stored

    depth_ranges <- NULL #this is a holder that will be built in the loop
    depth_extracts <- list() #this empty list object that will be filled in the loop for each species.
    
  for(i in target_species$scientific){
    
      message(paste0("working on ",i)) #progress message so you can see where you are at
      
      #do the obis extraction
      temp <- occurrence(i, #'i' is iteratively assigned the value of each species 
                         geometry=nw_atlantic%>%st_as_text(), #this is the text based syntax required of robis for the polygon
                         startdate = "2000-01-01",enddate = "2021-09-01")%>% #all observations as of September for the past 2 decades
              mutate(scientific=i)# this will make sure you know what the 'input' was. 
      
      #for some species there are no depth records and obis doesn't return a depth based column, so we need to build in a contingency
      if(!"depth" %in% colnames(temp)){
        
        temp$depth <- NA
        depth_temp <- data.frame(depth_lower_obis=NA,depth_upper_obis=NA,scientific=i)
        
       
      } else {
      
      #now extract the depth ranges based on quantiles
      depth_temp <- temp%>%
                    filter(!is.na(depth))%>%
                    summarise(depth_lower_obis=quantile(depth,0.1), #10th percentile
                              depth_upper_obis=quantile(depth,0.9))%>% #90th percentile
                    mutate(scientific = i)%>%
                    data.frame()
      }
      
      #now you can iteratively build a dataframe that has the scientific name and the approximate 10th and 90th percentiles
      depth_ranges <- rbind(depth_ranges,depth_temp)
      
      #create a variable new name without spaces
      name <- gsub(" ","_",i)
      
      #data extracts for location and full depth range - just so we have an idea of the distribution for later checking. This will be saved as a list object in R
      depth_extracts[[name]] <- temp%>%dplyr::select(scientific,decimalLongitude,decimalLatitude,depth)
      
      #assign that  name  to the data object 'temp' in the work environment. This stops it from being saved over each iteration of the loop
      #assign(name,temp) #now you want to create a variable name that 
      
      save(temp,file=paste0("output/robis_extractions/",name,".RData")) #save the output for future use. 
      
    }

  #save the summarized outputs and coordinates. Note this will only overwrite if the file is not there already. 
    if(!file.exists("output/depth_extracts_coordinates.RData")) {save(depth_extracts,file="output/depth_extracts_coordinates.RData")}
    if(!file.exists("output/depth_ranges_output.csv")){write.csv(depth_ranges,file="output/depth_ranges_output.csv",row.names=FALSE)}
   
  #Make a map of the depth range extractions
    load("output/depth_extracts_coordinates.RData")
    
    depth_df <- do.call("rbind",depth_extracts)%>% #depth_extracts is a list for each species, so it needs to be pulled apart and rbind'ed. do.call is your friend. 
                  rename(long=decimalLongitude,lat=decimalLatitude)%>%
                  filter(!is.na(depth))%>% #keep just the ones with depth observations. 
                  dplyr::select(long,lat)#this is a data.frame of lat and long that can be used for the raster calculation (which is fast)
    
    depth_grid <- raster(extent(nw_atlantic%>%st_as_sf()),res=0.25,crs=latlong) # a raster grid to make the layer of observation counts
    
    depth_count <- rasterize(depth_df,depth_grid,fun="count")#how many obs per 0.25 degree grid cell
    values(depth_count) <- log10(values(depth_count)) #scale to log10 because of the crazy amount of lobster values
    
    depth_count_stars <- st_as_stars(depth_count) #convert the raster to a ggplot'able stars object
    
    depth_plot <- ggplot()+
      geom_stars(data=depth_count_stars)+
      geom_sf(data=bioregion,fill=NA,col="grey40",lwd=1.2)+
      geom_sf(data=basemap_full,fill="darkolivegreen3")+
      theme_bw()+
      scale_fill_viridis_b(na.value="white",option="inferno",labels=c(expression(10^1),expression(10^2),expression(10^3),expression(10^4)))+
      coord_sf(expand=0)+
      labs(fill='# of observations',x="",y="")+
      theme(legend.position = "bottom")+
      annotation_scale(location="br")+
      annotation_north_arrow(location="tr",height = unit(0.6,"in"),width=unit(0.6,"in"));depth_plot
    
    ggsave("output/depth_plot.png",depth_plot,height=8,width=6,units="in",dpi=300)      
   
### DEPRICATED CODE ----------------

#if you want to do this in dplyr (a bit faster but more risky because the data from each obis extraction isn't saved)
# obis_extract <- target_species%>%
#                 filter(scientific %in% c("Homarus americanus","Anarhichas lupus"))%>%
#                 group_by(scientific)%>% #so dplyr will apply the function 'occurrence' using the 'do' function. so it 'does' 'occurance' for each 'scientific' name
#                 do(occurrence(.$scientific,# flags that you want this column 
#                               geometry=nw_atlantic%>%st_as_text(), #this is the text based syntax required of robis for the polygon
#                               startdate = "2000-01-01",enddate = "2021-09-01"))%>% #all observations as of September
#                 dplyr::select(decimalLatitude,decimalLongitude,scientificName,
#                               date_start,date_end,date_year,month,year,
#                               lifeStage,
#                               maximumDepthInMeters,minimumDepthInMeters,depth)%>%
#                 ungroup()%>% #this will collapse the information down with a column corresponding to each row for each 'scientific' or species
#                 data.frame() #this just converts it to a data.frame 
#                 
# 
# ggplot()+geom_histogram(data=tt,aes(x=depth))+geom_vline(xintercept = quantile(tt$depth,c(0.1,0.9),na.rm=T))

