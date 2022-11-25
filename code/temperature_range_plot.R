## Plot temperature 2025, 2050, 2075, 2100 for sites

#load libraries -----
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(R.matlab)
library(patchwork)

sf_use_s2(FALSE)

#Projections ------------
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
utm_mar <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0"

#load the GEBCO raster that is trimmed to the extent of the Canadian NW Atlantic 
bathy <- raster("data/gebco_nw_atlantic.tif")

#load the time of emergence summaries
load("output/toe_summaries/all_toe_summaries.RData")

toe_summaries <- toe_summaries%>%
  filter(mod != "GFDL")%>% # we are not using the GFDL model
  mutate(climate_proj = gsub("\\.","-",climate_proj))# the . was left in from the ensemble calculation


#load the species groupings and niche information
groupings <- read.csv("data/species_grouping.csv")%>%dplyr::select(SciName,functional_group,cosewic_status)
niches <- read.csv("data/species_niche_final.csv")%>%
  left_join(.,groupings)%>%
  data.frame()%>%
  dplyr::select(SciName, LwrDepth10,UprDepth90)%>%
  rename(species=SciName)

#load geographic files
bioregion <- read_sf("data/Shapefiles/MaritimesPlanningArea.shp")%>%st_transform(latlong)

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
  st_intersection(.,bioregion%>%st_buffer(0.15)%>%st_make_valid()%>%st_transform(latlong)%>%st_bbox()%>%st_as_sfc()%>%st_as_sf())# this will trim the polygon to the extent of our focal area of interest using a bounding box

#load the network and calculate the centroid (to look for a spatial connection)
network_base <- read_sf("data/shapefiles/networksites_proposed_OEM_MPA_v2.shp")%>%
  filter(NAME != "Bras d’Or Lakes EBSA")%>%
  group_by(NAME)%>%
  summarise(geometry=st_union(geometry))%>% #unify the western emerald bank polyons
  ungroup()%>%
  st_as_sf()

network_cents <- network_base%>% # to calculate a latitude covariate (lower latitude the more 'south' a site is)
  st_centroid()%>%
  st_transform(latlong)%>%
  mutate(long=st_coordinates(.)[,1],
         lat=st_coordinates(.)[,2])%>%
  data.frame()%>%
  dplyr::select(NAME,long,lat)

#grouping of stations (based in part on centriods)
fundy_sites <- c("Brier Island","Chignecto Bay","Head Harbour, West Isles and The Passages","John Lusby Marsh National Wildlife Area",
                 "Horse Mussel Reefs","Long Eddy","Musquash Estuary Marine Protected Area","South Grand Manan","Southern Bight",
                 "Boot Island National Wildlife Area","Chignecto Bay","Northern Gulf of Maine","Machias Seal Island Migratory Bird Sanctuary")

eastern_SS <- network_cents%>%filter(long>-61)%>%pull(NAME)

western_SS <- setdiff(network_base$NAME,c(fundy_sites,eastern_SS))

#add the grouping to network cents
network_cents <- network_cents%>%
  mutate(region=case_when(NAME %in% fundy_sites ~ "Bay of Fundy",
                          NAME %in% eastern_SS ~ "Eastern Scotian Shelf",
                          NAME %in% western_SS ~ "Western Scotian Shelf"))%>%
  arrange(long)
#save output
#write.csv(network_cents,"output/network_centriods.csv",row.names=FALSE)

#create sf version for plotting
network_cents_sf <- network_cents%>%st_as_sf(coords=c("long","lat"),crs=latlong)

#add data to the network
network <- network_base%>%
  left_join(.,network_cents)%>%
  st_transform(latlong)

#add the network centriod data to the toe_summaries data file. 
toe_dat <- toe_summaries%>%
  left_join(.,network%>%
              filter(NAME %in% unique(toe_summaries$NAME))%>%
              data.frame()
        

#now identify which species are within each site. -----------
species_site <- data.frame()

for(i in unique(toe_dat$NAME)){
  
  species_site <- rbind(species_site,
                        data.frame(NAME = i, 
                                   species = toe_dat%>%filter(NAME == i)%>%distinct(species)%>%pull(species)))
  
}

species_site_range <- species_site%>%
                      left_join(.,niches)%>%
                      group_by(NAME)%>%
                      summarise(lower=min(LwrDepth10,na.rm=T),
                                upper=max(UprDepth90,na.rm=T))%>%
                      ungroup()%>%
                      data.frame()


#load the network and create a new depth limited network for the aggregate species per site. -------------

network_initial <- read_sf("data/shapefiles/networksites_proposed_OEM_MPA_v2.shp")%>%
  st_transform(latlong)%>%
  filter(NAME != "Bras d’Or Lakes EBSA") # given this is a very esturarine system, I am not sure we want to include for things maybe other than lobster

#some sites have two separate polygons entered as individual identities. This will merge them and then bring back in the other columns using left_join() - otherwise lost in group_by
network_sub <- rbind(network_initial%>%
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
  mutate(species=NA,
         network="Draft Network",
         include=TRUE)%>% #this is for later plotting
  dplyr::select(NAME,STATUS,TYPE,network,species,geometry)%>%
  left_join(.,species_site_range)%>%
  filter(!is.na(lower))#values without a value don't have any focal species within them based on the toe_dat data. 

## this runs a grouped species depth trim on the network. It only needs to be done once and the interim output is saved/can be loaded. 

# #now trim each site by depth - this code is from the guts of network_scope
# 
# #set the plot trimming limits
# network_trimmed <- NULL #this is a dataframe that will grow for each loop   
# for(i in 1:nrow(network_sub)){
#   
#   #depth limits per site
#   lower <- network_sub[i,]%>%pull(lower)*-1
#   upper <- network_sub[i,]%>%pull(upper)*-1
#   
#   #Progress message I find this helps for for loops that are slow so you know if it is still working and if there is a specific site that is taking a while. (bigger sites == more time)
#   message(paste0("Working on ",i," of ",nrow(network_sub)," : ",network_sub%>%data.frame()%>%slice(i)%>%pull(NAME)))
#   
#   #develop a cropping extent
#   cr <- extent(network_sub[i,])
#   
#   #crop the bathy to the extent of the polygons 
#   b1 <- crop(bathy,cr,snap="out") #snap was originally set to near which was cutting off the edges in some places
#   
#   #logical to make sure there are depths within the specified range within the MPA
#   site_mask <- mask(b1,network_sub[i,])
#   site_range <- values(site_mask)[!is.na(values(site_mask))]%>%range()
#   depth_check <- upper <= site_range[2] & lower >=site_range[1] # https://stackoverflow.com/questions/325933/determine-whether-two-date-ranges-overlap/325964#325964
#   
#     #now you want to create NA values for the cells in the raster that are out of range
#     b1[b1[]>lower] <- 0
#     b1[b1[]<upper] <- 0
#     b1[is.na(b1[])] <- 0
#     
#     #this will create a polygon (b3) that is all cells that have a partial overlap with the MPA
#     #now create a polygon that encompasses the non-NA values
#     b2 <- rasterize(network_sub[i,],b1,getCover=TRUE)
#     b2[b2[] == 0] <- NA  #all the cells of 0 were not covered or partially covered by the polygon so ar given a value of NA. These will not be included when making the polygon in the next step
#     
#     b3 <- st_as_stars(b2)%>%
#       st_as_sf(as_points=FALSE,merge=TRUE)%>%
#       #st_make_valid()%>%
#       st_cast("MULTIPOLYGON")%>%
#       st_combine()%>%
#       suppressMessages()%>% #these just print things that we don't really need. 
#       suppressWarnings() 
#     
#     #now create a mask of the original bathymetric layer and add NA for the values that don't fit    
#     b4 <- mask(b1,b3%>%as_Spatial)
#     b4[b4[] == 0] <- NA #the 'zero' values here aren't within the polygon and are replaced with NAs so that the stars and sf packages can make them into polygons
#     
#     #now this is the final step - a polyon that wraps the proper depth range
#     b5 <- st_as_stars(b4)%>%
#       st_as_sf(as_points=FALSE,merge=TRUE)%>%
#       st_cast("POLYGON")%>%
#       st_transform(latlong)%>%
#       st_combine()%>%
#       st_intersection(network_sub[i,]%>%st_make_valid())%>% #this trims it all back to the same as the polygon.
#       suppressMessages()%>% #these just print things that we don't really need. 
#       suppressWarnings() 
#     
#     #copy over the new polygon in the sf dataset
#     #st_geometry(network_trim[i,]) <- st_geometry(b5)# this is was causing errors with the sf update
#     network_trimmed <- rbind(network_trimmed,b5%>% # this is slow but something changed in the sf package that makes it so we can no longer do this with on line of code and the st_geometry call :(
#                                st_as_sf()%>%
#                                st_transform(latlong)%>%
#                                cbind(.,network_sub[i,]%>%data.frame()%>%dplyr::select(-geometry))%>%
#                                rename(geometry=x)%>%
#                                dplyr::select(all_of(names(network_sub))))
#     
#   }
#   
# }#end of site loop
# 
# #recast to the native projection
# delete_dsn_logic <- file.exists(paste0("output/grouped_species_network_trim.shp")) #this just checks whether an overwrite is needed (delete_dsn variable) and prevents a message saying it couldn't delete something that doesn't exist
# 
# #write the shape file
# st_write(network_trimmed,paste0("output/grouped/grouped_species_network_trim.shp"),delete_dsn=delete_dsn_logic)%>%suppressMessages() #save to the new directory -- this will overwrite the file that is there. 

#this will load the output from the above commented code
network_trimmed <- st_read("output/grouped/grouped_species_network_trim.shp")%>%st_transform(latlong)

# #Extract the climate data for the grouped data (based on cmip_extract.R) ------

#only need to do this once so the interim data product is saved as an RData file. 
    
    fls<-c(list.files("data/climate_projections/2.6/", full.names=T),list.files("data/climate_projections/8.5/",full.names=T)) ##climate projections for RCP 2.6; run again for the 4.5 folder
    fls <- fls[!grepl("CNRM",fls)] #remove the CNRM model from the analysis
    fls <- fls[!grepl("GFDL",fls)] #remove the GFDL model from the analysis

    data <- readMat(fls[1]) #only do this for one projection because they are the same extent
    names(data) <- "datout" #this will update the ones with 'outt'

    bdata<-brick(data$datout,xmn=-83,xmx=-41,ymn=38,ymx=85,crs=latlong)
    cmip_proj <- bdata@crs #projection of the CMIP

    network_sp <- network_trimmed%>%
      st_transform(cmip_proj)%>%
      as_Spatial()

    network_extent <- extent(network_sp)

    #make a new mask that covers cells within and partially within a polygon - idea from here -https://gis.stackexchange.com/questions/255025/r-raster-masking-a-raster-by-polygon-also-remove-cells-partially-covered
    network_raster_mask <- rasterize(network_sp,crop(bdata[[1]],network_extent,snap="out"),getCover=TRUE)
    network_raster_mask[network_raster_mask == 0] <- NA

    #Now get the ensemble mean

    emmission_scenarios <- c("2.6","8.5")

    #loop over the modeled scenarios
    for(i in emmission_scenarios){

      cmip_files <- fls[grep(i,fls)]

      message(paste0("Loading data for RCP ",i))

      #read in data and create brick (month (12) x years (86))
      for(j in 1:3){

        temp <- readMat(cmip_files[j])
        names(temp) <- "datout"
        temp <- brick(temp$datout,xmn=-83,xmx=-41,ymn=38,ymx=85,crs=latlong)
        assign(paste0("emmisiondata_",j),temp)
        rm(temp)

      }

      #get the emsemble for each month/year combination

      message("Conducting model averaging to generate an ensemble.")

      ensemble.list <- list()
      for(s in 1:dim(emmisiondata_1)[3]){

        temp <- stack(emmisiondata_1[[s]],emmisiondata_2[[s]],emmisiondata_3[[s]])
        ensemble <- calc(temp,fun=mean,na.rm=T) # note that 'AWI' doesn't apply a land mask, so this will give us more data for coastal MPAs that are otherwise missed by the HAD/ISPL because of that 0.25 degree land mask
        ensemble.list[[s]] <- ensemble

      }


      #convert to a raster brick
      bdata <- ensemble.list%>%stack()%>%brick(.,xmx=-41,ymn=38,ymx=85,crs=latlong)
      cmip_proj <- proj4string(bdata)

      ## do the extracts on the ensembles ---
      message(paste0("Working on the climate extractions for RCP ",i))

      #loop through the network to extract the data.
      cmip_extracts<-list()
      cmip_extracts_shapes <- list()
      climate_proj <- i
      mod <- "Ensemble"
      spec <- "grouped species"

        #progress message
        message(paste0("Extracting ",spec," for RCP ",i))

        #load the depth-adjusted network (sf)
        network_sf <- network_trimmed%>%
          st_transform(cmip_proj)

        #covert depth-adjusted network (sp)
        network_sp <- network_sf%>%
          as_Spatial()

        #extent of the network
        network_extent <- extent(network_raster_mask)

        #apply the mask to the entire raster stack using raster
        bdata_processed <- bdata%>%
          crop(.,network_extent,snap="out")%>%
          raster::mask(.,network_raster_mask)
    #     
    #     #now use sf and stars to extract the data
        data_extract <- bdata_processed%>% #crop the raster to the extent of the PA
          st_as_stars()%>% #convert to stars raster brick
          st_as_sf()%>% #convert to sf dataframe
          st_transform(utm_mar)%>% #convert to planar coordinates for a more appropriate overlay
          st_intersection(.,network_sf%>%st_transform(utm_mar))%>%
          st_transform(st_crs(network_raster_mask))%>% #transform back to the raster brick projection
          mutate(cell_area=as.vector(st_area(.)/1000/1000),
                 id = 1:n())%>% #calculate the area of the raster cells that are overlaid
          gather(.,"layer","temp",starts_with("layer."))%>% #convert to the long-form
          mutate(month=rep(rep(1:12,each=length(layer)/86/12),86),
                 year=rep(2015:2100,each=length(layer)/86),
                 species=spec,
                 mod=mod,
                 climate_proj=climate_proj)%>%
           dplyr::select(mod,climate_proj,species,year,month,NAME,STATUS,TYPE,cell_area,id,temp,geometry)%>%
           suppressWarnings()%>% #"attribute variables are assumed to be spatially constant throughout all geometries" - will clutter the output so it is suppressed
           data.frame()%>%
           dplyr::select(-geometry)
    
        #   #save the data - this takes a while as the data is large
      message(paste0("Saving output from ",mod," ",climate_proj,". This step does take a while because the output can be large."))
     save(data_extract,file=paste0("output/grouped/climate_extracts_grouped_species_",tolower(mod),"_",gsub("\\.","-",climate_proj),".RData"))

    } #end of emmission loop

##bring it together to plot ------------

#List of climate model outputs
extract_fls <- list.files("output/grouped/", full.names=T)
extract_fls <- extract_fls[grepl("ensemble",extract_fls)] #get the model averaged results for each RCP scenario

combo_data <- NULL
for(i in 1:2){
  load(extract_fls[i])
  combo_data <- rbind(combo_data,data_extract)
  rm(data_extract)
}

#load species abbreviations
site_info <- read.csv("output/site_description_table.csv")%>%
             rbind(.,data.frame(site= "Big Glace Bay",abbreviation = "BGB",n_species=NA, #big glace bay was missing
                                long=-59.92992,lat=46.17751,region="Eastern Scotian Shelf",area=5.306933))

yearbuffer = 5 #buffer to take the average temp 

combo_data_sum <- combo_data%>%
                  group_by(climate_proj,year,id)%>%
                  summarise(temp=mean(temp,na.rm=T))%>% #annual average as there is a id per RCP(2), year(86), and month (12)
                  ungroup()%>%
                  mutate(year_group = case_when(year %in% (2025-yearbuffer):(2025+yearbuffer) ~ "Baseline",
                                                year %in% (2050-yearbuffer):(2050+yearbuffer) ~ "2050",
                                                year %in% (2075-yearbuffer):(2075+yearbuffer) ~ "2075",
                                                year %in% (2100-yearbuffer) ~ "2100"))%>%
                  filter(!is.na(year_group))%>%
                  left_join(.,combo_data%>%distinct(id,.keep_all=TRUE)%>%dplyr::select(id,NAME,cell_area))%>%
                  left_join(.,site_info%>%rename(NAME = site)%>%dplyr::select(NAME,abbreviation,long,lat,region,area))%>%
                  data.frame()%>%
                  mutate(year_group = factor(year_group,levels=c("Baseline","2050","2075","2100")),
                         region = factor(region,levels=c("Bay of Fundy","Western Scotian Shelf","Eastern Scotian Shelf")),
                         name = factor(abbreviation,levels=.[]%>%filter(climate_proj=="8.5",year==2025)%>%
                                                                 distinct(NAME,.keep_all=TRUE)%>%
                                                                 arrange(region,long)%>%pull(abbreviation)),
                         climate_proj = factor(climate_proj,levels=c("8.5","2.6")))

combo_data_sum <- combo_data%>%
                  group_by(climate_proj,year,id)%>%
                  summarise(temp=mean(temp,na.rm=T))%>% #annual average as there is a id per RCP(2), year(86), and month (12)
                  ungroup()%>%
                  mutate(year_group = case_when(year %in% (2025-yearbuffer):(2025+yearbuffer) ~ "Baseline",
                                                year %in% (2050-yearbuffer):(2050+yearbuffer) ~ "2050",
                                                year %in% (2075-yearbuffer):(2075+yearbuffer) ~ "2075",
                                                year %in% (2100-yearbuffer) ~ "2100"))%>%
                  filter(!is.na(year_group))%>%
                  group_by(climate_proj,id,year_group)%>%
                  summarise(min=min(temp,na.rm=T),
                            max=max(temp,na.rm=T),
                            temp=mean(temp,na.rm=T))%>%#average of the year group
                  ungroup()%>%
                  left_join(.,combo_data%>%distinct(id,.keep_all=TRUE)%>%dplyr::select(id,NAME,cell_area))%>%
                  left_join(.,site_info%>%rename(NAME = site)%>%dplyr::select(NAME,abbreviation,long,lat,region,area))%>%
                  data.frame()%>%
                  mutate(year_group = factor(year_group,levels=c("Baseline","2050","2075","2100")),
                         region = factor(region,levels=c("Bay of Fundy","Western Scotian Shelf","Eastern Scotian Shelf")),
                         name = factor(abbreviation,levels=.[]%>%filter(climate_proj=="8.5",year_group==2050)%>%
                                         distinct(NAME,.keep_all=TRUE)%>%
                                         arrange(region,long)%>%pull(abbreviation)),
                         climate_proj = factor(climate_proj,levels=c("8.5","2.6")))
                  

#assemble the plot using patchwork for each region. 
p1 <- ggplot(combo_data_sum%>%filter(region=="Bay of Fundy"),aes(x=name,y=temp,weight=cell_area,group=interaction(name,climate_proj),fill=climate_proj))+
  geom_boxplot(outlier.size = 0.1)+
  facet_grid(region ~ year_group,scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=6),
        legend.position="none",
        strip.background = element_rect(fill="white"),
        plot.margin = unit(c(0,30,0,0), "pt")
      )+
  labs(x="",y="",col="RCP")+ 
  scale_fill_manual(values=c("coral","cornflowerblue"))
  

p2 <- ggplot(combo_data_sum%>%filter(region=="Western Scotian Shelf"),aes(x=name,y=temp,weight=cell_area,group=interaction(name,climate_proj),fill=climate_proj))+
  geom_boxplot(outlier.size = 0.1)+
  facet_grid(region ~ year_group,scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=6),
        legend.position="none",
        strip.background.y = element_rect(fill="white"),
        strip.background.x = element_blank(),
        strip.text.x = element_blank(),
        plot.margin = unit(c(0,30,0,0), "pt")
        )+
  labs(x="",y=expression("Temperature " ( degree*C)),fill="RCP")+ 
  scale_fill_manual(values=c("coral","cornflowerblue"))

p3 <- ggplot(combo_data_sum%>%filter(region=="Eastern Scotian Shelf"),aes(x=name,y=temp,weight=cell_area,group=interaction(name,climate_proj),fill=climate_proj))+
  geom_boxplot(outlier.size = 0.1)+
  facet_grid(region ~ year_group,scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=6),
        legend.position="bottom",
        strip.background.y = element_rect(fill="white"),
        strip.background.x = element_blank(),
        strip.text.x = element_blank(),
        plot.margin = unit(c(0,30,0,0), "pt")
        )+
  labs(x="",y="",fill="RCP")+ 
  scale_fill_manual(values=c("coral","cornflowerblue"))

p4 <- p1 + p2 + p3 + plot_layout(nrow=3)

ggsave("output/ensemble_temp_rangeplot.png",p4,height=8,width=8,units="in",dpi=600)

#get data for a table 

weighted.median <- function(x, w) { #https://stackoverflow.com/questions/2748725/is-there-a-weighted-median-function
  w <- w[order(x)]
  x <- x[order(x)]
  
  prob <- cumsum(w)/sum(w)
  ps <- which(abs(prob - .5) == min(abs(prob - .5)))
  return(x[ps])
}

combo_data_median <- combo_data%>%
                      group_by(climate_proj,year,id)%>%
                      summarise(temp=mean(temp,na.rm=T))%>% #annual average as there is a id per RCP(2), year(86), and month (12)
                      ungroup()%>%
                      mutate(year_group = case_when(year %in% (2025-yearbuffer):(2025+yearbuffer) ~ "Baseline",
                                                    year %in% (2050-yearbuffer):(2050+yearbuffer) ~ "2050",
                                                    year %in% (2075-yearbuffer):(2075+yearbuffer) ~ "2075",
                                                    year %in% (2100-yearbuffer) ~ "2100"))%>%
                      filter(!is.na(year_group))%>%
                      left_join(.,combo_data%>%distinct(id,.keep_all=TRUE)%>%dplyr::select(id,NAME,cell_area))%>%
                      left_join(.,site_info%>%rename(NAME = site)%>%dplyr::select(NAME,abbreviation,long,lat,region,area))%>%
                      group_by(climate_proj,region,abbreviation,year_group)%>%
                      summarise(med=weighted.median(temp,cell_area))%>%#average of the year group
                      ungroup()%>%
                      data.frame()%>%
                      mutate(year_group = factor(year_group,levels=c("Baseline","2050","2075","2100")),
                             region = factor(region,levels=c("Bay of Fundy","Western Scotian Shelf","Eastern Scotian Shelf")),
                             name = factor(abbreviation,levels=combo_data_sum%>%filter(climate_proj=="8.5",year_group==2050)%>%
                                             distinct(NAME,.keep_all=TRUE)%>%
                                             arrange(region,long)%>%pull(abbreviation)),
                             climate_proj = factor(climate_proj,levels=c("8.5","2.6")))


combo_data_sum_table <- combo_data_sum%>%
                        group_by(climate_proj,region,abbreviation,year_group)%>%
                        summarise(min=min(min,na.rm=T), #among grid cells (id) within site
                                  max=max(max,na.rm=T))%>%
                        ungroup()%>%
                        left_join(.,combo_data_sum%>%distinct(abbreviation,.keep_all=T)%>%dplyr::select(NAME,abbreviation))%>%
                        mutate(year_group = factor(year_group,levels=c("Baseline","2050","2075","2100")),
                               region = factor(region,levels=c("Bay of Fundy","Western Scotian Shelf","Eastern Scotian Shelf")),
                               name = factor(abbreviation,levels=combo_data_sum%>%filter(climate_proj=="8.5",year_group==2050)%>%
                                               distinct(NAME,.keep_all=TRUE)%>%
                                               arrange(region,long)%>%pull(abbreviation)),
                               climate_proj = factor(climate_proj,levels=c("8.5","2.6")))%>%
                        gather("var","val",c("min","max"))

bof <- ggplot(combo_data_sum_table%>%filter(region=="Bay of Fundy"),aes(x=name,y=val,group=interaction(name,climate_proj),fill=climate_proj))+
        geom_line(position = position_dodge(width = 0.6),col=climate_proj)+
        geom_point(position=position_dodge(width=0.6),shape=21)+
        geom_point(data=combo_data_median%>%filter(region=="Bay of Fundy"),aes(x=name,y=med,group=interaction(name,climate_proj)),position=position_dodge(width=0.6),size=0.75,shape=4,show.legend=FALSE)+
        facet_grid(region~year_group,scales="free_y")+
        coord_flip()+
        theme_bw()+
        theme(legend.position="none",
              strip.background = element_rect(fill="white"),
              plot.margin = unit(c(0,30,0,0), "pt"),
              axis.text.x = element_blank()
        )+
        labs(x="",y="",fill="RCP")+ 
        scale_fill_manual(values=c("coral","cornflowerblue"))

wss <- ggplot(combo_data_sum_table%>%filter(region=="Western Scotian Shelf"),aes(x=name,y=val,group=interaction(name,climate_proj),fill=climate_proj))+
        geom_line(position = position_dodge(width = 0.6),col=climate_proj)+
        geom_point(position=position_dodge(width=0.6),shape=21)+
        geom_point(data=combo_data_median%>%filter(region=="Western Scotian Shelf"),aes(x=name,y=med,group=interaction(name,climate_proj)),position=position_dodge(width=0.6),size=0.75,shape=4,show.legend=FALSE)+
        facet_grid(region~year_group,scales="free_y")+
        coord_flip()+
        theme_bw()+
        theme(legend.position="none",
              strip.background.y = element_rect(fill="white"),
              strip.background.x = element_blank(),
              strip.text.x = element_blank(),
              plot.margin = unit(c(0,30,0,0), "pt"),
              axis.text.x = element_blank()
        )+
              labs(x="",y="",fill="RCP")+ 
        scale_fill_manual(values=c("coral","cornflowerblue"))
 
ess <- ggplot(combo_data_sum_table%>%filter(region=="Eastern Scotian Shelf"),aes(x=name,y=val,group=interaction(name,climate_proj),fill=climate_proj))+
  geom_line(position = position_dodge(width = 0.6),col=climate_proj)+
  geom_point(position=position_dodge(width=0.6),shape=21)+
  geom_point(data=combo_data_median%>%filter(region=="Eastern Scotian Shelf"),aes(x=name,y=med,group=interaction(name,climate_proj)),position=position_dodge(width=0.6),size=0.75,shape=4,show.legend=FALSE)+
  facet_grid(region~year_group,scales="free_y")+
  coord_flip()+
  theme_bw()+
  theme(legend.position="bottom",
        strip.background.y = element_rect(fill="white"),
        strip.background.x = element_blank(),
        strip.text.x = element_blank(),
        plot.margin = unit(c(0,30,0,0), "pt")
  )+
  guides(fill = guide_legend(override.aes = list(size=4)))+
  labs(x="",y=expression("Temperature " ( degree*C)),fill="RCP")+ 
  scale_fill_manual(values=c("coral","cornflowerblue")) 
  
plot_props <- combo_data_sum%>%
              distinct(name,.keep_all=T)%>%
              group_by(region)%>%
              summarise(count=n())%>%
              ungroup()%>%
              mutate(prop = count/sum(count))%>%
              data.frame()

p5 <- bof + wss + ess + plot_layout(nrow=3,heights = 12 * tt$prop)

ggsave("output/ENSEMBLE_temperature_dot_temp_range.png",p5, height=8,width=8,units="in",dpi=600)