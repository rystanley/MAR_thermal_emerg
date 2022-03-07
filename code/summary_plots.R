#load libraries
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(rnaturalearth)

sf_use_s2(FALSE)

#Projections ------------
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

#load the toe_summaries
load("output/toe_summaries/all_toe_summaries.RData")

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

western_SS <- setdiff(network$NAME,c(fundy_sites,eastern_SS))

#add the grouping to network cents
network_cents <- network_cents%>%
                  mutate(region=case_when(NAME %in% fundy_sites ~ "Bay of Fundy",
                                          NAME %in% eastern_SS ~ "Eastern Scotian Shelf",
                                          NAME %in% western_SS ~ "Western Scotian Shelf"))%>%
                  arrange(long)
#save output
write.csv(network_cents,"output/network_centriods.csv",row.names=FALSE)

#create sf version for plotting
network_cents_sf <- network_cents%>%st_as_sf(coords=c("long","lat"),crs=latlong)

#add data to the network
network <- network_base%>%
           left_join(.,network_cents)%>%
           st_transform(latlong)

#plot out the centriods on the MPAs
p1 <- ggplot()+
  geom_sf(data=basemap,fill="darkolivegreen3")+
  geom_sf(data=bioregion,fill=NA)+
  geom_sf(data=network,aes(fill=region))+
  geom_sf(data=network_cents_sf,size=1.25)+
  theme_bw()+
  labs(fill="")+
  coord_sf(expand=0)+
  theme(legend.position="bottom")

ggsave("output/regional_centriod_plot.png",p1,width=6,height=6,units="in",dpi=300)

#add the network centriod data to the toe_summaries data file. 
toe_dat <- toe_summaries%>%
                  left_join(.,network%>%
                              filter(NAME %in% unique(toe_summaries$NAME))%>%
                              data.frame()%>%
                              dplyr::select(NAME,long,lat))

## how many species per site
species_count <- toe_dat%>%
                 group_by(NAME)%>%
                 summarise(n_species = length(unique(species)))%>%
                 ungroup()%>%
                 data.frame()%>%
                 arrange(-n_species)


## calculate the time-series of area lost for each species network wide --------

#calculate the total area for each species in the network
agg_network_area <- toe_dat%>%
                  group_by(climate_proj,mod,species)%>% #slightly different for each mod and projection 
                  summarise(total_area=sum(cell_area))%>%
                  ungroup()%>%
                  data.frame()%>%
                  arrange(species)

#so now we will add together grid cells that emerged in the same year
agg_annual_toe <- toe_dat%>%
                  group_by(climate_proj,mod,species,ToE)%>%
                  summarise(area_lost=sum(cell_area))%>%
                  ungroup()%>%
                  data.frame()
                        
species <- unique(agg_annual_toe$species)
years <- 2015:2100
mods <- unique(agg_annual_toe$mod)
projs <- unique(agg_annual_toe$climate_proj)


#Big 'for loop' == this could probably be done using 'do' in dplyr but this doesn't take long and is easy to follow. 

habitat_loss <- NULL #will grow each loop
for(i in species){
  message(paste0("Working on ",i))
  for(p in projs){
    for(m in mods){
     
        temp <- agg_annual_toe%>%
                    filter(species==i,climate_proj == p,mod==m,!is.na(ToE))%>%
                    rbind(.,data.frame(climate_proj=p,mod=m,species=i,ToE=setdiff(years,.$ToE),area_lost=0))%>% #add in the years that are missing as 0 loss years
                    arrange(ToE)%>% #sort them
                    mutate(cum_lost = cumsum(area_lost))%>%
                    left_join(.,agg_network_area)%>%# add in the total area for a given mod, projection and species
                    mutate(prop_lost = cum_lost/total_area)%>%
                    data.frame()%>%suppressMessages()
        
        habitat_loss <- rbind(habitat_loss,temp)

    } #end of 'm' mods loop
  } #end of 'p' climate_proj loop
} #end of 'i' species loop


##plot the area lost summaries

#first average among CMIP models
habitat_loss_ave <- habitat_loss%>%
                    gather(key = "var",value="value",c("area_lost","cum_lost","prop_lost"))%>%
                    filter(!(climate_proj == "2-6"& mod=="GFDL"))%>% #there is no 'GFDL model for RCP 2.6' but during the left_join above it gets assigned a value of NA
                    group_by(climate_proj,species,ToE,var)%>% #this will average the models but note that some models have a 0 in the early years so they pull down the average
                    summarise(mean=mean(value,na.rm=T),
                              sd=sd(value,na.rm=T))%>%
                    ungroup()%>%
                    data.frame()


                              
focal_sp <- c("Amblyraja radiata","Gadus morhua","Homarus americanus")

ggplot(habitat_loss_ave%>%filter(species%in%focal_sp,var=="prop_lost"),aes(x=ToE,y=mean,col=climate_proj))+
  geom_line()+
  facet_grid(species~climate_proj)+
  theme_bw()

ggplot(habitat_loss_ave%>%filter(var=="prop_lost"),aes(x=ToE,y=mean,col=climate_proj,group=species))+
  geom_line()+
  facet_wrap(~climate_proj,ncol=2)+
  theme_bw()


## Species by site 'emergence' based on a threshold of habitat loss 

#total area in each site that is occupied by each species
agg_site_area <- toe_dat%>%
                    group_by(climate_proj,mod,species,NAME)%>% #slightly different for each mod and projection 
                    summarise(total_area=sum(cell_area))%>%
                    ungroup()%>%
                    data.frame()

habitat_loss_site <- toe_dat%>%
                     mutate(ToE = ifelse(is.na(ToE),2500,ToE))%>% #2500 is a placeholder for 'NA' or 'not emerged'
                     group_by(climate_proj,mod,species,NAME,ToE)%>%
                     summarise(area_lost=sum(cell_area))%>%
                     ungroup()%>%
                     left_join(agg_site_area)%>% # add in the total area within each site. 
                     mutate(prop_area=area_lost/total_area)%>%
                     arrange(climate_proj,mod,species,NAME,ToE)%>% #make sure everything is ordered so that ToE's are sequential
                     group_by(climate_proj,mod,species,NAME)%>%
                     mutate(cum_sum=cumsum(prop_area))%>%
                     ungroup()%>%
                     data.frame()
                      

loss_thresholds <- c(0.25,0.5,0.75,1)


                    
                     



### First year of emergence among models, species, among sites --------

first_year <- toe_dat%>%
              filter(!species %in% c("Sebastes mentella","Calanus glacialis"))%>% # these are all gone on day one of our analysis based on their thermal limits. 
              group_by(climate_proj,NAME)%>%
              summarise(first_year = ifelse(sum(is.na(ToE))==length(ToE),NA,min(ToE,na.rm=T)))%>%
              ungroup()%>%
              data.frame()%>%
              left_join(.,network_cents) #merge with the lat and long to see if there is a relationship with the position
              

ggplot(data=first_year,aes(y=long,x=first_year,col=region))+
  geom_point()+
  theme_bw()+
  facet_wrap(~climate_proj,nrow=2)


### By

habitat_loss%>%
  filter(species=="Amblyraja radiata",climate_proj=="2-6",ToE == 2050)
  
habitat_loss_ave%>%
  filter(species=="Amblyraja radiata",climate_proj=="2-6",ToE == 2050)




#calculate the total area within each site for each species, model and climate projection
step1 <- toe_summaries2%>%
          group_by(climate_proj,mod,species,NAME)%>%
          summarise(niche_area=sum(cell_area),
                    site_area=unique(site_area))%>%
          ungroup()%>%
          data.frame()

#get the niche area for the entire network for each species, model and climate projection
step2 <- step1%>%
            group_by(climate_proj,mod,species)%>%
            summarise(niche_area=sum(niche_area))%>%
            ungroup()%>%
            data.frame()

years <- 2015:2100
climate_proj <- unique(step1$climate_proj)
          
for (i in years){
  
  
  
  
  
}

          

## Summmaries of the first TOE within the sites

sum_toe <- toe_summaries2%>%
            group_by(climate_proj,mod,species,NAME)%>%
            summarise(first_year=min(ToE,na.rm=T),
                   mean_year=mean(ToE,na.rm=T),
                   mean_weighted=weighted.mean(ToE,cell_area,na.rm=T),
                   last_year=max(ToE,na.rm=T))%>%
            ungroup()%>%
            data.frame()%>%
            mutate(first_year=replace(first_year,is.infinite(first_year),NA), #clean up inf values for unemerged cells
                  mean_year=replace(mean_year,is.infinite(mean_year),NA),
                  mean_weighted=replace(mean_weighted,is.infinite(mean_weighted),NA),
                  last_year=replace(last_year,is.infinite(last_year),NA))%>%
            suppressWarnings()%>%#these are just for the NA calculations
            suppressMessages() # this is for the grouping auto message
            

sum_mod_toe <- sum_toe%>%
                group_by(climate_proj,species,NAME)%>% #average among models
                summarise(mean_first=mean(first_year,na.rm=T), #earliest projected
                          mean_year=mean(mean_year,na.rm=T), #mean projected
                          mean_weighted=mean(mean_weighted,na.rm=T), #mean (weighted) projected
                          mean_last=mean(last_year,na.rm=T))%>% #max projected
                ungroup()%>%
                data.frame()%>%
                  mutate(mean_first=replace(mean_first,is.infinite(mean_first),NA), #clean up inf values for unemerged cells
                         mean_year=replace(mean_year,is.infinite(mean_year),NA),
                         mean_weighted=replace(mean_weighted,is.infinite(mean_weighted),NA),
                         mean_last=replace(mean_last,is.infinite(mean_last),NA))%>%
                  suppressWarnings()%>%#these are just for the NA calculations
                  suppressMessages() # this is for the grouping auto message

#data for a plot

#factor levels -- arrange by first to last ToE within species and among projections and sites
tt=sum_mod_toe%>%
  group_by(species)%>%
  summarise(min=min(mean_year,na.rm=T))%>%
  ungroup()%>%
  arrange(min)

p1_data <- sum_mod_toe%>%
           filter(!is.na(mean_first),!is.na(mean_year),!is.na(mean_weighted),!is.na(mean_last))%>%
           mutate(species_factor = factor(species,levels = ))


p1_da


sum_network_toe <- sum_mod_toe%>%
                   group_by(species)%>%
                    summarise(mean_first=mean(mean_first,na.rm=T),
                              mean_year=mean(mean_year,na.rm=T),
                              mean_weighted=mean(mean_weighted,na.rm=T),
                              mean_last=max(mean_last,na.rm=T))%>%
                    ungroup()%>%
                    data.frame()

sum_toe_spatial <- sum_toe%>%
                   left_join(.,network%>%
                               filter(NAME %in% unique(sum_toe$NAME))%>% #some sites aren't considered as they are outside of all the species ranges (depth, Bras d'Or, or have never been observed within the buffered distance of a site)
                               dplyr::select(NAME,geometry))





