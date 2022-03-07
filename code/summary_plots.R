#load libraries
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(scales)

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


## Species by site 'emergence' based on a threshold of habitat loss summary data -----------

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

## evaluate the total amount of species lost (no remaining habitat) by 2100 aggregated by general area --------------

      species_lost_site <- habitat_loss_site%>%
                            filter(ToE != 2500, #these are species that did not emerge
                                   mod!="GFDL", #no GFDL for 2-6
                                   !species%in%c("Sebastes mentella","Calanus glacialis"),
                                   cum_sum==1)%>%
                            group_by(climate_proj,mod,NAME)%>%
                            summarise(count=n())%>%
                            ungroup()%>%
                            
                            left_join(.,species_count)%>%
                            mutate(prop_lost = count/n_species)%>%
                            data.frame()
      
      #species lost by 2100
        species_lost_site_format <- species_lost_site%>%
                                    select(climate_proj,mod,NAME,prop_lost)%>%
                                    spread(mod,prop_lost)%>% #this just reformats the data so that models are represented as columns
                                    rowwise()%>%
                                    mutate(mean=mean(c(AWI,HAD,IPSL),na.rm=T),
                                           sd=sd(c(AWI,HAD,IPSL),na.rm=T),
                                           comp = pmax(AWI,HAD,IPSL,na.rm=T))%>% #which model has the most lost
                                    data.frame()%>%
                                    left_join(.,network_cents%>%select(NAME,long,region))
        
        #plot data
        plotdata_comp <- species_lost_site_format%>%
                          mutate(NAME = factor(NAME,levels=species_lost_site_format%>%filter(climate_proj == "8-5")%>%arrange(comp)%>%pull(NAME)),
                                 facet_lab = ifelse(climate_proj == "2-6","RCP 2.6","RCP 8.5"))
      
        #plot
        p_comp <- ggplot(data=plotdata_comp,aes(x=comp,y=NAME,fill=region))+
                  geom_bar(stat="identity",col="black")+
                  facet_wrap(~facet_lab,ncol=2)+
                  theme_bw()+
                  labs(x="Lost species",y="",fill="")+
                  scale_x_continuous(labels=percent)+
                    theme(legend.position = "bottom");p_comp

        #save the plot
        ggsave("output/species_lost_2100.png",p_comp,width=9,height=6,units="in",dpi=300)    
                      

#benchmark years for the analysis to assess the impact of climate change on different species and sites. 
benchmark_years <- c(2025,2050,2075,2100)


species_lost_benchmark <- NULL
for(i in benchmark_years){
  
  species_lost_benchmark <- rbind(species_lost_benchmark,
            
            habitat_loss_site%>%
            filter(ToE != 2500, #these are species that did not emerge
                   mod!="GFDL", #no GFDL for 2-6
                   !species%in%c("Sebastes mentella","Calanus glacialis"),
                   cum_sum==1,
                   ToE<=i)%>%
            group_by(climate_proj,mod,NAME)%>%
            summarise(count=n())%>%
            ungroup()%>%
            
            left_join(.,species_count)%>%
            mutate(prop_lost = count/n_species,
                   benchmark=i)%>%
            data.frame()
            
  )#end of loop growing dataframe
}

plot_benchmark_formatted <- species_lost_benchmark%>%
                            group_by(climate_proj,NAME,benchmark)%>%
                            summarise(comp=max(prop_lost,na.rm=T),
                                      mean=mean(prop_lost,na.rm=T),
                                      sd=sd(prop_lost,na.rm=T))%>%
                            ungroup()%>%
                            mutate(facet_lab = ifelse(climate_proj == "2-6","RCP 2.6","RCP 8.5"))

plot_benchmark_formatted <- plot_benchmark_formatted%>%
                            mutate(NAME = factor(NAME,levels=plot_benchmark_formatted%>%filter(climate_proj == "8-5",benchmark==2100)%>%arrange(comp)%>%pull(NAME)))

benchmark_plot <-  ggplot(,aes(x=comp,y=NAME,fill=factor(benchmark)))+
              geom_bar(data=plot_benchmark_formatted%>%filter(benchmark==2100),stat="identity",col="black")+
              geom_bar(data=plot_benchmark_formatted%>%filter(benchmark==2075),stat="identity",col="black")+
              geom_bar(data=plot_benchmark_formatted%>%filter(benchmark==2050),stat="identity",col="black")+
              geom_bar(data=plot_benchmark_formatted%>%filter(benchmark==2025),stat="identity",col="black")+
              facet_wrap(~facet_lab,ncol=2)+
              theme_bw()+
              labs(x="Lost species",y="",fill="")+
              scale_x_continuous(labels=percent)+
              theme(legend.position = "bottom")+
              scale_fill_viridis(discrete=T); benchmark_plot

ggsave("output/benchmark_species_lost.png",benchmark_plot,width=9,height=6,units="in",dpi=300)


           
                     

#Now evaluate how much each species has lots at each of the benchmark years (similar to the plot for the % of species lost)

total_area <- toe_dat%>%
              filter(climate_proj == )

species_area_lost_benchmark <- NULL
for(i in benchmark_years){
  
  species_area_lost_benchmark <- rbind(species_area_lost_benchmark,
                                  
                                  habitat_loss_site%>%
                                    filter(ToE != 2500, #these are species that did not emerge
                                           mod!="GFDL", #no GFDL for 2-6
                                           !species%in%c("Sebastes mentella","Calanus glacialis"),
                                           ToE<=i)%>%
                                    group_by(climate_proj,mod,species)%>%
                                    mutate(sum_area = cumsum(area_lost))%>%
                                    summarise(area_lost = max(sum_area,na.rm=T))%>%
                                    ungroup()%>%
                                    data.frame()%>%
                                    left_join(.,agg_network_area)%>% # the total area for each species across the hole network ~ mod, climate_proj
                                    mutate(prop_lost = area_lost/total_area,
                                           benchmark=i)%>%
                                    data.frame()
                                  
  )#end of loop growing dataframe
}


plot_area_benchmark_formatted <- species_area_lost_benchmark%>%
                                group_by(climate_proj,species,benchmark)%>%
                                summarise(comp=max(prop_lost,na.rm=T),
                                          mean=mean(prop_lost,na.rm=T),
                                          sd=sd(prop_lost,na.rm=T))%>%
                                ungroup()%>%
                                mutate(facet_lab = ifelse(climate_proj == "2-6","RCP 2.6","RCP 8.5"))

plot_area_benchmark_formatted <- plot_area_benchmark_formatted%>%
                                 mutate(species = factor(species,levels=plot_area_benchmark_formatted%>%filter(climate_proj == "8-5",benchmark==2100)%>%arrange(comp)%>%pull(species)))

benchmark_area_plot <-  ggplot(,aes(x=comp,y=species,fill=factor(benchmark)))+
                        geom_bar(data=plot_area_benchmark_formatted%>%filter(benchmark==2100),stat="identity",col="black")+
                        geom_bar(data=plot_area_benchmark_formatted%>%filter(benchmark==2075),stat="identity",col="black")+
                        geom_bar(data=plot_area_benchmark_formatted%>%filter(benchmark==2050),stat="identity",col="black")+
                        geom_bar(data=plot_area_benchmark_formatted%>%filter(benchmark==2025),stat="identity",col="black")+
                        facet_wrap(~facet_lab,ncol=2)+
                        theme_bw()+
                        labs(x="Lost habitat extent",y="",fill="")+
                        scale_x_continuous(labels=percent)+
                        theme(legend.position = "bottom")+
                        scale_fill_viridis(discrete=T); benchmark_area_plot

ggsave("output/benchmark_species_area_lost.png",benchmark_area_plot,width=9,height=6,units="in",dpi=300)







