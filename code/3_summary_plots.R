#load libraries
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(scales)
library(patchwork)
library(ggspatial)
library(viridis)

sf_use_s2(FALSE)

#Projections ------------
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

#load the time of emergence summaries
load("output/toe_summaries/all_toe_summaries.RData")

toe_summaries <- toe_summaries%>%
                 filter(mod != "GFDL")%>% # we are not using the GFDL model
                 mutate(climate_proj = gsub("\\.","-",climate_proj))# the . was left in from the ensemble calculation


#load the species groupings and niche information
groupings <- read.csv("data/species_grouping.csv")%>%select(SciName,functional_group,cosewic_status)
niches <- read.csv("data/species_niche_final.csv")%>%
          left_join(.,groupings)

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
                              data.frame()%>%
                              dplyr::select(NAME,long,lat))


## create short names for draft site (used in later plotting)
site_names <- unique(toe_dat$NAME)%>%
              gsub(" - "," ",.)%>% #clean up the odds and ends
              gsub("-","",.)%>%
              gsub(" and "," ",.)%>%
              gsub("/"," ",.)%>%
              data.frame(NAME=unique(toe_dat$NAME),
                         abbreviation=sapply(strsplit(.," "), function(x){ #Function splits, then getes the first letter of each unique word
                           toupper(paste(substring(x, 1, 1), collapse = ""))}))%>%
              mutate(abbreviation = ifelse(NAME =="Bird Islands","BRDI",abbreviation), #bird island and brier island get the same 'BI'
                     abbreviation = ifelse(NAME =="Chebogue","CHEB",abbreviation))%>% # Chebogue had a boring acronym
              select(NAME,abbreviation)%>%
              arrange(abbreviation)

#plot out the centriods on the MPAs ------

#add the abbreviation names 
network_cents_sf <- network_cents_sf%>%
                    left_join(.,site_names)

#show a grid that represents the ~ resolution of CMIP (from readme)
study_grid <- bioregion%>%st_make_grid(cellsize=0.25)
bioregion_grid <- bioregion%>%st_intersection(study_grid)
sites_grid <- network%>%
  st_make_valid()%>%
  st_intersection(study_grid)


p1 <- ggplot()+
  geom_sf(data=basemap,fill="grey80")+
  geom_sf(data=bioregion,fill=NA)+
  geom_sf(data=sites_grid,aes(fill=region))+
  geom_sf(data=network_cents_sf,size=1.25)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  labs(fill="")+
  coord_sf(expand=0)+
  theme(legend.position="bottom")+
  annotation_scale(location="br")+ #location is 'bottom right'
  annotation_north_arrow(location="tl",height = unit(0.3,"in"),width=unit(0.3,"in")) 

ggsave("output/regional_centriod_plot.png",p1,width=8,height=8,units="in",dpi=300)


#quick and dirty plot to add labels. 

p1+geom_sf_label_repel(data=network_cents_sf,aes(label=abbreviation))

p1_labs <- ggplot()+
  geom_sf(data=basemap,fill="darkolivegreen3")+
  geom_sf(data=network_cents_sf,size=1.25)+
  geom_sf_label(data=network_cents_sf,aes(label=abbreviation),label.size = 0.1)+ #this type of thing is notoriously hard to do. there is a package you can try with the function geom_sf_label_repel() https://yutannihilation.github.io/ggsflabel/reference/geom_sf_label.html
  theme_bw()+
  labs(fill="")+
  coord_sf(expand=0)+
  theme(legend.position="bottom")

ggsave("output/sites_with_labs.png",p1_labs,width=6,height=6,units="in",dpi=300)


## create inset

nw_atlantic_inset <- st_bbox(c(xmin = -72 , xmax = -47.9, ymax = 61, ymin = 39.8 ),crs=latlong)%>%
  st_as_sfc()%>%st_bbox()%>%st_as_sfc() #this is for ~ the Canadian NW Atlantic

nw_box_inset <- st_bbox(nw_atlantic)

basemap_inset <- rbind(ne_states(country = "Canada",returnclass = "sf")%>%
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
                            mutate(country="USA"))%>%
                    st_intersection(.,nw_atlantic_inset)

p1_inset <- ggplot()+
  geom_sf(data=bioregion,fill=NA)+
  geom_sf(data=basemap_inset,fill="grey80")+
  coord_sf(expand=0,xlim=nw_box_inset[c(1,3)],ylim=nw_box_inset[c(2,4)])+
  theme_bw()+
  theme(panel.grid = element_blank())
#annotation_north_arrow(location="tr")

ggsave("output/obis_range.png",p1_inset,height=6,width=5,units="in",dpi=300)




##Data prep species,site summaries ------------

    ## how many species per site
    species_count <- toe_dat%>%
                     group_by(NAME)%>%
                     summarise(n_species = length(unique(species)))%>%
                     ungroup()%>%
                     data.frame()%>%
                     arrange(-n_species)
    
    ## species unique to each site
    species_site <- toe_dat%>%
                    group_by(NAME)%>%
                    distinct(species)%>%
                    ungroup()%>%
                    data.frame()%>%
                    arrange(species)%>%
                    mutate(id=paste(species,NAME,sep="-"))
    
    ## species dummy data frame
    sd1<- niches%>%
               filter(!SciName %in% c("Sebastes mentella","Calanus glacialis"))%>%
               select(SciName,functional_group,cosewic_status)%>%
               rename(species=SciName)
    
    sd2 <- rbind(sd1,sd1,sd1,sd1)%>%
                      mutate(benchmark=rep(rep(c(2025,2050,2075,2100),each=nrow(sd1))))
    
    species_dummy <- rbind(sd2,sd2)%>%
                    mutate(climate_proj = rep(c("2-6","8-5"),each=nrow(sd2)),
                           id=paste(climate_proj,benchmark,species,sep="-"))
                                                     


## calculate the time-series of area lost for each species network wide 
    site_df <- site_names%>%
               left_join(.,species_count)%>%
               left_join(.,network_cents)%>%
               left_join(.,network%>%
                          group_by(NAME)%>%
                           summarise(area=round(as.numeric(st_area(geometry))/1000/1000,2))%>%
                           data.frame()%>%
                           select(NAME,area)%>%
                           suppressMessages())%>%#suppress the planar coordinate warning. 
               rename(site=NAME)%>%
               arrange(region,long)
    
    write.csv(site_df,"output/site_description_table.csv",row.names=F)
    
    #dummy dataframe that can be used to buffer the site summaries
    site_dummy <- rbind(site_df%>%select(site,long,abbreviation,region)%>%mutate(climate_proj="2-6"),
                        site_df%>%select(site,long,abbreviation,region)%>%mutate(climate_proj="8-5"))%>%
                  rename(NAME = site)%>%
                  mutate(id = paste(NAME,climate_proj,sep="-"))


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
                        

#extract the summaries of habitat loss (e.g., when a cell becomes too warm) ----------------------------

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


##model averaged propotion of habtiat loss per species aggregated for the network -------------------------

#first average among CMIP models
habitat_loss_ave <- habitat_loss%>%
                    gather(key = "var",value="value",c("area_lost","cum_lost","prop_lost"))%>%
                    filter(mod == "Ensemble")%>%
                    rename(mean = value)
                    group_by(climate_proj,species,ToE,var)%>% #this will average the models but note that some models have a 0 in the early years so they pull down the average
                    summarise(mean=mean(value,na.rm=T),
                              sd=sd(value,na.rm=T))%>%
                    ungroup()%>%
                    data.frame()

#format for the plot
network_loss_plot_df <- habitat_loss_ave%>%
                        filter(var=="prop_lost",
                               !species %in% c("Sebastes mentella","Calanus glacialis"))%>%
                        mutate(facet_lab = ifelse(climate_proj == "2-6","RCP 2.6","RCP 8.5"),
                               benchmark = case_when(ToE<=2025 ~ 2025,
                                                     ToE>2025 & ToE<=2050 ~2050,
                                                     ToE>2050 & ToE<=2075 ~ 2075,
                                                     ToE>2075 ~ 2100))

#take the mean of mean models among species
network_loss_mean <- habitat_loss_ave%>%
                     filter(!species %in% c("Sebastes mentella","Calanus glacialis"))%>%
                     group_by(climate_proj,ToE,var)%>%
                     summarise(sd=sd(mean,na.rm=T),
                               mean=mean(mean,na.rm=T))%>%
                     ungroup()%>%
                     data.frame()%>%
                     filter(var=="prop_lost")%>%
                      mutate(facet_lab = ifelse(climate_proj == "2-6","RCP 2.6","RCP 8.5"),
                             benchmark = case_when(ToE<=2025 ~ 2025,
                                                   ToE>2025 & ToE<=2050 ~2050,
                                                   ToE>2050 & ToE<=2075 ~ 2075,
                                                   ToE>2075 ~ 2100))

focal_loss_years <- c(2025,2050,2075)

network_loss_plot <- ggplot()+
                      geom_line(data=network_loss_plot_df,aes(x=ToE,y=mean,group=species),col="grey80",lty=2,lwd=0.5)+
                      geom_line(data=network_loss_mean,lwd=2,aes(x=ToE,y=mean,col=factor(benchmark)))+
                      geom_segment(data=network_loss_mean%>%filter(ToE %in% focal_loss_years),
                                   aes(x=-Inf,xend=ToE,y=mean,yend=mean,col=factor(benchmark)),lwd=1.25)+
                      geom_segment(data=network_loss_mean%>%filter(ToE %in% focal_loss_years),
                                   aes(x=ToE,xend=ToE,y=-Inf,yend=mean,col=factor(benchmark)),lwd=1.25)+
                      facet_wrap(~facet_lab,ncol=2)+
                      theme_bw()+
                      labs(x="",y="Propotion of habitat")+
                      scale_y_continuous(labels=percent)+
                      theme(legend.position = "none",
                            strip.background = element_rect(fill="white"))+
                      scale_color_viridis(discrete = T);network_loss_plot

ggsave("output/ENSEMBLE_habitat_loss-models_species.png",network_loss_plot,width=8,height=6,units="in",dpi=300)


## Species by site 'emergence' based on a threshold of habitat loss summary data -----------------------

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

## Species loss by 2100 aggregated for the network -------------------------

      species_lost_site <- habitat_loss_site%>%
                            filter(ToE != 2500, #these are species that did not emerge
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
                                    mutate(comp = Ensemble)%>% #just select the ensemble 
                                    data.frame()%>%
                                    left_join(.,network_cents%>%select(NAME,long,region))
        
        #plot data
        plotdata_comp <- species_lost_site_format%>%
                          mutate(region = factor(region,levels=c("Bay of Fundy","Western Scotian Shelf","Eastern Scotian Shelf")))%>%
                          arrange(climate_proj,region,long)%>%
                          mutate(id=paste(NAME,climate_proj,sep="-"),
                                 comp=ifelse(is.na(comp),0,comp))%>%
                          select(NAME,region,climate_proj,comp,id,long)%>%
                          left_join(.,site_names)
        
        plotdata_comp <- rbind(plotdata_comp,
                                  site_dummy%>% #add the sites that are missing and didn't loose any 
                                  filter(id %in% setdiff(site_dummy$id,plotdata_comp%>%pull(id)))%>%
                                  mutate(comp=0)%>%
                                  select(NAME,region,climate_proj,comp,id,long,abbreviation))%>%
                          arrange(climate_proj,region,long)%>%
                          mutate(facet_lab = ifelse(climate_proj == "2-6","RCP 2.6","RCP 8.5"),
                                 NAME = factor(NAME,levels=.[]%>%filter(climate_proj=="8-5")%>%arrange(region,comp)%>%pull(NAME)),
                                 abbreviation = factor(abbreviation,levels=.[]%>%filter(climate_proj=="8-5")%>%arrange(region,comp)%>%pull(abbreviation)))
      
        #plot
        p_comp <- ggplot(data=plotdata_comp,aes(x=comp,y=abbreviation,fill=region))+
                  geom_bar(stat="identity",col="black")+
                  facet_wrap(~facet_lab,ncol=2)+
                  theme_bw()+
                  labs(x="Lost species",y="",fill="")+
                  scale_x_continuous(labels=percent,breaks=c(0,0.25,0.5,0.75),limits = c(0,round(max(plotdata_comp$comp),1)),expand=c(0,0))+
                    theme(legend.position = "bottom",
                          axis.text.x=element_text(size=6),
                          strip.background = element_rect(fill="white"));p_comp

        #save the plot
        ggsave("output/ENSEMBLE_species_lost_2100.png",p_comp,width=9,height=6,units="in",dpi=300)    
        
        #make a range plot 
        
        species_lost_site_format_rng <- species_lost_site%>%
                                        select(climate_proj,mod,NAME,prop_lost)%>%
                                        spread(mod,prop_lost)%>% #this just reformats the data so that models are represented as columns
                                        rowwise()%>%
                                        mutate(comp = Ensemble,
                                               min = pmin(AWI,HAD,IPSL,na.rm=T),
                                               max = pmax(AWI,HAD,IPSL,na.rm=T))%>% #just select the ensemble 
                                        data.frame()%>%
                                        left_join(.,network_cents%>%select(NAME,long,region))
        
        plotdata_comp_rng <- species_lost_site_format_rng%>%
                                  mutate(region = factor(region,levels=c("Bay of Fundy","Western Scotian Shelf","Eastern Scotian Shelf")))%>%
                                  arrange(climate_proj,region,long)%>%
                                  mutate(id=paste(NAME,climate_proj,sep="-"),
                                         comp=ifelse(is.na(comp),0,comp))%>%
                                  select(NAME,region,climate_proj,comp,min,max,id,long)%>%
                                  left_join(.,site_names)
        
        
        plotdata_comp_rng <- rbind(plotdata_comp_rng,
                               site_dummy%>% #add the sites that are missing and didn't loose any 
                                 filter(id %in% setdiff(site_dummy$id,plotdata_comp_rng%>%pull(id)))%>%
                                 mutate(comp=0,min=NA,max=NA)%>%
                                 select(NAME,region,climate_proj,comp,min,max,id,long,abbreviation))%>%
                              arrange(climate_proj,region,long)%>%
                              mutate(facet_lab = ifelse(climate_proj == "2-6","RCP 2.6","RCP 8.5"),
                                     NAME = factor(NAME,levels=.[]%>%filter(climate_proj=="8-5")%>%arrange(region,comp)%>%pull(NAME)),
                                     abbreviation = factor(abbreviation,levels=.[]%>%filter(climate_proj=="8-5")%>%arrange(region,comp)%>%pull(abbreviation)))
        
        
        p_comp_rng <- ggplot(data=plotdata_comp_rng,)+
                      geom_segment(aes(x=min,xend=comp,y=abbreviation,yend=abbreviation))+
                      geom_segment(aes(x=comp,xend=max,y=abbreviation,yend=abbreviation))+
                      geom_point(aes(x=comp,y=abbreviation,col=region),size=2.5)+
                        facet_wrap(~facet_lab,ncol=2)+
                        theme_bw()+
                        labs(x="Lost species",y="",col="")+
                        scale_x_continuous(labels=percent,breaks=c(0,0.25,0.5,0.75),limits = c(0,round(max(plotdata_comp$comp),1)),expand=c(0,0))+
                        theme(legend.position = "bottom",
                              axis.text.x=element_text(size=6),
                              strip.background = element_rect(fill="white"));p_comp_rng
        
        ggsave("output/ENSEMBLE_species_lost_2100_range.png",p_comp_rng,width=9,height=6,units="in",dpi=300)
        write.csv(plotdata_comp_rng,file="output/ENSEMBLE_species_lost_range.csv",row.names=F)              

## Species by site loss for benchmark years  --------------------------------------
benchmark_years <- c(2025,2050,2075,2100)


species_lost_benchmark <- NULL
for(i in benchmark_years){
  
  species_lost_benchmark <- rbind(species_lost_benchmark,
            
            habitat_loss_site%>%
            filter(ToE != 2500, #these are species that did not emerge
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

##make the data for plotting

benchmark_buffer  <-  rbind(site_dummy,site_dummy,site_dummy,site_dummy)%>% # four times, on dummy dataframe for each benchmark year
                    mutate(benchmark = rep(benchmark_years,each=nrow(site_dummy)),
                           max=NA,
                           min=NA,
                           comp=0,
                           id=paste(climate_proj,NAME,benchmark,sep="-"))


plot_benchmark_formatted <- species_lost_benchmark%>%
                            group_by(climate_proj,NAME,benchmark)%>%
                            summarise(max=max(prop_lost,na.rm=T), #min among models
                                      min=min(prop_lost,na..rm=T), #max among models
                                      comp=prop_lost[mod == "Ensemble"])%>%
                            ungroup()%>%
                            mutate(id=paste(climate_proj,NAME,benchmark,sep="-"))%>%
                            left_join(.,site_names)%>%
                            left_join(.,network_cents%>%select(NAME,region,long))%>%
                            dplyr::select(names(benchmark_buffer)) #line up the rows for the rbind() next
                            

plot_benchmark_formatted <- rbind(plot_benchmark_formatted,
                                  benchmark_buffer%>%
                                    filter(id %in% setdiff(benchmark_buffer$id,plot_benchmark_formatted$id)))%>%
                              mutate(facet_lab = ifelse(climate_proj == "2-6","RCP 2.6","RCP 8.5"),
                                    NAME = factor(NAME,levels=.[]%>%filter(climate_proj == "8-5",benchmark==2100)%>%arrange(region,comp)%>%pull(NAME)),
                                    abbreviation = factor(abbreviation,levels=.[]%>%filter(climate_proj == "8-5",benchmark==2100)%>%arrange(region,comp)%>%pull(abbreviation)))%>%
                             data.frame()
            

benchmark_plot <-  ggplot(,aes(x=comp,y=abbreviation,fill=factor(benchmark)))+
                    geom_bar(data=plot_benchmark_formatted%>%filter(benchmark==2100),stat="identity",col="black")+
                    geom_bar(data=plot_benchmark_formatted%>%filter(benchmark==2075),stat="identity",col="black")+
                    geom_bar(data=plot_benchmark_formatted%>%filter(benchmark==2050),stat="identity",col="black")+
                    geom_bar(data=plot_benchmark_formatted%>%filter(benchmark==2025),stat="identity",col="black")+
                    facet_grid(region~facet_lab,space="free_y",scales="free_y")+
                    theme_bw()+
                    labs(x="Lost species",y="",fill="")+
                    scale_x_continuous(labels=percent,breaks=c(0,0.25,0.5,0.75),limits = c(0,round(max(plot_benchmark_formatted$comp),1)),expand=c(0,0))+
                    theme(legend.position = "bottom",
                          strip.text.y = element_text(size=6),
                          axis.text.x = element_text(size=6))+
                    scale_fill_viridis(discrete=T); benchmark_plot

ggsave("output/ENSEMBLE_benchmark_species_lost.png",benchmark_plot,width=9,height=6,units="in",dpi=300)
write.csv()

#create dataframe for Shaylyn to make maps from 

species_lost_df <- plot_benchmark_formatted%>%
                   select(-id)%>%
                   spread(benchmark,comp)

save(species_lost_df,file="data/ENSEMBLE_species_lost_df.RData")
write.csv(species_lost_df,file="output/ENSEMBLE_species_lost.csv",row.names=FALSE)
                     

##Species area loss plot --------------------------------------


species_area_lost_benchmark <- NULL
for(i in benchmark_years){
  
  species_area_lost_benchmark <- rbind(species_area_lost_benchmark,
                                  
                                  habitat_loss_site%>%
                                    filter(ToE != 2500, #these are species that did not emerge
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
                                summarise(max=max(prop_lost,na.rm=T), #min among models
                                          min=min(prop_lost,na..rm=T), #max among models
                                          comp=prop_lost[mod == "Ensemble"])%>%
                                ungroup()%>%
                                mutate(id = paste(climate_proj,benchmark,species,sep="-"))

plot_area_benchmark_formatted <- rbind(plot_area_benchmark_formatted,
                                       species_dummy%>%
                                         filter(id %in% setdiff(species_dummy$id,plot_area_benchmark_formatted$id))%>%
                                         mutate(max=NA,
                                                min=NA,
                                                comp=0)%>%
                                         select(names(plot_area_benchmark_formatted)))%>%
                                 left_join(.,niches%>%rename(species=SciName)%>%select(species,functional_group))%>%
                                mutate(facet_lab = ifelse(climate_proj == "2-6","RCP 2.6","RCP 8.5"),
                                       functional_group = factor(functional_group,levels=rev(c("Mammals/Reptiles","Copepod","Benthic Invertebrates",
                                                                                           "Pelagic Fish/Cepholopods","Benthic fish"))))%>%
                                data.frame()

#set plotting levels
plot_area_benchmark_formatted$species <- factor(plot_area_benchmark_formatted$species,
                                                levels=plot_area_benchmark_formatted%>%
                                                        filter(benchmark==2100,climate_proj=="8-5")%>%
                                                        arrange(functional_group,comp)%>%pull(species))  

benchmark_area_plot <-  ggplot(,aes(x=comp,y=species,fill=factor(benchmark)))+
                        geom_bar(data=plot_area_benchmark_formatted%>%filter(benchmark==2100),stat="identity",col="black")+
                        geom_bar(data=plot_area_benchmark_formatted%>%filter(benchmark==2075),stat="identity",col="black")+
                        geom_bar(data=plot_area_benchmark_formatted%>%filter(benchmark==2050),stat="identity",col="black")+
                        geom_bar(data=plot_area_benchmark_formatted%>%filter(benchmark==2025),stat="identity",col="black")+
                        facet_grid(functional_group~facet_lab,scales="free_y",space="free_y")+
                        theme_bw()+
                        labs(x="Lost habitat extent",y="",fill="")+
                       scale_x_continuous(labels=percent,breaks=c(0,0.25,0.5,0.75,1),
                                          expand=c(0,0.01))+
                        theme(legend.position = "bottom",
                              strip.background.y = element_blank(),
                              strip.text.y = element_blank(),
                              axis.text.x = element_text(size=6,colour="black"),
                              panel.spacing.x = unit(4, "mm"))+
                         scale_fill_viridis(discrete=T); benchmark_area_plot

ggsave("output/ENSEMBLE_benchmark_species_area_lost.png",benchmark_area_plot,width=9,height=6,units="in",dpi=300)
write.csv(plot_area_benchmark_formatted,file="output/ENSEMBLE_species_area_lost.csv",row.names=FALSE)

### Loss of habitat by MPA summary tile plot --------------------------------------

#this is so we can fill in the blanks
tb1 <- rbind(species_site%>%mutate(prop_lost=0,benchmark=2025),
             species_site%>%mutate(prop_lost=0,benchmark=2050),
             species_site%>%mutate(prop_lost=0,benchmark=2075),
             species_site%>%mutate(prop_lost=0,benchmark=2100))%>%
        filter(!species%in%c("Sebastes mentella","Calanus glacialis"))

tile_buffer <- rbind(tb1,tb1)%>%
                mutate(climate_proj = rep(c("2-6","8-5"),each=nrow(tb1)),
                       id=paste(climate_proj,benchmark,species,NAME,sep="-"))%>% #unique ID we can use to pad the actual data extracted below
                select(climate_proj,NAME,species,prop_lost,benchmark,id)

tile_df <- NULL
for(i in benchmark_years){
  
  tile_df <- rbind(tile_df,
                                       
                   habitat_loss_site%>%
                   filter(ToE != 2500, #these are species that did not emerge
                          !species%in%c("Sebastes mentella","Calanus glacialis"),
                          ToE<=i)%>%
                   group_by(climate_proj,mod,NAME,species)%>%
                   mutate(sum_area = cumsum(area_lost))%>%
                   summarise(area_lost = max(sum_area,na.rm=T))%>%
                   ungroup()%>%
                   data.frame()%>%
                   left_join(.,agg_site_area)%>% # the total area for each species across the hole network ~ mod, climate_proj
                                mutate(prop_lost = area_lost/total_area,
                                       benchmark=i)%>%
                    data.frame()
                                       
  )#end of loop growing dataframe
}


tile_df_formatted <- tile_df%>%
                     filter(mod=="Ensemble")%>% #just focusing on this model
                     mutate(id=paste(climate_proj,benchmark,species,NAME,sep="-"))%>%
                     select(names(tile_buffer))

tile_df_formatted <- rbind(tile_df_formatted,
                           tile_buffer%>%filter(id %in% setdiff(tile_buffer$id,tile_df_formatted$id)))%>%#pad the dataframe
                     mutate(prop_lost = round(prop_lost,3)*100, #put into the xx.y % format
                            benchmark = factor(benchmark,levels = rev(benchmark_years)),
                            facet_lab = ifelse(climate_proj == "2-6","RCP 2.6","RCP 8.5"))%>% 
                    left_join(.,site_names)%>%
                    left_join(.,network_cents)


tile_df_formatted$species_ord <- factor(tile_df_formatted$species,levels=plot_area_benchmark_formatted%>% #using the same data so we can use this again
                                                                        filter(benchmark==2100,climate_proj=="8-5")%>%
                                                                        arrange(functional_group,-comp)%>%pull(species))

#grouping order of the stations (grouped by longitude within their respective regions)
site_order <- network_cents%>%
              mutate(region_ord = factor(region,levels=c("Bay of Fundy","Western Scotian Shelf","Eastern Scotian Shelf")))%>% #order the regions first
              arrange(region_ord,long)%>%
              left_join(.,tile_df_formatted%>%distinct(NAME,.keep_all=TRUE)%>%select(NAME,abbreviation))%>%
              pull(abbreviation)
                       

tile_df_formatted$abbreviation_ord <- factor(tile_df_formatted$abbreviation,levels=site_order)
tile_df_formatted$region_ord <- factor(tile_df_formatted$region,levels=c("Bay of Fundy","Western Scotian Shelf","Eastern Scotian Shelf"))
    

pfacet <- ggplot(data=tile_df_formatted%>%mutate(prop_lost=ifelse(prop_lost==0,NA,prop_lost)),aes(x=abbreviation_ord,y=benchmark))+
          geom_tile(aes(fill=prop_lost),col="black")+
          facet_grid(species_ord~facet_lab)+
          scale_y_discrete(breaks=c(2025,2075))+ #gets crowded
          scale_fill_viridis(option = "B",na.value="white")+
          theme(strip.text.y = element_text(angle = 360),
                legend.position = "bottom",
                panel.spacing.y=unit(0.05, "lines"),
                panel.background = element_rect(fill="white"),
                axis.text.x = element_text(angle=45,vjust = 1, hjust=1,size=5,colour="black"),
                axis.text.y = element_text(size=5,colour="black"),
                strip.background.x = element_rect(fill="white",colour="black"),
                strip.background.y = element_blank())+
          labs(x="",y="",fill="% habitat lost");pfacet

ggsave("output/tiled_species_area_lost.png",pfacet,width=12,height=6,units="in",dpi=300)

#make a facet fancy

pfacet_26 <- ggplot(data=tile_df_formatted%>%mutate(prop_lost=ifelse(prop_lost==0,NA,prop_lost))%>%filter(climate_proj=="2-6"),
                    aes(x=abbreviation_ord,y=benchmark))+
              geom_tile(aes(fill=prop_lost),col="black")+
              facet_grid(species_ord~region_ord,scales="free_x",space="free_x")+
              scale_y_discrete(breaks=c(2025,2075))+ #gets crowded
              scale_fill_viridis(option = "B",na.value="white")+
              theme(legend.position = "bottom",
                    panel.spacing.y=unit(0.05, "lines"),
                    panel.spacing.x=unit(0.15, "lines"),
                    panel.background = element_rect(fill="white"),
                    axis.text.x = element_text(angle=45,vjust = 1, hjust=1,size=5,colour="black"),
                    axis.text.y = element_text(size=5,colour="black"),
                    strip.background.y = element_blank(),
                    strip.background.x = element_rect(fill="white",color="black"),
                    strip.text.x = element_text(color="black",size=6),
                    strip.text.y = element_blank())+
              labs(x="",y="",fill="% habitat lost",title = "RCP 2.6")


pfacet_85 <- ggplot(data=tile_df_formatted%>%mutate(prop_lost=ifelse(prop_lost==0,NA,prop_lost))%>%filter(climate_proj=="8-5"),
                    aes(x=abbreviation_ord,y=benchmark))+
          geom_tile(aes(fill=prop_lost),col="black")+
          facet_grid(species_ord~region_ord,scales="free_x",space="free_x")+
          scale_y_discrete(breaks=c(2025,2075))+ #gets crowded
          scale_fill_viridis(option = "B",na.value="white")+
          theme(legend.position = "bottom",
                panel.spacing.y=unit(0.05, "lines"),
                panel.spacing.x=unit(0.15, "lines"),
                panel.background = element_rect(fill="white"),
                axis.text.x = element_text(angle=45,vjust = 1, hjust=1,size=5,colour="black"),
                axis.text.y = element_blank(),
                strip.background.y = element_blank(),
                strip.background.x = element_rect(fill="white",color="black"),
                strip.text.x = element_text(color="black",size=6),
                strip.text.y = element_text(color="black",angle = 360))+
          labs(x="",y="",fill="% habitat lost",title = "RCP 8.5")

#knit them all together using facet *** note thee is something wonky going on here with guides="collect' that has something to do with the R version. 
combo_tile <- pfacet_26 + 
              pfacet_85 + 
              plot_layout(ncol=2,guides = "collect") & ## the '&' here comes from the help file. Not sure exactly what it is doing or how it works, but it does. 
              theme(legend.position = "bottom")

ggsave("output/ENSEMBLE_combination_tile_plot.png",combo_tile,width=12,height=6,units="in",dpi=300)

## mean temperature plots -------------

#extract the outputs (these are the temperatures associated with species niches)
extract_fls <- list.files("output/climate_extracts/", full.names=T)
extract_fls <- extract_fls[!grepl("shape",extract_fls)] #there aer some additional shape files that aren't needed
extract_fls <- extract_fls[!grepl("CNRM",extract_fls)] #the CNRM model was duplicated from GFDL
extract_fls <- extract_fls[!grepl("GFDL",extract_fls)] #the CNRM model was duplicated from GFDL

#if these have already been calculated load them
if(file.exists("output/ENSEMBLE_temperature_timeseries.RData")& file.exists("output/ENSEMBLE_temperature_timeseries_strat.RData")){
  
  load("output/ENSEMBLE_temperature_timeseries.RData")
  load("output/ENSEMBLE_temperature_timeseries_strat.RData")
  
}

#if mean temperature series haven't been calculated then calculate them
if(!file.exists("output/ENSEMBLE_temperature_timeseries.RData")& !file.exists("output/ENSEMBLE_temperature_timeseries_strat.RData")){
  
  #calculate the summary outputs
  timeseries_temp <- NULL
  timeseries_temp_strat <-NULL
  
  for(i in extract_fls){
    
    #progress message
    climate_proj <- strsplit(i,"_")%>%
      unlist()%>%
      .[5]%>%#use the file name to get the projection given the folders structure where the 3rd in line from 'data' is the projection (data/climate_projections/8.5/IPSL_SSP5_RCP8.5_SST_0.25deg_regrid.mat)
      gsub("\\.RData","",.)%>%
      gsub("\\.","-",.)
    
    mod <- strsplit(i,"_")%>%
          unlist()%>%
          .[4] #forth split is the file names
    
    message(paste0("Working on ",mod," ",climate_proj))
    
    #load data
    load(i) #load in the cmip_extracts from a model run (generated by climate_extract.R)
    dat <- do.call("rbind",cmip_extracts)%>% #stack it in a dataframe
           left_join(.,network_cents%>%dplyr::select(NAME,region))   
      
    #mean for all sites
    mean_temp <- dat%>%
                 group_by(species,NAME,year,month)%>%
                 summarise(meant = weighted.mean(temp,cell_area,na.rm=T))%>% #monthly averages - species, site, year (area weighted)
                 ungroup()%>%
                 group_by(year)%>%
                 summarise(sd = sd(meant,na.rm=T),
                           meant = mean(meant, na.rm=T))%>% #annual average with sd
                 ungroup()%>%
                 data.frame()%>%
                 mutate(climate_proj = climate_proj,
                        mod = mod)%>%
                 dplyr::select(climate_proj,mod,year,meant,sd)
    
    #mean stratified among regional sites
    mean_temp_strat <- dat%>%
                        group_by(region,species,NAME,year,month)%>%
                        summarise(meant = weighted.mean(temp,cell_area,na.rm=T))%>% #monthly averages
                        ungroup()%>%
                        group_by(region,year)%>%
                        summarise(sd = sd(meant,na.rm=T),
                                  meant = mean(meant, na.rm=T))%>% #annual average with sd
                        ungroup()%>%
                        data.frame()%>%
                        mutate(climate_proj = climate_proj,
                               mod = mod)%>%
                        dplyr::select(climate_proj,mod,year,region,meant,sd)
  
    
    timeseries_temp <- rbind(timeseries_temp,mean_temp)
    timeseries_temp_strat <- rbind(timeseries_temp_strat,mean_temp_strat)
    
  } #end of i loop 'extract_fls'
  
  #save intermediate outputs - takes a while to run so no need to do it twice unless the data has changed. 
  save(timeseries_temp,file="output/ENSEMBLE_temperature_timeseries.RData")
  save(timeseries_temp_strat,file="output/ENSEMBLE_temperature_timeseries_strat.RData")
   
} #end if for the intermediate file checks ('do we need to run this code or not?' check)


#plot data cleaning
timeseries_temp <- timeseries_temp %>% mutate(mod = factor(mod,levels=c("AWI","IPSL","HAD","Ensemble")))

timeseries_temp_strat <- timeseries_temp_strat %>% mutate(mod = factor(mod,levels=c("AWI","IPSL","HAD","Ensemble")),
                                                          region = factor(region,levels=c("Bay of Fundy","Western Scotian Shelf","Eastern Scotian Shelf")))

timeseries_temp_strat_invert <- timeseries_temp_strat%>%mutate(climate_proj = ifelse(climate_proj == "2-6","8-5","2-6"))# this inverts for the plot

plot_cols <- c("#000000", "#E69F00", "#D55E00", "#CC79A7")

#timeseries (network average) plot

temp_series_plot <- ggplot()+
                      geom_line(data=timeseries_temp%>%filter(mod != "Ensemble"),aes(x=year,y=meant,group=mod,col=mod))+
                      stat_smooth(data=timeseries_temp%>%filter(mod != "Ensemble"),aes(x=year,y=meant,group=mod,col=mod),se=F)+
                      stat_smooth(data=timeseries_temp,aes(x=year,y=meant),col="black",lwd=2)+
                      facet_wrap(~climate_proj,ncol=2)+
                      labs(x="",y=expression("Temperature " ( degree*C)),col="")+
                      theme_bw()+
                      theme(strip.background = element_rect(fill="white"))+
                      scale_color_manual(values=plot_cols);temp_series_plot


temp_series_strat_plot <- ggplot()+
                            geom_line(data=timeseries_temp_strat%>%filter(mod != "Ensemble"),aes(x=year,y=meant,group=mod,col=mod))+
                            stat_smooth(data=timeseries_temp_strat%>%filter(mod != "Ensemble"),aes(x=year,y=meant,group=mod,col=mod),se=F)+
                            stat_smooth(data=timeseries_temp_strat,aes(x=year,y=meant),col="black",lwd=2)+
                            facet_grid(region~climate_proj)+
                            labs(x="",y=expression("Temperature " ( degree*C)),col="")+
                            theme_bw()+
                            theme(strip.background = element_rect(fill="white"))+
                            scale_color_manual(values=plot_cols);temp_series_strat_plot 

temp_series_strat_plot_extra <- ggplot()+
                                  geom_line(data=timeseries_temp_strat_invert%>%filter(mod != "Ensemble"),aes(x=year,y=meant,group=mod),col="grey70",lwd=0.5)+
                                  stat_smooth(data=timeseries_temp_strat_invert%>%filter(mod != "Ensemble"),aes(x=year,y=meant,group=mod),col="grey50",lwd=0.6,se=F)+
                                  geom_line(data=timeseries_temp_strat%>%filter(mod != "Ensemble"),aes(x=year,y=meant,group=mod,col=mod))+
                                  stat_smooth(data=timeseries_temp_strat%>%filter(mod != "Ensemble"),aes(x=year,y=meant,group=mod,col=mod),se=F)+
                                  stat_smooth(data=timeseries_temp_strat,aes(x=year,y=meant),col="black",lwd=2)+
                                  facet_grid(climate_proj~region)+
                                  labs(x="",y=expression("Temperature " ( degree*C)),col="")+
                                  theme_bw()+
                                  theme(strip.background = element_rect(fill="white"),
                                        legend.position = c(0.05,0.91), #this code will make it render differently in RStudio vs. the image by ggsave below. We might need to update to Facets dimenions. 
                                        legend.title = element_blank(),
                                        legend.margin=margin(t=0, r=0, b=0, l=0, unit="cm"),
                                        legend.text = element_text(size=7))+
                                  scale_color_manual(values=plot_cols);temp_series_strat_plot_extra 


##save plots
ggsave("output/ENSEMBLE_temperature_mean_timeseries.png",temp_series_plot,width=9,height=6,units="in",dpi=300)
ggsave("output/ENSEMBLE_temperature_mean_timeseries_strat.png",temp_series_strat_plot,width=9,height=6,units="in",dpi=300)
ggsave("output/ENSEMBLE_temperature_mean_timeseries_strat_extra.png",temp_series_strat_plot_extra,width=9,height=6,units="in",dpi=300)
