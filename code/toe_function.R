#Function that can be used to calculated the toe

calculate_toe <- function(x,lag=3,func=function(x){mean(x,na.rm=T)},niche=NULL){
  
  require(dplyr)
  require(tidyr)
  require(RcppRoll)
  
  if(is.null(niche)){niche<-read.csv("data/species_niche_final.csv")} ## note that this is hard coded. So the function will only work if the working directory is set to the root of mar_thermal_emerg
  
  
  ##logic is that we create a logical for each species, site, cell and year
  
  #this is a data.frame that can be used to 'pad' the data.frame with two fake years that will distinguish between species and sites
 pad_df <- x%>%
    filter(year%in%c(2015,2016))%>%
    mutate(year=ifelse(year==2015,2013,2014),
           meant=-999,
           UprTemp90=NA,
           threshold=FALSE)%>%
    dplyr::select("species","NAME","year","id","meant","UprTemp90","threshold")
  
  message("Extracting maximum temperatures per species, site, year")
  
  mean_temp <- x%>%
    group_by(species,NAME,year,id)%>%
    summarise(meant=func(temp))%>%#mean temperature 
    ungroup()%>%
    left_join(.,niche%>%dplyr::select(SciName,UprTemp90)%>%rename(species=SciName))%>%
    mutate(threshold = meant>UprTemp90)%>% 
    rbind(.,pad_df)%>%
    arrange(species,NAME,id,year)%>%
    mutate(emerge=roll_sum(threshold,lag,fill=NA,align="left"),
           emerged=emerge==lag)%>% # a score == lag means that it is the first time consecutively there has been a ToE
    data.frame()%>%
    suppressMessages() #just gets rid of the friendly group_by messages   
  
  message("Calculating ToE for each species")
  
  #calculate the TOE based on the roll_sum (where the lag is observed)
  toe_calc <- mean_temp%>%
    group_by(species,NAME,id)%>%
    summarise(ToE = ifelse(sum(emerged)>0,year[year==min(year[emerged])],NA))%>%
    ungroup()%>%
    data.frame()
  
  #now create a dataframe that shows the max temp per species, site, and cell, the upper limit for the species and the estimated ToE        
  output <- toe_calc%>%
    left_join(.,mean_temp%>%
                mutate(meant=ifelse(meant==(-999),NA,meant))%>%# i used the -999 to force an 'na' for the 2013 and 2013 but this needs to be removed for this calc
                group_by(species,NAME,id)%>%
                summarise(meant=func(meant))%>%
                ungroup()%>%
                left_join(.,mean_temp%>%
                            filter(year>2015)%>% #this is because '2013' and '2014' are just dummy years
                            dplyr::select(species,UprTemp90)%>%
                            distinct(species,.keep_all=TRUE))%>%suppressMessages())%>%
    mutate(mod=unique(x$mod), #add in the model data
           climate_proj=unique(x$climate_proj))%>%
    left_join(.,x%>%
                filter(month==1,
                       year==2015)%>%
                dplyr::select(NAME,id,species,site_area,cell_area))%>%
    dplyr::select(mod,climate_proj,species,NAME,cell_area,id,site_area,meant,UprTemp90,ToE)
  
  return(output)
  
  
} #end of the function