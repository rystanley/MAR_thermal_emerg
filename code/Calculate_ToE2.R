## Calculate Time of Emergence (TOE) for all models and projections

#load libraries -----
library(dplyr)
library(tidyr)

#load the toe function
source("code/toe_function.R")

#List of climate model outputs
extract_fls <- list.files("output/climate_extracts/", full.names=T)
extract_fls <- extract_fls[!grepl("shape",extract_fls)] #there aer some additional shape files that aren't needed
extract_fls <- extract_fls[!grepl("CNRM",extract_fls)] #the CNRM model was duplicated from GFDL

#read in species niches
niche<-read.csv("data/species_niche_final.csv")

#Create directory (if needed) for the toe_summaries
if(!dir.exists("output/toe_summaries/")){dir.create("output/toe_summaries/")}

#calculate the summary outputs
toe_summaries <- NULL
timeseries<-NULL
    
  for(i in extract_fls){
      
      #Get the data together
        load(i) #load in the cmip_extracts from a model run (generated by climate_extract.R)
        dat <- do.call("rbind",cmip_extracts) #stack it in a dataframe
      
      #progress message
        mod <- unique(dat$mod)
        climate_proj <- unique(dat$climate_proj)
        
        message(paste0("Working on ",mod," ",climate_proj))
      
      #Modify to include a grid cell ID
        dat2 <- dat%>%
              group_by(species,NAME,year,month)%>%
              mutate(id=1:n())%>% #this is a unique id for each 'cell' categorized within each site (NAME), species and year
              ungroup()%>%
              data.frame()%>%
              left_join(.,dat)%>% #brings in the columns lost by group_by()
              suppressMessages() #just the joins and are not required 
      
      #Calculate Time of Emergence using he sourced function (based on a mean and 3 year lag)
        toe_output <- calculate_toe(x=dat2,lag=3,niche=niche,func=function(x){mean(x,na.rm=T)})%>%suppressMessages() #using the mean annual summary - messages are returned (e.g., what was joined )
      
        #save the intermediate steps
        save(toe_output,file=paste0("output/toe_summaries/",mod,"_",climate_proj,"_toe_summaries.RData"))
      
      #combine the outputs for one data.frame
      toe_summaries <- rbind(toe_summaries,toe_output)
      timeseries<-rbind(timeseries,dat2)
      
      rm(dat,mod,climate_proj,toe_output) #clean up the loop so that nothing is accidentally repeated (though this technically can't happen)
      
  } #end i loop, takes around 

#save for fast loading
save(toe_summaries,file=paste0("output/toe_summaries/all_toe_summaries.RData"))
save(timeseries,file=paste0("output/timeseries.RData"))

load("output/timeseries.RData")
require(dplyr)
meant.network<-timeseries%>%
  filter(species=="Amblyraja radiata")%>%
  group_by(mod,climate_proj,year)%>%
  summarise(mean.temp=weighted.mean(temp,cell_area), temp.sd=sd(temp))%>%
  ungroup()%>%
  data.frame()
write.csv(meant.network,"meant_network.csv")


# ## some diagnostic tests
# dat3 <- toe_summaries%>%filter(species %in% c("Brosme brosme","Hippoglossus hippoglossus"),mod=="AWI",climate_proj=="2-6")
# 
# 
# test_run <- NULL
# for(i in 2:5){
#   for(j in c("mean","max")){
#     
#     message(paste0("working on lag ",i," years ",j))
#     
#     if(j == "mean"){
#       
#       out <- Calculate_toe(dat3,niche=niche,lag=i)%>%mutate(test=j,lag=paste(i,"years",sep=" "))
#       
#     }
#     
#     if(j == "max"){
#       
#       out <- Calculate_toe(dat3,niche=niche,lag=i,func=function(x){max(x,na.rm=T)})%>%mutate(test=j,lag=paste(i,"years",sep=" "))
#       
#     }
#     
#     test_run <- rbind(test_run,out)
#     
#   }
# }
# 
# 
# ggplot(test_run%>%filter(test=="mean"),aes(x=species,y=ToE,fill=lag))+
#   geom_jitter(size=0.5)+
#   geom_boxplot(alpha=0.5)+
#   theme_bw()+
#   labs(y="Time of Emergence",x="Species")
