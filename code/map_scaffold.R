### maps of the species lost put within the network
library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)
library(rnaturalearth)
library(patchwork)
library(viridis)

sf_use_s2(FALSE) # this is something for the sf package. Important that it is run at least once

#Projections ------------
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"


#load geographic files-----------

  #load the 'MaritimesPlanningArea' shape file and transform to the 'latlong' projection defined above. 


#create basemap from rnaturalearth and make it bounded to our study region ------------
  

#load the network  ---------
  
  # create a object called 'network_base' by loading the 'networksites_proposed_OEM_MPA_v2' and again convert to latlong.
  # in all of our code we do two standard things:
  #         1)  filter out the Bras d'Or
  #         2)  st_union because in the shape file "Western Emerald Bank" is in two pieces (rows)
  

#load the species lost dataframe (geneated in summary_plots.R) -------------
  load("data/species_lost_df.RData")

#Now we add the data to the network ------------- 

    #so here you will need to merge (I like left_join) the species_lost data with the network (here I called it network_base). This uses the 'gather' fuction and I will give you an example of how to do this below
  
    #RCP 2.6 plot
        network_26 <- network_base%>%
                      filter(NAME %in% unique(species_lost_df$NAME))%>% # there was no species with suitable depth within the range of these sits "Big Glace Bay","Cold Seeps","John Lusby Marsh National Wildlife Area"
                      left_join(.,species_lost_df%>%filter(climate_proj == "2-6"))%>% #you will need to make one plot per climate projection
                      gather(benchmark,lost,7:10) # 7:10 because those are the last 4 columns that are 'gathered' into a new column. 
          
        #now you can make the plot. Think about adding layers sequentially (see how we did it in various pecies of code including 'p1' in summary_plots)
        #trick here will be to add your network (network_26) after the basemap and the bioregion. 
          #to make it faceted you will want to facet for each year (variable 'benchmark' above) with 2 columns (4 years, 2 columns, 4 panels)
          #I would also suggest playing with the default colour fill (we used viridis in a few other plots with the difference here being that the fill is not discrete). When you add the layer with the network_26
              # you will need to add the 'fill' (e.g., aes(fill=)) for the variable corresponding to the amount of species lost
          #you will want to make sure the legend is on the bottom (think of how we did this on the call the other day)
          #last for the labels you will want to add a top level title. In my example I used RCP 2.6
                
    #RCP 8.5 plot
       #repeat steps above


##use patchwork to combine the plots 

  #I will help you with this but the basic ingredients for how to do it are in the tile plot code in summary_plots if you want to try. 


message("YOU CAN DO THIS SHAYLYN!!!!!")
