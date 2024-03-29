### Ryan's code to create ggplot of the depth and thermal range comparisons

#load libraries ----
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

#load data ----
groupings <- read.csv("data/species_grouping.csv")%>%select(SciName,functional_group,cosewic_status)%>%rename(scientific_name=SciName)

niches <- read.csv("data/species_niche_formatted.csv")%>%
  dplyr::select(- functional_group)%>%
  left_join(.,groupings)%>%
  mutate(functional_group = factor(functional_group,levels=rev(c("Mammals/Reptiles","Copepod","Benthic Invertebrates",
                                                                 "Pelagic Fish/Cepholopods","Benthic fish"))))

  #filter(!SciName %in% c("Sebastes mentella","Calanus glacialis"))

#make depth plot -----

    #Assign the order so that it plots with the species with the deeptest depth first down the the shallowest possible depth among the ranges
    depth_order <- niches%>%
                    filter(var=="depth")%>%
                    group_by(scientific_name,functional_group)%>%
                    summarise(max=max(upper,na.rm=T))%>% # highest estimated depth among datasets
                    ungroup()%>%
                    arrange(functional_group,max)%>%
                    pull(scientific_name)
    
    #Subset the data for just depth and order the scientific names (which will be the y axis after cord_flip) to be the order in depth_order
    depth_data <- niches%>%
                  filter(var=="depth")%>%
                  mutate(scientific_name=factor(scientific_name,levels=depth_order))
    
    #construct the plot
    depth_plot <- ggplot(data=depth_data,aes(group=interaction(scientific_name,type),col=type,
                                               y=upper,ymin=lower,ymax=upper,x=scientific_name))+
      geom_linerange(position = position_dodge(0.7))+ #line spanning the range
      geom_point(aes(y=lower),position = position_dodge(0.7),size=2)+ #point at the lower point
      geom_point(position = position_dodge(0.7),size=2)+ #point at the upper point
      facet_grid(functional_group~.,scales="free",space="free")+
      theme_bw()+
      labs(y="Estimated depth range (m)",x="",col="",title="Depth Niche")+
      coord_flip()+ #this will invert the x and y axes. I do this because syntactically it is easier to start with the data ranges on the y axis
      theme(legend.position = c(0.90,0.88),
            legend.background = element_rect(colour = "black"),
            legend.title = element_blank(),
            strip.background.y = element_blank(),
            strip.text.y = element_blank(),
            axis.text.x = element_text(size=12,colour="black"),
            axis.text.y = element_text(colour="black"),
            panel.spacing.x = unit(4, "mm"));depth_plot
 
#Make the temperature plot ------

    #Assign the order so that it plots with the species with the highest temp first down the the lowest possible temp among the ranges
    temp_order <- niches%>%
                    filter(var=="temp")%>%
                    group_by(scientific_name,functional_group)%>%
                    summarise(max=max(upper,na.rm=T))%>% # highest estimated depth among datasets
                    ungroup()%>%
                    arrange(functional_group,max)%>%
                    pull(scientific_name)
    
    #Subset the data for just temperature and order the scientific names (which will be the y axis after cord_flip) to be the order in temp_order
    temp_data <-niches%>%
                filter(var=="temp")%>%
                mutate(scientific_name=factor(scientific_name,levels=temp_order))
    
    #construct the plot
    temp_plot <- ggplot(data=temp_data,aes(group=interaction(scientific_name,type),col=type,
                                             y=upper,ymin=lower,ymax=upper,x=scientific_name))+
                  geom_linerange(position = position_dodge(0.7))+
                  geom_point(aes(y=lower),position = position_dodge(0.7),size=2)+
                  geom_point(position = position_dodge(0.7),size=2)+
                  facet_grid(functional_group~.,scales="free",space="free")+
                  theme_bw()+
                  labs(y=expression("Estimated temperature range " ( degree*C)),x="",col="",title="Temperature Niche")+
                  coord_flip()+
                  theme(legend.position = "none",
                        strip.background.y = element_blank(),
                        strip.text.y = element_blank(),
                        axis.text.x = element_text(size=12,colour="black"),
                        axis.text.y = element_text(colour="black"),
                        panel.spacing.x = unit(4, "mm"));temp_plot
    
 
#Now lets combine the plots together using patchwork. This makes the whole process more simple because the plot order and axis scaling is independant among plots. This also alows you to build on the existing plots
    alt_depth_plot <- depth_plot+
                      scale_y_continuous(position="left") #because the original plot is flipped we have to make the plot have a differnet y axis which is flipped in its basis formulation
                      
    alt_temp_plot <- temp_plot
    
    combo_plot <- alt_depth_plot + alt_temp_plot + plot_layout(nrow=2)

## save the plots -----
    ggsave("output/depth_comparison.png",depth_plot,width=6,height=6,units="in",dpi=300)
    ggsave("output/temp_comparison.png",temp_plot,width=6,height=6,units="in",dpi=300)
    ggsave("output/combination_comparison.png",combo_plot,width=8,height=9,units="in",dpi=600)

    #End.
    
    
#### Christine's ggplot code ####
    #note: I like Ryan's better, but showing you anyways 
    #so you get an example of how there are always multiple ways 
      #to do the same thing
    #Also, there are a few peices of code in here that will be good for 
    #you to learn (e.g., black and white colour scale)
df<-read.csv("data/Species list-Shaylyn_reformatted.csv")
head(df) #see reformatting and new column names

require(ggplot2) # can use "library()" or "require()"... both work
require(dplyr)
require(tidyr)
require(patchwork)

#use Ryan's code for ordering -- definitely most effective
depth_order <- df%>%
  filter(var=="depth")%>%
  group_by(sci.name)%>%
  summarise(max=max(upr,na.rm=T))%>% # highest estimated depth among datasets
  ungroup()%>%
  arrange(max)%>% # too many here
  select(sci.name)

#limit data to where depth data exist (only aquamaps and obis), and reorder sci.name by depth
#note square brackets to subset data
df.d <- df[df$source%in%c("aquamaps","obis"),]%>%
  filter(var=="depth")%>%
  mutate(sci.name=factor(sci.name,levels=depth_order$sci.name))

p.depth<-ggplot(data=df.d,aes(group=source,col=source,x=upr,
                xmin=lwr,xmax=upr.depth,y=sci.name))+
  geom_errorbar(aes(xmin=lwr, xmax=upr), width=0.65,
                position=position_dodge(width=0.6))+
  theme_bw()+
  scale_colour_grey(start=0.2, end=0.63)+
  labs(x="Estimated depth range (m)",y="",col="")+
  theme(legend.position = c(0.85,0.2),
        panel.grid = element_blank())

#Ryan's code for ordering again
temp_order <- df%>%
  filter(var=="temp")%>%
  group_by(sci.name)%>%
  summarise(max=max(upr,na.rm=T))%>% # highest estimated depth among datasets
  ungroup()%>%
  arrange(max)%>% # too many here
  select(sci.name)

#limit data to our temps (only aquamaps and shackell), and reorder sci.name by temp
df.t <- df[df$source%in%c("aquamaps","shackell"),]%>%
  filter(var=="temp")%>%
  mutate(sci.name=factor(sci.name,levels=temp_order$sci.name))

p.temp<-ggplot(data=df.t,aes(group=source,col=source,x=upr,
                              xmin=lwr,xmax=upr.depth,y=sci.name))+
  geom_errorbar(aes(xmin=lwr, xmax=upr), width=0.65,
                position=position_dodge(width=0.6))+
  theme_bw()+
  scale_colour_grey(start=0.2, end=0.63)+
  labs(x="Estimated temperature tolerance range ",y="",col="")+
  theme(legend.position = c(0.85,0.2),
        panel.grid = element_blank())

#I have only used grid.arrange in the past, shown here, but Ryan's way 
  #to combine plots using patchwork is better n the long-run
  #(you'll run into fewer issues with lining up y axes when plots get more complicated)
  #but mine works great here and is quite simple
require(gridExtra)
grid.arrange(p.depth,p.temp,ncol=1)

#We could also arrange the y-axes both by depth, so they match
#Then we could merge the two plots side-by-side (i.e., ncol=2 in grid.arrange),
  #so that they are 2 panels of the same figure
#I often do this by clipping the figures together in GIMP

#..and another way to print your plots to a tiff 
  #tiff often a good choice for publications (they usually perfer tiff over jpeg)
  #also note 300dpi is often the preferred resolution for publications
  #168mm is also the maximum width of a figure for publication (full page)
tiff("output/Depth-TempRanges.tiff",width=168,height=168,units="mm",compression="lzw",bg="white", res=300)
grid.arrange(p.depth,p.temp,ncol=1) # note that the original had a '&' in the file name and github won't track the change. I am actually not sure why
dev.off() #apparently tiff won't show up in figures at main page of repository because github doesnt like tiffs, alas, Ill save as png just for GitHub

ggsave("output/storts_range_plot.png",grid.arrange(p.depth,p.temp,ncol=1),width=8,height=9,units="in",dpi=300)

##Final notes: 
##It's often a good idea to stick with black and grey 
  #colour-schemes to avoid the issue of red/green being hard to decipher 
  #for colour-blind people, and because it can be cheaper to publish
  #black and white manuscripts.
##With my code, I usually save the figure and check the legend position
  #sometimes I need to go back and adjust the position since it changes
  #slightly after saving to tiff.
