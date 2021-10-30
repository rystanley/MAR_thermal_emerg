## Code to create ggplot of the depth and thermal range comparisons

#load libraries ----
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

#load data ----
dat <- read.csv("data/species_niche_formatted.csv")

#make depth plot -----

    #Assign the order so that it plots with the species with the deeptest depth first down the the shallowest possible depth among the ranges
    depth_order <- dat%>%
                    filter(var=="depth")%>%
                    group_by(scientific_name)%>%
                    summarise(max=max(upper,na.rm=T))%>% # highest estimated depth among datasets
                    ungroup()%>%
                    arrange(max)%>% # too many here
                    select(scientific_name)
    
    #Subset the data for just depth and order the scientific names (which will be the y axis after cord_flip) to be the order in depth_order
    depth_data <- dat%>%
                  filter(var=="depth")%>%
                  mutate(scientific_name=factor(scientific_name,levels=depth_order$scientific_name))
    
    #construct the plot
    depth_plot <- ggplot(data=depth_data,aes(group=interaction(scientific_name,type),col=type,
                                               y=upper,ymin=lower,ymax=upper,x=scientific_name))+
      geom_linerange(position = position_dodge(0.7))+ #line spanning the range
      geom_point(aes(y=lower),position = position_dodge(0.7))+ #point at the lower point
      geom_point(position = position_dodge(0.7))+ #point at the upper point
      theme_bw()+
      labs(y="Estimated depth range (m)",x="",col="")+
      coord_flip()+ #this will invert the x and y axes. I do this because syntactically it is easier to start with the data ranges on the y axis
      theme(legend.position = c(0.8,0.1),
            panel.grid = element_blank())
    
    #View plot
    depth_plot
  
#Make the temperature plot ------

    #Assign the order so that it plots with the species with the highest temp first down the the lowest possible temp among the ranges
    temp_order <- dat%>%
                  filter(var=="temp")%>%
                  group_by(scientific_name)%>%
                  summarise(max=max(upper,na.rm=T))%>% # highest estimated depth among datasets
                  ungroup()%>%
                  arrange(max)%>% # too many here
                  select(scientific_name)
    
    #Subset the data for just temperature and order the scientific names (which will be the y axis after cord_flip) to be the order in temp_order
    temp_data <- dat%>%
                filter(var=="temp")%>%
                mutate(scientific_name=factor(scientific_name,levels=temp_order$scientific_name))
    
    #construct the plot
    temp_plot <- ggplot(data=temp_data,aes(group=interaction(scientific_name,type),col=type,
                                             y=upper,ymin=lower,ymax=upper,x=scientific_name))+
                  geom_linerange(position = position_dodge(0.7))+
                  geom_point(aes(y=lower),position = position_dodge(0.7))+
                  geom_point(position = position_dodge(0.7))+
                  theme_bw()+
                  labs(y=expression("Estimated temperature range " ( degree*C)),x="",col="")+
                  coord_flip()+
                  theme(legend.position = c(0.8,0.1),
                        panel.grid = element_blank())
    
    #View the plot
    temp_plot()

#Now lets combine the plots together using patchwork. This makes the whole process more simple because the plot order and axis scaling is independant among plots. This also alows you to build on the existing plots
    alt_depth_plot <- depth_plot+
                      scale_y_continuous(position="left")+ #because the original plot is flipped we have to make the plot have a differnet y axis which is flipped in its basis formulation
                      theme(legend.position = c(0.9,0.15))
    
    alt_temp_plot <- temp_plot+theme(legend.position = c(0.9,0.15))
    
    combo_plot <- alt_depth_plot + alt_temp_plot + plot_layout(nrow=2)

## save the plots -----
    ggsave("output/depth_comparison.png",depth_plot,width=6,height=6,units="in",dpi=300)
    ggsave("output/temp_comparison.png",temp_plot,width=6,height=6,units="in",dpi=300)
    ggsave("output/combination_comparison.png",combo_plot,width=8,height=9,units="in",dpi=300)
    