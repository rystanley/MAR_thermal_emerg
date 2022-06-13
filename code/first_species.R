first_spp <- habitat_loss_site%>%
  filter(ToE != 2500, #these are species that did not emerge
         mod!="GFDL", #no GFDL for 2-6
         !species%in%c("Sebastes mentella","Calanus glacialis"),
         cum_sum==1)%>%
  group_by(climate_proj,mod,NAME,species,ToE)%>%
  summarise(count=n())%>%
  ungroup()%>%
  data.frame()

first_spp<-first_spp[first_spp$mod=="Ensemble",]
write.csv(first_spp,"first_spp.csv")
