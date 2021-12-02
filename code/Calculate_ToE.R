require(devtools)
require(raster)# package for raster manipulation
library(ncdf4) # package for netcdf manipulation
library(sp)
library(rgdal)
library(ggplot2)
require(R.matlab) #to open Dan's MatLab files in R

### Time of thermal emergence + 2100 ###############
####################################################

### STEP 1: Create a time series of maximum monthly average per year for each model/emissions scenario ###
#create a file path to your climate projection files. Keep in the folders (2.6,8.5) Dan had them in
pn<-('C:/Users/StortiniC/Desktop/Shaylyn/Climate Projections/Diff var/')  #I put all 2.6 and 8.5 files into a new folder called "All"
fls<-list.files(pn, full.names=T) ##climate projections for RCP 2.6; run again for the 4.5 folder
l<-list()

for(i in 1:length(fls)){
  #pathname <- file.path(pn, paste(fls[i]))
  data <- readMat(fls[i])
  bdata<-brick(data$outt,xmn=-83,xmx=-41,ymn=38,ymx=85,crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  #Note, if you run bdata, you'll find it has 1032 layers, which represents 1032 months
  #1032 months/12 months per year gives you 86 years.
  #We know the end year is 2100; 2100-86 is 2014, add 1 because we are counting the year 2100, 
  #so 2015 is our starting year.
  #note that x and y are longitude and latitude (these are cells of data in geographic space, with values corresponding to temperature, and stacked layers of cells corresponding to months)
  df<-as.data.frame(bdata,xy=TRUE,long=TRUE,centroids=TRUE) #turn the 3-dimensional stack of rasters into a 2-d dataframe with month (i.e., the name of the layers in the raster brick) as a column
  df$cont.month<-substr(df$layer,7,8) #take the last two characters of the layer names to get the month number (from 1 to 1032)
  df$month<-rep(rep(1:12,each=31584),86) #add a column for the actual month in a year (1 to 12, for 86 years)
  df$year<-rep(2015:2100, each=379008) #column for year
  df$model<-substr(gsub('.mat','',paste(fls[i])),56,59)#create a column for model name
  df$emiss.scen<-substr(gsub('.mat','',paste(fls[i])),60,71)#create a column for emissions scenario
  l[[i]]<-df
}
require(data.table)
monthly.temps1<-data.frame(rbindlist(l))
#Error: cannot allocate vector of 994.7Mb....

##files with different variable names, so have to run them separately.
pn<-('C:/Users/StortiniC/Desktop/Shaylyn/Climate Projections/All_datout/')  #I put all 2.6 and 8.5 files into a new folder called "All"
fls<-list.files(pn, full.names=T) ##climate projections for RCP 2.6; run again for the 4.5 folder
l<-list()

for(i in 1:length(fls)){
  #pathname <- file.path(pn, paste(fls[i]))
  data <- readMat(fls[i])
  bdata<-brick(data$datout,xmn=-83,xmx=-41,ymn=38,ymx=85,crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  #Note, if you run bdata, you'll find it has 1032 layers, which represents 1032 months
  #1032 months/12 months per year gives you 86 years.
  #We know the end year is 2100; 2100-86 is 2014, add 1 because we are counting the year 2100, 
  #so 2015 is our starting year.
  #note that x and y are longitude and latitude (these are cells of data in geographic space, with values corresponding to temperature, and stacked layers of cells corresponding to months)
  df<-as.data.frame(bdata,xy=TRUE,long=TRUE,centroids=TRUE) #turn the 3-dimensional stack of rasters into a 2-d dataframe with month (i.e., the name of the layers in the raster brick) as a column
  df$cont.month<-substr(df$layer,7,8) #take the last two characters of the layer names to get the month number (from 1 to 1032)
  df$month<-rep(rep(1:12,each=31584),86) #add a column for the actual month in a year (1 to 12, for 86 years)
  df$year<-rep(2015:2100, each=379008) #column for year
  df$model<-substr(gsub('.mat','',paste(fls[i])),56,59)#create a column for model name
  df$emiss.scen<-substr(gsub('.mat','',paste(fls[i])),60,71)#create a column for emissions scenario
  l[[i]]<-df
}
require(data.table)
monthly.temps2<-data.frame(rbindlist(l))
#Error: cannot allocate vector of 994.7Mb....


###### FROM HERE ON, NOT DEVELOPED ############
#create another file path to your species-specific cropped network shapefiles
pn2<-('C:/Users/StortiniC/Desktop/Shaylyn/Code/MAR_thermal_emerg/output/species_networks')
fls2<-list.files(pn2, full.names=T) ##shapefiles containing cropped MPA polygons for each species

for(i in fls2){
df$sp<-substr(fls2[i],1,15) #create a column for species name (a substring of characters from the file name, which contains the species name)
l2[[i]]<-df2
}

## Find max mean per year
pn<-paste('C:/Users/StortiniC/Desktop/Freshwater_CCVA/ECCC_data/monthlyT/',sep='')
fls<-list.files(pn)
l<-list()
mmfun<-function(d){
  max<-data.frame(maxT=max(d$temp-3,na.rm=TRUE),
                  rcp=substr(fls[i],4,5))
  return(max)
}
require(plyr)
for(i in 1:length(fls)){
  pathname <- paste(pn, fls[i],sep='')
  data <- read.csv(pathname)
  dvel<-ddply(data,.(ws,model,year),.fun=mmfun,.progress='text')
  l[[i]]<-dvel}
require(data.table)
max<-data.frame(rbindlist(l))
head(max)
summary(max)
length(unique(max$model))

## Average across models for each rcp
fn1<-function(d){data.frame(meanmax=mean(d$maxT,na.rm=TRUE),
                            meanmaxsd=sd(d$maxT,na.rm=TRUE),
                            meanmaxse=sd(d$maxT,na.rm=TRUE)/sqrt(length(unique(d$model))),
                            nmods=length(unique(d$model)))}
meanmaxt<-ddply(max,.(ws,rcp,year),.fun=fn1)
meanmaxt$meanmax_upr<-meanmaxt$meanmax+(1.96*meanmaxt$meanmaxse)
meanmaxt$meanmax_lwr<-meanmaxt$meanmax-(1.96*meanmaxt$meanmaxse)
head(meanmaxt)
write.csv(meanmaxt,"maxmeanT_nsws_minus3.csv")

setwd("C:/Users/StortiniC/Desktop/Freshwater_CCVA/ECCC_data")
maxmeant<-read.csv("maxmeanT_nsws_minus3.csv")
#maxmeant<-meanmaxt
#TMAX Salmon=23; Eel=32; Bass=26; Whitefish=24
maxmeant$year<-as.numeric(maxmeant$year)
maxmeant<-maxmeant[maxmeant$year>2021,]
summary(maxmeant)
fn1<-function(d){YearEmerg_salmon<-d[d$meanmax>20,]
YearEmerg_salmon_lwr<-d[d$meanmax_upr>20,]
YearEmerg_salmon_upr<-d[d$meanmax_lwr>20,]
YearEmerg_eel<-d[d$meanmax>32,]
YearEmerg_eel_lwr<-d[d$meanmax_upr>32,]
YearEmerg_eel_upr<-d[d$meanmax_lwr>32,]
YearEmerg_bass<-d[d$meanmax>26,]
YearEmerg_bass_lwr<-d[d$meanmax_upr>26,]
YearEmerg_bass_upr<-d[d$meanmax_lwr>26,]
YearEmerg_whitefish<-d[d$meanmax>24,]
YearEmerg_whitefish_upr<-d[d$meanmax_lwr>24,]
YearEmerg_whitefish_lwr<-d[d$meanmax_upr>24,]
data.frame(YearEmerg_salmon=min(YearEmerg_salmon$year),
           YearEmerg_salmon_upr=min(YearEmerg_salmon_upr$year),
           YearEmerg_salmon_lwr=min(YearEmerg_salmon_lwr$year),
           YearEmerg_bass=min(YearEmerg_bass$year),
           YearEmerg_bass_upr=min(YearEmerg_bass_upr$year),
           YearEmerg_bass_lwr=min(YearEmerg_bass_lwr$year),
           YearEmerg_eel=min(YearEmerg_eel$year),
           YearEmerg_eel_upr=min(YearEmerg_eel_upr$year),
           YearEmerg_eel_lwr=min(YearEmerg_eel_lwr$year),
           YearEmerg_whitefish=min(YearEmerg_whitefish$year),
           YearEmerg_whitefish_upr=min(YearEmerg_whitefish_upr$year),
           YearEmerg_whitefish_lwr=min(YearEmerg_whitefish_lwr$year))}
tsms<-ddply(maxmeant,.(ws,rcp),.fun=fn1)
summary(tsms)
tsms$by2100_salmon<-ifelse(tsms$YearEmerg_salmon<2101,1,0)
tsms$by2100_eel<-ifelse(tsms$YearEmerg_eel<2101,1,0)
tsms$by2100_bass<-ifelse(tsms$YearEmerg_bass<2101,1,0)
tsms$by2100_whitefish<-ifelse(tsms$YearEmerg_whitefish<2101,1,0)
write.csv(tsms,"YearEmerge_nsws_minus3_20.csv")