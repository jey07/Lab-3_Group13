library(ggplot2)
library(plotly)
library(ggmap)
library(akima)

#Reading file
swedfile<-read.csv("Swedish_Household.csv")
swedimage<-readRDS("Sweden_counties.rds")

#Renaming the levels of Age to more readable form and modifying the columns in table
levels(swedfile$age)<-c("Young","Adult","Senior")
colnames(swedfile)<-c("Region","Age","Mean_Income")

#Renaming the county to simpler form.
columnsplit<-strsplit(levels(swedfile$Region)," ")
levels(swedfile$Region)<-sapply(columnsplit,function(x) x[[2]])

#Spreading the data from long to wide based on factors of Age
new_swedfile<-tidyr::spread(swedfile,Age,Mean_Income)
#colnames(new_swedfile)<-c("Region","Yo","Mean_Income")
new_swedfile$Region=as.character(new_swedfile$Region)

#Changing the names of region in the file from csv to match region names in sf file
new_swedfile[new_swedfile$Region=="Örebro",]$Region<-"Orebro"
new_swedfile[new_swedfile$Region=="Västra",]$Region<-"Västra Götaland"


#Violin Plot for three Age group showing mean income in SEK 
p<-ggplot(swedfile,aes(Age,Mean_Income,color=Age)) + geom_violin()
   
  p+stat_summary(fun.y=median,geom="point",color="blue") +xlab("\n Age Group ") +
  ylab("Mean Income in SEK\n")

#Doing a cubic interpolation to get continous variable and creating a surace plot
  interpolated=interp(new_swedfile$Young,new_swedfile$Adult,new_swedfile$Senior,duplicate = "mean")
  plot_ly(x=~interpolated$x, y=~interpolated$y, z=~interpolated$z, type="surface")
  
#Creating a surface plot
  plot_ly(new_swedfile[,2:4]) %>% add_surface()
  
#Modifying the rownames  
rownames(new_swedfile)<-new_swedfile$Region

#Adding Mean values of Young and Adult age group corresponding to the name from
#rds file
swedimage$Young<-new_swedfile[swedimage$NAME_1,"Young"]
swedimage$Adult<-new_swedfile[swedimage$NAME_1,"Adult"]

#Plotting chlorpeth plot of sweden counties based on mean income of Young and Adult respectively.)
p<-plot_ly() %>% add_sf(data = swedimage,color=~Young,split=~NAME_1,colors="Purples") 
p2<-plot_ly() %>% add_sf(data = swedimage,color=~Adult,split=~NAME_1,colors="Blues")

#Creating a dataframe with coordinate of Linköping city to show in a map
linkplot<-data.frame("Linköping",58.409814,15.624525)
colnames(linkplot)<-c("City","Latitude","Longitude")

#Plotting Linköping coordinate in pre existing map
p2 %>% add_markers(data=linkplot,x=~Longitude,y=~Latitude,hoverinfo="text",text=~City)

