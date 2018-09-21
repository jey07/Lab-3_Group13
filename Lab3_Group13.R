

#Reading file
swedfile<-read.csv("Swedish_Household.csv")

#Renaming the levels of Age to more readable form and modifying the columns in table
levels(swedfile$age)<-c("Young","Adult","Senior")
colnames(swedfile)<-c("Region","Age","Mean_Value(SEK thousands)")

#Renaming the county to simpler form.
columnsplit<-strsplit(levels(swedfile$Region)," ")
levels(swedfile$Region)<-sapply(columnsplit,function(x) x[[2]])


swedfile$Young<-NULL ; swedfile$Adult<-NULL ; swedfile$Old<-NULL