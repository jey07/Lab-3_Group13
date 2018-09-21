library(ggplot2)

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


#Violin Plot for three Age group showing mean income in SEK 
p<-ggplot(swedfile,aes(Age,Mean_Income,color=Age)) + geom_violin()
p+stat_summary(fun.y=median,geom="point",color="blue") +xlab("\n Age Group ") +
  ylab("Mean Income in SEK\n")


