library(ggplot2)
library(plotly)
library(akima)
library(sf)



#Reading file
swedfile <- read.csv("Swedish_Household.csv")
swedimage <- readRDS("Sweden_counties.rds")

#Renaming the levels of Age to more readable form and modifying the columns in table
levels(swedfile$age) <- c("Young", "Adult", "Senior")
colnames(swedfile) <- c("Region", "Age", "Mean_Income")

#Renaming the county to simpler form.
columnsplit <- strsplit(levels(swedfile$Region), " ")
levels(swedfile$Region) <- sapply(columnsplit, function(x)
    x[[2]])

#Spreading the data from long to wide based on factors of Age
new_swedfile <- tidyr::spread(swedfile, Age, Mean_Income)
#colnames(new_swedfile)<-c("Region","Yo","Mean_Income")
new_swedfile$Region = as.character(new_swedfile$Region)



#Changing the names of region in the file from csv to match region names in sf file
new_swedfile[new_swedfile$Region == "Örebro", ]$Region <- "Orebro"
new_swedfile[new_swedfile$Region == "Västra", ]$Region <- "Västra Götaland"
    

#Violin Plot for three Age group showing mean income in SEK
#Violin Plot for three Age group showing mean income in SEK
p <- ggplot(swedfile, aes(Age, Mean_Income, fill = Age)) + geom_violin() + geom_boxplot(width=0.3, outlier.color="black", fill="white")

p + stat_summary(fun.y = median,
                 geom = "point",
                 color = "Red") + xlab("\n Age Group ") +
                 ylab("Mean Income in SEK\n")

#Doing a cubic interpolation to get continous variable and creating a surace plot
interpolated = interp(new_swedfile$Young,
                      new_swedfile$Adult,
                      new_swedfile$Senior,
                      duplicate = "mean")

#Creating a surface plot
plot_ly(
    x =  ~ interpolated$x,
    y =  ~ interpolated$y,
    z =  ~ interpolated$z,
    type = "surface"
   
    
) %>% layout(scene = list(
    xaxis = list(title = 'X',gridcolor="grey",gridwidth=2),
    yaxis = list(title = 'Y',gridcolor="grey",gridwidth=2),
    zaxis = list(title = 'Z',gridcolor="grey",gridwidth=2)),
    title= "Surface plot showing dependence of Z on X and Y\n"

    )



#Modifying the rownames
rownames(new_swedfile) <- new_swedfile$Region

#Adding Mean values of Young and Adult age group corresponding to the name from
#rds file
swedimage$Young <- new_swedfile[swedimage$NAME_1, "Young"]
swedimage$Adult <- new_swedfile[swedimage$NAME_1, "Adult"]

#Plotting chlorpeth plot of sweden counties based on mean income of Young and Adult respectively.)
p <- plot_ly(width=900) %>% layout(
    plot_bgcolor = "#ebfafa",
    title = "Distribution of Mean Income(SEK thousands) for age group 18-29 <br> in counties of Sweden ",
    margin = list(t = 100, 
                  pad = 10)
) %>%
    add_sf(
        data = swedimage,
        color =  ~ Young,
        colors = c("#66ccff","#000066"),
        split = ~ NAME_1,
        showlegend = FALSE
    ) 
p


p2 <- plot_ly(width = 900) %>% layout(
    plot_bgcolor = "#ebfafa",
    title = "Distribution of Mean Income(SEK thousands) for age group 30-49 <br> in counties of Sweden ",
    margin = list(t = 100, 
                  pad = 10)
) %>% add_sf(
    data = swedimage,
    color =  ~ Adult,
    split =  ~ NAME_1,
    colors = c("#66ccff","#000066"),
    showlegend = FALSE
) 
p2



#Plotting Linköping coordinate in pre existing map
p  %>% add_markers(
    x =  15.624525,
    #Longitude
    y =  58.409814,
    #Latitude
    hoverinfo = "text",
    text =  "Linköping", #City Name to Show
    color=I("#d10404")
) %>% layout(title = "Red dot showing the location of Linköping.")

