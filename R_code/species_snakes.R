#Reproducing data from the paper Rapoport's rule and determinants of species range size in snakes

#Library----------------------------------------------------------
library(ggplot2)
library(tidyverse)

#Read Data------------------------------------------------------------------------
Species_range <- read.csv("./Snake_data/species_range.csv")

#Plots-----------------------------------------------------------
#Reproducing sub figures a,b, and d from figure 1
#fig 1a
Species_range$area <- exp(Species_range$log_area) #convert log area to area
fig_1a<- hist(Species_range$area, xlab="area (square km)", main = "Frequency of range sizes across species")
fig_1a
#hist with internal conversion of log area to area
hist(exp(Species_range$log_area), xlab="area (square km)", main="Frequency of range sizes across species") 

#fig 1b
fig_1b <- hist(Species_range$log_area, xlab="log area (square km)", main="Frequency of log range sizes across species")

#fig 1d
cutpoints <- c(seq(-40,50, by=5)) #define ylim range

Species_range$mid_lat_bins<-cut(Species_range$mid_latitude,cutpoints,right = FALSE) #cut function divides range of x into intervals as defined by cutpoints and mid_lat

MidLat_Range_Area<-
  Species_range %>% 
  group_by(mid_lat_bins) %>% 
  summarise(median_range_area=median(exp(log_area)))

## Using ggplot 
fig_1d <- ggplot(data=MidLat_Range_Area,aes(mid_lat_bins,median_range_area)) +
  geom_col() + 
  coord_flip()+
  xlab ("Median Range Area")+
  ylab ("Latitudinal band mid-point (degrees")
fig_1d

## Using barplot function 
ttlabels <- c(seq(-37.5,47.5, by=5))
barplot(MidLat_Range_Area$median_range_area,
        horiz = TRUE,
        names.arg = ttlabels, 
        las=2, xlim=c(0,2000000), xlab="Median Range Area", ylab="Latitudinal band mid-point (degrees)")

#print sub figures a,b, and d of figure 1 onto pdf
pdf("./Plots/Bohm_Figure1.pdf")
hist(Species_range$area, xlab="area (square km)", main = "Frequency of range sizes across species")
hist(Species_range$log_area, xlab="log area (square km)", main="Frequency of log range sizes across species")
fig_1d
dev.off()


#