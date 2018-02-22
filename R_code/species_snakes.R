#Reproducing data from the paper Rapoport's rule and determinants of species range size in snakes

#Library------------------------------------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(RColorBrewer)

#Read Data----------------------------------------------------------------------------------
Species_range <- read.csv("./Snake_data/species_range.csv")

#Plots--------------------------------------------------------------------------------------

#Reproducing figure 1a,b, and d
#fig 1a----------------------------------------------------------------

Species_range$area <- exp(Species_range$log_area) #convert log area to area
fig_1a<- hist(Species_range$area, xlab="area (sq km)", main = "Frequency of range sizes across species")
fig_1a
#hist with internal conversion of log area to area
hist(exp(Species_range$log_area), xlab="area (sq km)", main="Frequency of range sizes across species") 


#fig 1b----------------------------------------------------------------

fig_1b <- hist(Species_range$log_area, xlab="log area (sq km)", main="Frequency of log range sizes across species")


#fig 1d---------------------------------------------------------------

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
hist(Species_range$area, xlab="area (sq km)", main = "Frequency of range sizes across species")
hist(Species_range$log_area, xlab="log area (sq km)", main="Frequency of log range sizes across species")
fig_1d
dev.off()


#Reproducing figure 2a-c
#fig_2a-------------------------------------------------------------------------
Species_range$sqrt_no_habitats <- sqrt(Species_range$No_habitats)
fig_2a_reg <- lm(log_area~sqrt_no_habitats, data=Species_range)
fig_2a <- plot(log_area~sqrt_no_habitats, data=Species_range, pch=20, 
               xlab="Sqrt Habitat Breath (No. of Habitats)", ylab="Log range size (sq km)")
abline(fig_2a_reg) #add regression line for fig_2a

#fig_2b-------------------------------------------------------------------------
fig_2b_reg <- lm(log_area~logSVL, data=Species_range)
fig_2b <- plot(log_area~logSVL, data=Species_range, pch=20,
               xlab="Log body size (max SVL)", ylab="Log range size (sq km)")
abline(fig_2b_reg)

#fig_2c------------------------------------------------------------------------
Species_range$sqrt_Altitude_AAD <- sqrt(Species_range$Altitude_AAD)
fig_2c_reg <- lm(log_area~sqrt_Altitude_AAD, data=Species_range)
fig_2c <- plot(log_area~sqrt_Altitude_AAD, data=Species_range, pch=20,
               xlab="Sqrt altitude (AAD)", ylab="Log range size (sq km)")
abline(fig_2c_reg)

#Print figure 2a-c onto a pdf
pdf("./Plots/Bohm_figure2.pdf")
fig_2a <- plot(log_area~sqrt_no_habitats, data=Species_range, pch=20, 
               xlab="Sqrt Habitat Breath (No. of Habitats)", ylab="Log range size (sq km)")
abline(fig_2a_reg)
fig_2b <- plot(log_area~logSVL, data=Species_range, pch=20,
               xlab="Log body size (max SVL)", ylab="Log range size (sq km)")
abline(fig_2b_reg)
fig_2c <- plot(log_area~sqrt_Altitude_AAD, data=Species_range, pch=20,
               xlab="Sqrt altitude (AAD)", ylab="Log range size (sq km)")
abline(fig_2c_reg)
dev.off()

#Ecoregions-----------------------------------------------------------------------
#what is the mean number of ecoregions?
summary(Species_range$no_ecoregions)
#the mean number of ecoregions was determined to be 12.52, therefore generalists will have regions
#greater than the mean.
Species_range$Ecoregion_type[Species_range$no_ecoregions>12.52] <- "Generalist"
Species_range$Ecoregion_type[Species_range$no_ecoregions<=12.52] <- "Specialist"
Species_range$Ecoregion_type <- as.factor(Species_range$Ecoregion_type)

#Figure 4-------------------------------------------------------------------------
#reproducing figure 4, with color adjusted by Ecoregion_type
cols <- brewer.pal(n=4, name="Set2")
cols_ecoregion <- cols[Species_range$Ecoregion_type] #set color
Species_range$Sqrt_YearsDescrip <- sqrt(Species_range$Years_descrip) #change years described to sqrt
fig_4_reg <- lm(Sqrt_YearsDescrip~log_area, data=Species_range)
fig_4 <- plot(Sqrt_YearsDescrip~log_area, data=Species_range, pch=20, col=cols_ecoregion,
              xlab="Log range size (sq km)", ylab="Sqrt years since description")
abline(fig_4_reg)

#print figure 4 onto pdf to save in figs folder
pdf("./Plots/Bohm_figure4.pdf")
fig_4 <- plot(Sqrt_YearsDescrip~log_area, data=Species_range, pch=20, col=cols_ecoregion,
              xlab="Log range size (sq km)", ylab="Sqrt years since description")
abline(fig_4_reg)
dev.off()
