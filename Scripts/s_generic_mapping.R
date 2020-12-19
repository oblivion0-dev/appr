workingDirectory  = 'E:/Github/appr/'                   # For Windows
#workingDirectory = 'home/pdell/github/appr/'           # For Linux

setwd(workingDirectory)

# -------------------------------------------------------------------------------------------

# Loading packages from the 'tidyverse' core
library(ggplot2) 

# Mapping-related packages
library(sf)
library(tmap)
library(tmaptools)
library(leaflet)
library(dplyr)

# Loading entire homemade R library
files <- list.files(paste(workingDirectory,'Lib/', sep = ''), pattern = '^.*[Rr]$', include.dirs = FALSE, full.names = TRUE)
for (f in files) {
  source(f)
  print(paste('Loading',f))
}

myData <- read.table("./Data/table.txt", header = TRUE, na.strings = 'NA')
myMap <- st_read("./Data/Shp/0_ALLUVIONS.shp", stringsAsFactors = FALSE)
str(myMap)

# Joining
mapAndData <- inner_join(myData,myMap)  # Jointure par champ commun

# Aesthetic settings
myGraphOptions <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15), hjust = 0.5),
                        plot.subtitle = element_text(hjust = 0.5),
                        legend.title = element_text(colour = "black",  face = "bold.italic", family = "Helvetica"),
                        legend.text = element_text(face = "italic", colour="black",family = "Helvetica"),
                        legend.position="right", legend.direction="vertical",
                        axis.title = element_text(family = "Helvetica", size = (13), colour = "black"),
                        axis.text.y = element_text(family = "Helvetica", colour = "black", size = (11), angle = 90),
                        axis.text.x = element_text(family = "Helvetica", colour = "black", angle = 0, hjust = 1, size = (11)),
                        plot.margin = unit(c(1,1,1,1), "cm"))


# Plotting

ggplot(mapAndData) + 
  myGraphOptions + 
  ggtitle(label = "Figure title",  subtitle = "Figure sub-title") +
  geom_sf(data = mapAndData, aes(fill=Transm, geometry = geometry)) + 
  scale_fill_viridis_c(option = "magma", direction = -1)

