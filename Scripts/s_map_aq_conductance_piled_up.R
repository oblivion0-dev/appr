# Last revision : 09/02/2021

# Script which :
# - Maps the CaWaQS conductance parameter fields (all AQ layers piled up = so it only displays the 
# conductance values (and not the drainance !)

workingDirectory  = 'E:/Github/appr/'  
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
library(scales)
library(patchwork)

# Loading entire homemade R library
files <- list.files(paste(workingDirectory,'Lib', sep = ''), pattern = '^.*[Rr]$', include.dirs = FALSE, full.names = TRUE)
for (f in files) {
  source(f)
  print(paste('Loading',f))
}
 
# USER CORNER ----------------------------------------------------------------------------------
 
# Vector of cell number per layer, from top to bottom
cellsPerLayer <- c(6108,3272,6610,6093,8203,4399,37911)
  
# Relative paths to the folder containing the AQ layer grids
shpList <- c("Data/GRID_AQ/0_ALLUVIONS.shp","Data/GRID_AQ/1_BEAUCE.shp",
             "Data/GRID_AQ/2_BRIE.shp","Data/GRID_AQ/3_CHAMPIGNY.shp",
             "Data/GRID_AQ/4_LUTETIEN.shp","Data/GRID_AQ/5_THANETIEN.shp",
             "Data/GRID_AQ/6_CRAIE.shp")

# Relative paths to the folder containing the parameters files
paramFiles <- c("Data/Conductance_param/COND_DRAIN_37_ALLU.txt","Data/Conductance_param/COND_DRAIN_44_BEAU.txt",
                "Data/Conductance_param/COND_DRAIN_44_BRIE.txt","Data/Conductance_param/COND_DRAIN_44_CHAM.txt",
                "Data/Conductance_param/COND_DRAIN_16_LUTE.txt","Data/Conductance_param/COND_DRAIN_37_THAN.txt",
                "Data/Conductance_param/COND_DRAIN_40_CRAI.txt")

# Application name
modelName <- 'Seine'

# Run name
simname <- 'SAFRAN-1970-2020'

# Parameter name
paramName <- 'Conductance'

# END OF USER CORNER------------------------------------------------------------------------------

nbLayers <- length(cellsPerLayer)
nbCellsAq <- sum(cellsPerLayer)

# Storing parameters values accoring to the ABS AQ identifier
for (i in (1:nbLayers))
{
  # Loading the layer grid's shapefile
  shpAQ <- st_read(shpList[i], stringsAsFactors = FALSE)
  #str(shpAQ)
  
  # Storage of the surfaces' cells 
  areas <- as.vector(st_area(shpAQ))
  
  val <- f_InternToAbsIdAquifer(cellsPerLayer, i)
  f_isFileReachable(paramFiles[i], 0, 1)
  pFile <- read.table(paramFiles[i], header = FALSE, na.strings = 'NA')
  pFile[,1] <- seq(val[1],val[2])
  pFile[,2] <- pFile[,2]*areas

  
  colnames(pFile)<- c("ID_ABS","Cond")
  
  spatialJoin <- inner_join(pFile,shpAQ, by = "ID_ABS")
  
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
  print(paste('Rendering parameter map for layer ',i,sep=" "))
        
  pngTitle = paste(paramName,"_",i,".png",sep="")
  png(file = pngTitle, width = 1920, height = 1080, units = "px")
           
  figTitle <- paste(pngTitle)
  
  pl <-   ggplot(spatialJoin) + myGraphOptions + ggtitle(label = figTitle) +
          geom_sf(data = spatialJoin, aes(fill=Cond, geometry = geometry), color=NA) +
          scale_fill_gradient2(low = muted("red"),mid = "white",high = muted("blue"),
          midpoint = 0,space = "Lab",na.value = "grey50",guide = "colourbar",
          aesthetics = "fill", limits=c(0.001,100),trans = "log")
  
  if (!exists("plAll"))
  {
    plAll <- ggplot()
    plAll <- pl  }
  else
  {
    plAll <- plAll + pl
  }
    
  print(plAll)
  
  dev.off()
}

print("Program exited successfully.")