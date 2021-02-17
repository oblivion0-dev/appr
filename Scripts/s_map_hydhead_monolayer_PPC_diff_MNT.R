rm(list = ls())


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
library(scales)
library(patchwork)
library(viridis) 
library(gganimate)
library(av)

# Loading entire homemade R library
files <- list.files(paste(workingDirectory,'Lib/', sep = ''), pattern = '^.*[Rr]$', include.dirs = FALSE, full.names = TRUE)
for (f in files) {
  source(f)
  print(paste('Loading',f))
}


# Constants definitions (HANDS OFF ! Unless you know what you're messing with.) ----------------

# Number of binary records in CaWaQS MB_AQ file
nbRecAqMbFile <- 1  

# Total number of criteria
nbCrit <- 5 

# USER INPUTS ----------------------------------------------------------------------------------

# Starting year of simulation
yearStart <- 1909

# Ending year of simulation
yearEnd <- 1910

# MNT
FileMNT <- paste(workingDirectory,'Data/COTE_MNT.txt', sep = '')

# Simulation start date (in 'aaaa-mm-dd' format)
dateStart <- '1909-08-01' 

# Simulation length (in days)
nbDays <- 365 

# CaWaQS date offfset (correspond to the CaWaQS starting simulation date - 1)
#dateCawOffset <- 61572    # 2018-07-31

# Vector of cell number per layer
cellsPerLayer <- c(16465,6192)

# Layer ID (starts at 1)
layerID <- 1 # Couche PPC

# 
absStartDay <- 154 

#
absEndDay <- 243

# -----------------------------------------------------------------------------------------------

# Loading piezometers properties
coteMNT <- read.table(FileMNT, header = TRUE, na.strings = 'NA')

# (Ox) dates management
vecDate <- seq(as.Date(dateStart), as.Date(dateStart)+nbDays-1, by = 'day')
vecX <- seq(1, nbDays)

# Initializations
totalDayCounter <- 0

# Computing absolute staring and ending layer cells
AbsIDs <- f_InternToAbsIdAquifer(cellsPerLayer,layerID)

# Loading the layer grid's shapefile
myShapefile <- st_read("./Data/GRID_PPC/maillage_PPC.shp", stringsAsFactors = FALSE)

print(paste('Extraction for layer',layerID,' Number of cells :',cellsPerLayer[layerID],'Abs start point :',AbsIDs[1],'Abs end point :',AbsIDs[2]))

# Simulated data storage loop
for (y in (yearStart:(yearEnd-1)))
{
  fileSim <- paste(workingDirectory,'Data/OUTPUT_SC_1910_R100_NONE/AQ_H.',y,y+1,'.bin',sep='')
  f_isFileReachable(fileSim, 0, 1)
  
  nbDays <- 365
  if ((y+1)%%4 == 0) nbDays <- 366
  
  binfile = file(fileSim, 'rb')
  print(paste('Reading for binary file :',fileSim,' in progress...'))
  
  for (d in (1:nbDays))
  {
    totalDayCounter = totalDayCounter + 1
    
    for (r in (1:nbRecAqMbFile))
    {
      nbAqCells = readBin(binfile, integer(), size = 4, endian = 'little')
      recValues = readBin(binfile, double(), n = nbAqCells, size = 8, endian = 'little')
      nbAqCells = readBin(binfile, integer(), size = 4, endian = 'little')
      
    #  print(totalDayCounter)
      
      # Extracting simulated hydraulic heads
      if (r == 1)
      {
        if ((totalDayCounter >= absStartDay) && (totalDayCounter <= absEndDay))
        {
          df <- data.frame(seq(AbsIDs[1], AbsIDs[2]),recValues[AbsIDs[1]:AbsIDs[2]]) 
          colnames(df)<- c("id_ABS","Charge")
         
          dataShp <- merge(df,coteMNT,by="id_ABS") 
          
          diff <- dataShp[,2]-dataShp[,5]
          
          dataShp <- cbind(dataShp,diff)
          colnames(dataShp)<- c("id_ABS","Charge","min","max","moy","diff")   

          # Joining
          spatialJoin <- inner_join(dataShp,myShapefile, by = "id_ABS")

          # Aesthetic settings
          myGraphOptions <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (33), hjust = 0.5),
                                  plot.subtitle = element_text(hjust = 0.5),
                                  legend.title = element_text(colour = "black",  face = "bold.italic", family = "Helvetica"),
                                  legend.text = element_text(face = "italic", colour="black",family = "Helvetica"),
                                  legend.position="right", legend.direction="vertical",
                                  axis.title = element_text(family = "Helvetica", size = (33), colour = "black"),
                                  axis.text.y = element_text(family = "Helvetica", colour = "black", size = (15), angle = 90, hjust = 0.5),
                                  axis.text.x = element_text(family = "Helvetica", colour = "black", angle = 0, hjust = 0.5, size = (15)),
                                  plot.margin = unit(c(1,1,1,1), "cm"))
          
          # Plotting
          print(paste('Rendering plot for day :',vecDate[totalDayCounter], sep=""))
          
          pngTitle = paste("Piezo_jour_",totalDayCounter,".png",sep="")
          png(file = pngTitle, width = 1920, height = 1080, units = "px")
          
          figTitle <- paste('Date : ',vecDate[totalDayCounter])

          
          pl <- ggplot(spatialJoin) + myGraphOptions + ggtitle(label = figTitle) +
                geom_sf(data = spatialJoin, aes(fill=diff, geometry = geometry), color=NA) + 
                scale_fill_gradientn(colours=c("coral","coral1","coral2","coral3","orangered4",
                                               "deepskyblue1","blue","blue4"), na.value = "grey98",limits = c(-15,9)) +
                theme(legend.key.size = unit(10, 'cm')) + 
                theme(legend.key.height= unit(4, 'cm')) + 
                theme(legend.key.width= unit(2, 'cm')) + 
                theme(legend.title = element_text(size=25)) +
                theme(legend.text = element_text(size=35)) + labs(fill = "Hauteur / MNT25 (m)")

          remove(diff,dataShp,df)
          print(pl)
          dev.off()
        }
      }
    }
  }
  close(binfile)
  print(paste('Reading for binary file :',fileSim,'done. File closed.'))
}

print("Done.")