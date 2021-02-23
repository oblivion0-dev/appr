# Clearing everything...
rm(list = ls())

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
library(viridis) 

# Loading entire homemade R library
files <- list.files(paste(workingDirectory,'Lib/', sep = ''), pattern = '^.*[Rr]$', include.dirs = FALSE, full.names = TRUE)
for (f in files) {
  source(f)
  print(paste('Loading',f))
}

# USER INPUTS ----------------------------------------------------------------------------------

# Number of scenarios
nbScenarios <- 2

# Scenarios Names
scNames <- c("SC_1910_R100","SC_1910_R115")

# Declination number of each scenarios
nbDeclin <- 4

# Declination names
declinNames <- c("STOP","TOUS","POMP","NONE")

# CaWaQS output file type
typeFile <- "AQ_H"

# Starting year of simulation
yearStart <- 1909

# Ending year of simulation
yearEnd <- 1910

# Path of the MNT file
FileMNT <- paste(workingDirectory,'Data/COTE_MNT.txt', sep = '')

# Simulation start date (in 'aaaa-mm-dd' format)
dateStart <- '1909-08-01' 

# Simulation length (in days)
nbDays <- 365 

# Vector of cell number per layer
cellsPerLayer <- c(16465,6192)

# Layer ID (starts at 1)
layerID <- 1 # Couche PPC

# Absolute starting day to print map (1 = First day of the [01/08/yearStart:31/07/yearEnd] period)
absStartDay <- 150 

# Absolute ending day to print map
absEndDay <- 250

# -----------------------------------------------------------------------------------------------

nbRecAqMbFile <- f_setNbRecOutputs(typeFile) 
print(paste("Number of daily binary records set to",nbRecAqMbFile,sep=" "))

# Loading piezometers properties
coteMNT <- read.table(FileMNT, header = TRUE, na.strings = 'NA')

# (Ox) dates management
vecDate <- seq(as.Date(dateStart), as.Date(dateStart)+nbDays-1, by = 'day')
vecX <- seq(1, nbDays)

# Computing absolute staring and ending layer cells
AbsIDs <- f_InternToAbsIdAquifer(cellsPerLayer,layerID)

# Loading the layer grid's shapefile
myShapefile <- st_read("./Data/GRID_PPC/maillage_PPC.shp", stringsAsFactors = FALSE)

# Storage matrix for all results
matResults <- matrix(data = NA, nrow = sum(cellsPerLayer), ncol = nbScenarios*nbDeclin)

print(paste('Extraction for layer',layerID,' Number of cells :',cellsPerLayer[layerID],'Abs start point :',AbsIDs[1],'Abs end point :',AbsIDs[2]))

countSim <- 0

# Go !
for (sc in (1:nbScenarios))
{
  for (dec in (1:nbDeclin))
  {
    
    # Initializations
    totalDayCounter <- 0
    countSim <- countSim + 1
    
    # Simulated data storage loop
    for (y in (yearStart:(yearEnd-1)))
    {
      fileSim <- paste(workingDirectory,'Data/',scNames[sc],'_',declinNames[dec],'/',typeFile,'.',y,y+1,'.bin',sep='')
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
          
          # Extracting simulated hydraulic heads
          if (r == 1)
          {
            if ((totalDayCounter >= absStartDay) && (totalDayCounter <= absEndDay))   # Only print the map if within period
            {
              df <- data.frame(seq(AbsIDs[1], AbsIDs[2]),recValues[AbsIDs[1]:AbsIDs[2]]) 
              
              colnames(df)<- c("id_ABS","Charge")
              dataShp <- merge(df,coteMNT,by="id_ABS") 
              diff <- dataShp[,2]-dataShp[,5]
              dataShp <- cbind(dataShp,diff) # Operating on the mean value of the MNT
              
              matResults[,countSim] <- diff
              
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
              
              figTitle <- paste('Sim : ',simName,' - Date : ',vecDate[totalDayCounter])
              
              
              pl <- ggplot(spatialJoin) + myGraphOptions + ggtitle(label = figTitle) +
                geom_sf(data = spatialJoin, aes(fill=diff, geometry = geometry), color=NA) +
                scale_fill_gradientn(colours=c("deepskyblue1","deepskyblue2","deepskyblue3","blue","orangered1","orangered2","red","orchid"), na.value = "grey98",limits = c(-12,12)) +
                theme(legend.key.size = unit(10, 'cm')) + 
                theme(legend.key.height= unit(6.5, 'cm')) + 
                theme(legend.key.width= unit(2, 'cm')) + 
                theme(legend.title = element_text(size=25)) +
                theme(legend.text = element_text(size=35)) + 
                labs(fill = "Hauteur / MNT25 (m)")
              
              remove(dataShp,df)
              print(pl)
              dev.off()
            }
          }
        }
      }
      close(binfile)
      print(paste('Reading for binary file :',fileSim,'done. File closed.'))
    }
    
    
    
    
    
    
  }
}













print("Done.")