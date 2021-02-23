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
nbScenarios <- 6

# Scenarios Names
scNames <- c("SC_1910_R060","SC_1910_R070","SC_1910_R080","SC_1910_R080","SC_1910_R100","SC_1910_R115")   # A adapter lorsque 0.9 aura run

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

# (Ox) dates management
vecDate <- seq(as.Date(dateStart), as.Date(dateStart)+nbDays-1, by = 'day')
vecX <- seq(1, nbDays)

# Computing absolute staring and ending layer cells
AbsIDs <- f_InternToAbsIdAquifer(cellsPerLayer,layerID)

# Loading the layer grid's shapefile
myShapefile <- st_read("./Data/GRID_PPC/maillage_PPC.shp", stringsAsFactors = FALSE)

print(paste('Extraction for layer',layerID,' Number of cells :',cellsPerLayer[layerID],'Abs start point :',AbsIDs[1],'Abs end point :',AbsIDs[2]))

# Storage matrix for all results
matResults <- matrix(data = NA, nrow = sum(cellsPerLayer), ncol = nbScenarios*nbDeclin)

countSim <- 0

# Go !
for (sc in (1:nbScenarios))
{
  for (dec in (1:nbDeclin))
  {
    # Initializations
    totalDayCounter <- 0
    countSim <- countSim + 1
    vecMiniH <- replicate(sum(cellsPerLayer), 9999.0)
    vecMaxiH <- replicate(sum(cellsPerLayer), -9999.0)
   
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
          
          print(paste(scNames[sc],'_',declinNames[dec],' - ',totalDayCounter,sep=""))
          
          # Extracting simulated hydraulic heads
          if (r == 1)
          {
            if ((totalDayCounter >= absStartDay) && (totalDayCounter <= absEndDay))
            {
              for (i in (1:sum(cellsPerLayer)))
              {
                if (recValues[i] < vecMiniH[i])
                {
                  vecMiniH[i] <- recValues[i]
                }
                if (recValues[i] > vecMaxiH[i])
                {
                  vecMaxiH[i] <- recValues[i]
                }
              }
            }
          }
        }
      }
      close(binfile)
      print(paste('Reading for binary file :',fileSim,'done. File closed.'))
    }
    
    # Post-traitement de la déclinaison en cours
    
    amplitude <- vecMaxiH-vecMiniH
    matResults[,countSim] <- amplitude
    
    # Plotting for the current scenario and declination
    
    df <- data.frame(seq(AbsIDs[1], AbsIDs[2]),amplitude[AbsIDs[1]:AbsIDs[2]]) 
    colnames(df)<- c("id_ABS","Amplitude")
    
    # Joining
    spatialJoin <- inner_join(df,myShapefile, by = "id_ABS")
        
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
    print("Plotting")
        
    pngTitle = paste(paste("AQ_amplitude_period_",absStartDay,"-",absEndDay,"_",scNames[sc],'_',declinNames[dec],".png",sep=""))
    png(file = pngTitle, width = 1920, height = 1080, units = "px")
        
    figTitle <- paste(scNames[sc],'_',declinNames[dec]," - AQ_amplitude_period_",absStartDay,"-",absEndDay,sep="")
        
    pl <- ggplot(spatialJoin)  + myGraphOptions + ggtitle(label = figTitle) +
         geom_sf(data = spatialJoin, aes(fill=Amplitude, geometry = geometry), color=NA) + 
         theme(legend.key.size = unit(10, 'cm')) + 
         theme(legend.key.height= unit(6.5, 'cm')) + 
         theme(legend.key.width= unit(2, 'cm')) + 
         theme(legend.title = element_text(size=25)) +
         theme(legend.text = element_text(size=35)) + 
         labs(fill = "Amplitude (m)") +
         scale_fill_gradientn(colours=rainbow(10), #c("#ebf5fb","#d6eaf8","#aed6f1","#85c1e9","#5dade2","#3498db","#3498db","#2e86c1","#1f618d","#1b4f72"),
                              na.value = "transparent",guide = "colourbar",aesthetics = "fill",limits=c(0,20),breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))
        #   scale_fill_gradient(low = "blue",high = "red",space = "Lab",)
        #  scale_fill_gradient(low="white",high="blue",space = "Lab")
     #  #   scale_fill_viridis_c(option = "viridis", limits = c(2, 15),direction = -1) +
     #  #      scale_fill_viridis_c(option="magma",space = "Lab",na.value = "transparent",guide = "colourbar",
     # #    aesthetics = "fill", limits=c(20,40), direction=1) +
     
    print(pl)
    dev.off()
    
    remove(df,amplitude)
  }
}

# Writing the global matrix for all runs
dfMat <- as.data.frame(matResults)

columnNames <- vector()
#columnNames <- append(columnNames,"id_ABS")

for (sc in (1:nbScenarios))
{
  for (dec in (1:nbDeclin))
  {
    chaine <- paste(scNames[sc],'_',declinNames[dec],sep="")
    columnNames <- append(columnNames,chaine)
  }
}

colnames(dfMat) <- columnNames
write.table(dfMat, file = "AMPLITUDES.txt")

print("Done.")
