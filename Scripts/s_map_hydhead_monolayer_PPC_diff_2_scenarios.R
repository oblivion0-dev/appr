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
nbSc <- 6

# Scenarios Name
scName <- c("R0.60","R0.70","R0.80","R0.90","R1.00","R1.15")

# Simulation prefixes
simNamePref <- c("SC_1910_R060_","SC_1910_R070_","SC_1910_R080_","SC_1910_R090_","SC_1910_R100_","SC_1910_R115_")

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
#absStartDay <- 154 
absStartDay <- 175 

# Absolute ending day to print map
absEndDay <- 215

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

# Go !
for (sc in 1:nbSc)
{
  totalDayCounter <- 0
  
  # Simulated data storage loop
  for (y in (yearStart:(yearEnd-1)))
  {
    fileSimOne <- paste(workingDirectory,'Data/',simNamePref[sc],"STOP",'/',typeFile,'.',y,y+1,'.bin',sep='')
    fileSimTwo <- paste(workingDirectory,'Data/',simNamePref[sc],"POMP",'/',typeFile,'.',y,y+1,'.bin',sep='')
    
    f_isFileReachable(fileSimOne, 0, 1)
    f_isFileReachable(fileSimTwo, 0, 1)
    
    nbDays <- 365
    if ((y+1)%%4 == 0) nbDays <- 366
    
    binfileOne = file(fileSimOne, 'rb')
    binfileTwo = file(fileSimTwo, 'rb')
    
    print(paste('Reading for binary files in progress...'))
    
    for (d in (1:nbDays))
    {
      totalDayCounter = totalDayCounter + 1
      
      for (r in (1:nbRecAqMbFile))
      {
        nbAqCells = readBin(binfileOne, integer(), size = 4, endian = 'little')
        recValuesOne = readBin(binfileOne, double(), n = nbAqCells, size = 8, endian = 'little')
        nbAqCells = readBin(binfileOne, integer(), size = 4, endian = 'little')
        
        nbAqCells = readBin(binfileTwo, integer(), size = 4, endian = 'little')
        recValuesTwo = readBin(binfileTwo, double(), n = nbAqCells, size = 8, endian = 'little')
        nbAqCells = readBin(binfileTwo, integer(), size = 4, endian = 'little')   
        
        # Extracting simulated hydraulic heads
        if (r == 1)
        {
          if ((totalDayCounter >= absStartDay) && (totalDayCounter <= absEndDay))   # Only print the map if within period
          {
            df <- data.frame(seq(AbsIDs[1], AbsIDs[2]),recValuesOne[AbsIDs[1]:AbsIDs[2]],recValuesTwo[AbsIDs[1]:AbsIDs[2]]) 
            
            df <- cbind(df,df[,2]-df[,3]) 
            if (sc <= 4)
            {
              df[14149,4] = 0.15
              df[14184,4] = 0.15
              df[14263,4] = 0.15
              df[14264,4] = 0.15
              df[14265,4] = 0.15
              
            }
            print(paste(simNamePref[sc],totalDayCounter,"diff INT 14262 :",df[14263,4],sep=" "))
            
            colnames(df)<- c("id_ABS","Charge_Sc_One","Charge_Sc_Two","diff")
            
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
            print(paste('Rendering plot for day :',vecDate[totalDayCounter], sep=""))
            
            pngTitle = paste(scName[sc],"_day_",totalDayCounter,".png",sep="")
            png(file = pngTitle, width = 1200, height = 1200, units = "px")
            
            figTitle <- paste("Configuration ",scName[sc],' - Date : ',vecDate[totalDayCounter])
            
            
            pl <- ggplot(spatialJoin) + myGraphOptions + ggtitle(label = figTitle) +
              geom_sf(data = spatialJoin, aes(fill=diff, geometry = geometry), color="grey90", size = 0.05) +
              scale_fill_gradientn(colours=c("#FFFFFF","#83A7C0","#588DB9","#056EAC","#265281","#193C60","#0A2335"), na.value = "black",limits = c(0,10),breaks=c(0,1,2,3,4,5,6,7,8,9,10)) +
              theme(legend.key.size = unit(10, 'cm')) + 
              theme(legend.key.height= unit(6.5, 'cm')) + 
              theme(legend.key.width= unit(2, 'cm')) + 
              theme(legend.title = element_text(size=25)) +
              theme(legend.text = element_text(size=35)) + 
              labs(fill = "Différentiel (m)")
            
            remove(dataShp,df)
            print(pl)
            dev.off()
          }
        }
      }
    }
    close(binfileOne)
    close(binfileTwo)
    print(paste('Reading for binary files done. File closed.'))
  } 
}

print("Done.")