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

# Constants definitions (HANDS OFF ! Unless you know what you're messing with.) ----------------

# Number of binary records in CaWaQS MB_AQ file
nbRecAqMbFile <- 16  

# Total number of criteria
nbCrit <- 5 

# USER INPUTS ----------------------------------------------------------------------------------

# Starting year of simulation
yearStart <- 2018

# Ending year of simulation
yearEnd <- 2020

# Piezometer attributes text file
caracPiezo <- paste(workingDirectory,'Data/liste_piezos.txt', sep = '')
f_FileInformation('piezo_info')

# Simulation start date (in 'aaaa-mm-dd' format)
dateStart <- '2018-08-01' 

# Simulation length (in days)
nbDays <- 731 

# CaWaQS date offfset (correspond to the CaWaQS starting simulation date - 1)
dateCawOffset <- 61572    # 2018-07-31

# Vector of cell number per layer
cellsPerLayer <- c(6108,3272,6610,6093,8203,4399,37911,34880)

# Layer ID (starts at 1)
layerID <- 1 # Alluvions

# -----------------------------------------------------------------------------------------------

# Loading piezometers properties
properties <- read.table(caracPiezo, header = FALSE, na.strings = 'NA')
nbPiezo <- length(properties[,1])

# (Ox) dates management
vecDate <- seq(as.Date(dateStart), as.Date(dateStart)+nbDays-1, by = 'day')
vecX <- seq(1, nbDays)

# Initializations
matData <- matrix(data = NA, nrow = nbDays, ncol = 2*nbPiezo+1)
totalDayCounter <- 0

# Loading date values in main data matrix
matData[,1] <- vecX

# Computing absolute staring and ending layer cells
AbsIDs <- f_InternToAbsIdAquifer(cellsPerLayer,layerID)

# Loading the layer grid's shapefile
myShapefile <- st_read("./Data/Shp/0_ALLUVIONS.shp", stringsAsFactors = FALSE)
# str(myMap)


print(paste('Extraction for layer',layerID,' Number of cells :',cellsPerLayer[layerID],'Abs start point :',AbsIDs[1],'Abs end point :',AbsIDs[2]))


# Simulated data storage loop
for (y in (yearStart:(yearEnd-1)))
{
  fileSim <- paste(workingDirectory,'Data/AQ_MB.',y,y+1,'.bin',sep='')
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
        df <- data.frame(seq(AbsIDs[1], AbsIDs[2]),recValues[AbsIDs[1]:AbsIDs[2]]) 
        colnames(df)<- c("ID_geo","Piezo")
         
        # Joining
        spatialJoin <- inner_join(df,myShapefile, by = "ID_geo")
        
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
        print(paste('Rendering plot for day :',vecDate[totalDayCounter], sep=""))
        
        pngTitle = paste("Piezo_day_",totalDayCounter,".png",sep="")
        png(file = pngTitle, width = 1920, height = 1080, units = "px")
        
        figTitle <- paste('Piezo - Day',vecDate[totalDayCounter])
        
        pl <- ggplot(spatialJoin) + 
          myGraphOptions + 
          ggtitle(label = figTitle,  subtitle = "Figure sub-title") +
          geom_sf(data = spatialJoin, aes(fill=Piezo, geometry = geometry), color=NA) + 
          scale_fill_viridis_c(option = "magma", limits = c(0, 200),oob = scales::squish,direction = -1)
       
        print(pl)
        
        dev.off()
      }
    }
  }
  close(binfile)
  print(paste('Reading for binary file :',fileSim,'done. File closed.'))
}

print("Done.")

# Need to add rendering to video file