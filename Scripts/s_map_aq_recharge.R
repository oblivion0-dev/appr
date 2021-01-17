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
 
# Loading entire homemade R library
files <- list.files(paste(workingDirectory,'Lib', sep = ''), pattern = '^.*[Rr]$', include.dirs = FALSE, full.names = TRUE)
for (f in files) {
 source(f)
 print(paste('Loading',f))
}
 
# USER CORNER ----------------------------------------------------------------------------------

# Type of CaWaQS-output file what are being read 
outputType <- "AQ_MB"
 
# CaWaQS version
versionID <- 2.91

# Model name
modelName <- 'Seine-8C'

# Simulation name
simname <- 'SAFRAN_1970-2020'

# Starting year of simulation
yearStart <- 1970
 
# Ending year of simulation
yearEnd <- 1974
 
# Vector of cell number per layer, organized from top to bottom
cellsPerLayer <- c(6108,3272,6610,6093,8203,4399,37911,34880)
 
# Absolute path to the folder containing the overall unique AQ-aff layer
shpAQAff <- "E:/Github/appr/Data/AQ_AFFLEURANT.shp"

# Absolute path to the folder containing the output binary files
simFolder <- 'C:/Users/Nicolas/Desktop/binaries'

# Absolute path to the NSAT-AQ text file (Col1 : NSAT cell ID, Col2 : Layer ID (0), Col3 : Int_ID (0), Cell Surface (m2))
linkFile <- 'E:/Github/appr/Data/Corresp_NSAT_AQ_AFF.txt'

# END OF USER CORNER------------------------------------------------------------------------------

# Setting the number of binary records
nbRecBinFile <- f_setNbRecOutputs(outputType)
totalDayCounter <- 0

# NSAT-AQ link file storage
linkFile <- read.table(linkFile, header = TRUE, na.strings = 'NA')

# Absolute IDs calculations for all cells
AbsIDAq <- vector(mode = 'integer', length = length(linkFile[,1]))
linkFile <- cbind(linkFile,AbsIDAq)

print("Converting intern AQ IDs to absolute IDs for all cells...")
for (i in (1:length(linkFile[,1])))
{
 linkFile[i,5] <- f_Int2AbsAqID(cellsPerLayer,linkFile[i,2],linkFile[i,3])
}

# Linking Absolute Cells IDs and their sizes
temp <- cbind(linkFile[,5],linkFile[,4])
linkAbsSize <- test[order(temp[,1],decreasing = F),]

# Loading the layer grid's shapefile
shpAQ <- st_read(shpAQAff, stringsAsFactors = FALSE)
#str(shpAQ)

# Simulated data storage loop
for (y in (yearStart:(yearEnd-1)))
{
    fileSim <- paste(simFolder,'/',outputType,'.',y,y+1,'.bin',sep='')
    f_isFileReachable(fileSim, 0, 1)
    
    nbDays <- 365
    if ((y+1)%%4 == 0) nbDays <- 366
    
    binfile = file(fileSim, 'rb')
    print(paste('Reading for binary file :',fileSim,'in progress...'), sep=" ")
    
    # Vector of cumulative recharge over the year for all AQ cells, in mm/an
    rechAQyear <- vector(mode = 'double', length = sum(cellsPerLayer))
    
    for (d in (1:nbDays))
    {
      totalDayCounter = totalDayCounter + 1
      
      for (r in (1:nbRecBinFile))
      {
        nbAqCells = readBin(binfile, integer(), size = 4, endian = 'little')
        recValues = readBin(binfile, double(), n = nbAqCells, size = 8, endian = 'little')
        nbAqCells = readBin(binfile, integer(), size = 4, endian = 'little')
        
        # Extracting AQ-recharge discharges rates (in m3/s)
        if (r == 9) 
        {
          rechAQyear <- rechAQyear + recValues*86400.
        }
      }
    }
    close(binfile)
    print(paste('Reading for binary file :',fileSim,'done. File closed.', sep = ' '))   
    
    # Projection onto the NSAT shapefile
    
    # Conversion into mm/year
 #   for (i in (1:sum(cellsPerLayer)))
#    {
#      rechAQyear[i] <- (rechAQyear[i]/(linkAbsSize[i,2]))*1.e3
#      print(paste(i,rechAQyear[i]))
#    }
          
          
          
     df <- data.frame(seq(AbsIDs[1], AbsIDs[2]),recValues[AbsIDs[1]:AbsIDs[2]]) 
#       colnames(df)<- c("ID_geo","Piezo")
#          
   # Joining
#   spatialJoin <- inner_join(df,myShapefile, by = "ID_geo")
         
   # Aesthetic settings
#   myGraphOptions <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15), hjust = 0.5),
#                           plot.subtitle = element_text(hjust = 0.5),
#                           legend.title = element_text(colour = "black",  face = "bold.italic", family = "Helvetica"),
#                           legend.text = element_text(face = "italic", colour="black",family = "Helvetica"),
#                           legend.position="right", legend.direction="vertical",
#                           axis.title = element_text(family = "Helvetica", size = (13), colour = "black"),
#                           axis.text.y = element_text(family = "Helvetica", colour = "black", size = (11), angle = 90),
#                           axis.text.x = element_text(family = "Helvetica", colour = "black", angle = 0, hjust = 1, size = (11)),
#                           plot.margin = unit(c(1,1,1,1), "cm"))
         
   # Plotting
#   print(paste('Rendering plot for year : ',y,y+1, sep=""))
#         
#         pngTitle = paste("Piezo_day_",totalDayCounter,".png",sep="")
#         png(file = pngTitle, width = 1920, height = 1080, units = "px")
#         
#         figTitle <- paste('Piezo - Day',vecDate[totalDayCounter])
#         
#         pl <- ggplot(spatialJoin) + 
#           myGraphOptions + 
#           ggtitle(label = figTitle,  subtitle = "Figure sub-title") +
#           geom_sf(data = spatialJoin, aes(fill=Piezo, geometry = geometry), color=NA) + 
#           scale_fill_viridis_c(option = "magma", limits = c(0, 200),oob = scales::squish,direction = -1)
#        
#         print(pl)
#         
#         dev.off()
#       }
#     }
#   }

}
 
print("Done.")