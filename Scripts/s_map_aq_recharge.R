# Last revision : 18-01-2021 : 11:07:22

# Script which :
# - Maps the annual aquifer recharge (directly projected on the AQ-aff layer), in mm/year
# - Plots the annual recharge over the entire aquifer system.

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
library(data.table)

# Loading entire homemade R library
files <- list.files(paste(workingDirectory,'Lib', sep = ''), pattern = '^.*[Rr]$', include.dirs = FALSE, full.names = TRUE)
for (f in files) {
  source(f)
  print(paste('Loading',f))
}
 
# USER CORNER ----------------------------------------------------------------------------------

# Type of CaWaQS-output file which will be read
outputType <- "AQ_MB"
 
# Current CaWaQS version
versionID <- 2.91

# Application name
modelName <- 'Seine-8C'

# Run name
simname <- 'SAFRAN_1970-2020'

# Starting year of simulation
yearStart <- 1970
 
# Ending year of simulation
yearEnd <- 1974
 
# Vector of cell number per layer, from top to bottom
cellsPerLayer <- c(6108,3272,6610,6093,8203,4399,37911,34880)
 
# Absolute path to the folder containing the AQ-aff layer
shpAQAff <- "E:/Github/appr/Data/AQ_AFFLEURANT.shp"

# Absolute path to the folder containing the output binary files
simFolder <- 'C:/Users/Nicolas/ownCloud/Script_recharge'

# Absolute path to the NSAT-AQ text file (Col1 : NSAT cell ID, Col2 : Layer ID (0), Col3 : Int_ID (0), Cell Surface (m2))
linkFile <- 'E:/Github/appr/Data/Corresp_NSAT_AQ_AFF.txt'

# END OF USER CORNER------------------------------------------------------------------------------

# Automatically setting up the number of daily records expected to be read
nbRecBinFile <- f_setNbRecOutputs(outputType)

# Layer number of the application
nbLayer <- length(cellsPerLayer)

# NSAT-AQ link file storage in a data table
linkFile <- fread(linkFile, header = TRUE, na.strings = 'NA') # data table

nbNSATcells <- length(linkFile[,ID_unique])
print(nbNSATcells)

# Absolute IDs calculations for all cells
AbsIDAq <- vector(mode = 'integer', length = nbNSATcells)
linkFile <- cbind(linkFile,AbsIDAq)

# -------------------------------------------------------------------------------------
print("Converting intern AQ IDs to absolute IDs for all cells...")
sum <-0
cumIDs <- vector(mode = "integer", length = nbLayer)

for (i in (1:nbLayer))
{
  sum <- sum + 1
  cumIDs[i] <- sum
  sum <- sum+cellsPerLayer[i]-1
} 
print(cumIDs)
break()
for (i in (1:nbNSATcells))
{
  if( i%%5000 == 0 ) print(i)
 # linkFile[i,5] <- f_Int2AbsAqID(cellsPerLayer,linkFile[i,2],linkFile[i,3])
 # linkFile[i,AbsIDAq] <- f_Int2AbsAqID(cellsPerLayer,linkFile[i,ID_LAYER],linkFile[i,Id_AQ_Int])
#  values <- f_Int2AbsAqID(cellsPerLayer,linkFile[i,ID_LAYER],linkFile[i,Id_AQ_Int])
 # print(paste(i,values))
#  print (i)
 # set(linkFile, 1, AbsIDAq, 999)
  absValue <- cumIDs[linkFile[i,ID_LAYER]+1] + linkFile[i,Id_AQ_Int]
  
  linkFile[i, AbsIDAq := absValue ]
}
print("Done !")

# AQ-aff grid area (in m2)
AqAffArea <- sum(linkFile[,4])
print(paste("Aq-aff area :",AqAffArea*1.e-6,"km2",sep = " "))

# Loading the layer grid's shapefile
shpAQ <- st_read(shpAQAff, stringsAsFactors = FALSE)
str(shpAQ)
 
# Simulated data storage loop
for (y in (yearStart:(yearEnd-1)))
{
  fileSim <- paste(simFolder,'/',outputType,'.',y,y+1,'.bin',sep='')
  f_isFileReachable(fileSim, 0, 1)
     
  nbDays <- 365
  if ((y+1)%%4 == 0) nbDays <- 366
     
  binfile = file(fileSim, 'rb')
  print(paste('Reading for binary file :',fileSim,'in progress...'), sep=" ")
     
  # Vector of cumulative recharge over the year in mm (vector sized over NSAT cells)
  rechAQyear <- vector(mode = 'double', length = length(linkFile[,1]))
  totalVolume <- 0
  
  for (d in (1:nbDays))
  {
    print(d)
    for (r in (1:nbRecBinFile))
    {
      nbAqCells = readBin(binfile, integer(), size = 4, endian = 'little')
      recValues = readBin(binfile, double(), n = nbAqCells, size = 8, endian = 'little')
      nbAqCells = readBin(binfile, integer(), size = 4, endian = 'little')
         
      # Extracting AQ-recharge discharges rates and directly projecting onto the AQ-aff cells, in mm/year
      
      if (r == 9) 
      {
        for (c in (1:nbNSATcells))
        {
        #  id <- linkFile[c,AbsIDAq]
        #totalVolume <- totalVolume + (recValues[id]*86400.)
      #    rechAQyear[linkFile[c,1]] <- rechAQyear[linkFile[c,1]] + ((recValues[linkFile[c,5]]*86400.)/(linkFile[c,4]))*1.e-3 # mm/day
        }
        break()
      }
    }
    print(paste("Day",d,"Rech",rechAQyear[1]))
  }
  close(binfile)
  print(paste('Reading for binary file :',fileSim,'done. File closed.', sep = ' '))   

  # Printing overall yearly recharge
  print(paste("Yearly period :",y,y+1,"- Aquifer recharge : ",totVol/AqAffArea*1.e3,"mm/year",sep = " "))
  
  # Spatial join
  df <- data.frame(linkFile[,1],rechAQyear) 
  colnames(df)<- c("ID_unique","Rech")
         
  spatialJoin <- inner_join(df,shpAQ, by = "ID_unique")
          
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
  print(paste('Rendering AQ-recharge map for :',y,y+1,'period.',sep=" "))
       
  pngTitle = paste("Aq_rech_",y,y+1,".png",sep="")
  png(file = pngTitle, width = 1920, height = 1080, units = "px")
        
  figTitle <- paste('Aquifer recharge - Year : ',y,y+1)
         
  pl <- ggplot(spatialJoin) + 
        myGraphOptions + 
        ggtitle(label = figTitle) + #,  subtitle = "Figure sub-title") +
        geom_sf(data = spatialJoin, aes(fill=Rech, geometry = geometry), color=NA) + 
        scale_fill_viridis_c(option = "magma", limits = c(0, 200),oob = scales::squish,direction = -1) +
        labs(caption = paste('CaWaQS',versionID,' - ',modelName,' application - Simulation : ',simname,sep=''))
        
  print(pl)
       
  dev.off()

}
  
# Recharge plot attributes
 
# Plotting
# dataFrame <- as.data.frame(matData)    
# 
# pl =  ggplot(dataFrame) +
#   geom_line(aes(x = dataFrame[,1], y = dataFrame[,2*i+1], color = 'Simulated head'), size = 0.4, alpha = 0.8)  +
#   geom_point(aes(x = dataFrame[,1], y = dataFrame[,2*i], color = 'Measured head'), size = 0.6, alpha = 0.3) +
#   ggtitle(label = "CaWaQS AQ recharge" + subtitle = "statLabel") +
#   labs(x = 'Time (years)', y = 'AQ rechar (mm)', 
#        caption = paste('CaWaQS',versionID,' - ',modelName,' application - Simulation : ',simname,sep=''))
# 
# 
# print(pl + myGraphOptions + scale_color_manual(values=c("red", "blue")) +
#         labs(color = "Color code : ") +
#         scale_x_continuous(limits=c(startGraph, endGraph)) + ggtitle(label = "toto", subtitle = "statLabel"))
# 
# 

print("Program exited successfully.")