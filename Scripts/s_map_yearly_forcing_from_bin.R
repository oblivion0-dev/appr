# Scripts mapping annual cumulative values of forcings on the forcing grid
# and also the interannual values over the entire period

# Setting working directory
workingDirectory  = 'E:/Github/appr/'                   # For Windows
#workingDirectory = '/home/nicolas/GitHub/appr/'           # For Linux
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

# USER INPUTS ----------------------------------------------------------------------------------

# Starting year of forcing files set
yearStart <- 1980

# Ending year of forcing files set
yearEnd <- 2018

# Number of forcing cells over the model
nbForcingCells <- 1490

# Forcing variable name (or type)
varName <- "runoff"

# -----------------------------------------------------------------------------------------------

nbYears <- yearEnd-yearStart+1
maxAllValues <- -9999

# Loading the forcing grid's shapefile
forcingShp <- st_read("./Data/GRID_ORCHIDEE.shp", stringsAsFactors = FALSE)
str(forcingShp)

# Forcing data reading loop for all yearly files

# Vector of mean values calculated over pluriannual time period
vecMean <- rep(0,nbForcingCells)
   
for (y in (yearStart:(yearEnd-1)))
{
  # Opening yearly binary file
  forcingBinFile <- paste(workingDirectory,'Data/BIN_FORCINGS/runoff.',y,y+1,'.dir',sep="")
     
  # Existence test, stops the program if unreachable
  f_isFileReachable(forcingBinFile, 0, 1) 
     
  nbDays <- 365
  if ((y+1)%%4 == 0) nbDays <- 366
        
  binfile = file(forcingBinFile, 'rb')
  print(paste('Reading for binary file :',forcingBinFile,' in progress...'))
       
  # Vector of cumulative forcing values
  vecYearSum <- rep(0,nbForcingCells)
    
  for (d in (1:nbDays))
  {
    # Forcing values are read as floats in CaWaQS, and not double !
    recValues = readBin(binfile, double(), n = nbForcingCells, size = 4, endian = 'little')   
    vecYearSum <- vecYearSum + recValues
  }
  close(binfile)
  print(paste('Reading for binary file :',binfile,'done. File closed.'))
     
  # --------------------------- PRINTING THE MAP FOR THE CURRENT YEAR
  
  # Creating data frame from spatial join
  df <- data.frame(seq(1, nbForcingCells),vecYearSum)
  colnames(df)<- c("ID_ORCD","FORC")
     
  # Joining...
  spatialJoin <- inner_join(df,forcingShp, by = "ID_ORCD")
     
  # Aesthetic settings
  myGraphOptions <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (25), hjust = 0.5),
                         plot.subtitle = element_text(hjust = 0.5),
                         legend.title = element_text(colour = "black",  face = "bold.italic", family = "Helvetica"),
                         legend.text = element_text(face = "italic", colour="black",family = "Helvetica"),
                         legend.position="right", legend.direction="vertical",
                         axis.title = element_text(family = "Helvetica", size = (16), colour = "black"),
                         axis.text.y = element_text(family = "Helvetica", colour = "black", size = (13), angle = 90, hjust = 0.5),
                         axis.text.x = element_text(family = "Helvetica", colour = "black", angle = 0, hjust = 0.5, size = (13)),
                         plot.margin = unit(c(1,1,1,1), "cm"))
     
  # Plotting
  print('Rendering plot for entire period...')
     
  figTitle <- paste(varName,y,y+1,sep="_")
     
  pngTitle <- paste(figTitle,".png", sep="")
  png(file = pngTitle, width = 1920, height = 1080, units = "px")
     
  pl <- ggplot(spatialJoin) +
    myGraphOptions +
    # ggtitle(label = figTitle,  subtitle = "") +
    ggtitle(label = figTitle) +
    geom_sf(data = spatialJoin, aes(fill=FORC, geometry = geometry), color=NA) +
    scale_fill_viridis_c(option = "viridis", limits = c(0, 1000),direction = -1) +
    theme(legend.key.size = unit(10, 'cm')) + theme(legend.key.height= unit(4, 'cm')) + theme(legend.key.width= unit(2, 'cm')) + 
    theme(legend.title = element_text(size=25)) +   theme(legend.text = element_text(size=15))
   
  print(pl)
  dev.off()
  
  vecMean <- vecMean + vecYearSum
}
   
vecMean <- vecMean / nbYears
  
# --------------------------- PRINTING THE MAP FOR THE INTERANNUAL VALUES

# Creating data frame from spatial join
df <- data.frame(seq(1, nbForcingCells),vecMean)
colnames(df)<- c("ID_ORCD","FORC")

# Joining...
spatialJoin <- inner_join(df,forcingShp, by = "ID_ORCD")

# Aesthetic settings
myGraphOptions <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (25), hjust = 0.5),
                        plot.subtitle = element_text(hjust = 0.5),
                        legend.title = element_text(colour = "black",  face = "bold.italic", family = "Helvetica"),
                        legend.text = element_text(face = "italic", colour="black",family = "Helvetica"),
                        legend.position="right", legend.direction="vertical",
                        axis.title = element_text(family = "Helvetica", size = (16), colour = "black"),
                        axis.text.y = element_text(family = "Helvetica", colour = "black", size = (13), angle = 90, hjust = 0.5),
                        axis.text.x = element_text(family = "Helvetica", colour = "black", angle = 0, hjust = 0.5, size = (13)),
                        plot.margin = unit(c(1,1,1,1), "cm"))

# Plotting
print(paste('Rendering plot for : ',forcingBinFile, sep=""))

figTitle <- paste(varName,yearStart,yearEnd,sep="_")

pngTitle <- paste(figTitle,".png", sep="")
png(file = pngTitle, width = 1920, height = 1080, units = "px")

pl <- ggplot(spatialJoin) +
  myGraphOptions +
  # ggtitle(label = figTitle,  subtitle = "") +
  ggtitle(label = figTitle) +
  geom_sf(data = spatialJoin, aes(fill=FORC, geometry = geometry), color=NA) +
  scale_fill_viridis_c(option = "viridis", limits = c(0, 600),direction = -1) +
  theme(legend.key.size = unit(10, 'cm')) + theme(legend.key.height= unit(4, 'cm')) + theme(legend.key.width= unit(2, 'cm')) + 
  theme(legend.title = element_text(size=25)) +   theme(legend.text = element_text(size=15))

print(pl)
dev.off()

print("Done.")
