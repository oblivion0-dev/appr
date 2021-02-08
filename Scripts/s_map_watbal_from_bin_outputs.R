# Last revision : Monday, February, 8th, 13:58:21 (Script is at version 0, right now)

# Scripts mapping annual cumulative values (in mm) of a given WATBAL output term
# and also the inter-annual values over the entire simulation period

# Improvements needed : WARNING ! FOR NOW, ONLY WORKS FOR WATBAL OUTPUTS AT THE WATBAL ELEMENT SCALE, NOT THE CPROD SCALE !!!!!
# Need to improve the genericity of the path settings 

# -------------------------------------------------------------------------------------------

# Setting working directory
workingDirectory  = 'E:/Github/appr/'                     # For Windows
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

# Path of the text file storing the surface (in m2) of surface element
SurfFile <- './Data/Elebu_surfaces.txt'

# Number of WATBAL surface elements
nbSurfaceCells <- 10829

# Generic output variable name which is plotter
varName <- "Infiltration"

# CaWaQS output type file
outputType <- "WATBAL_MB"

# Binary record ID of the output variable which is plotted (Runoff =3, Infiltration = 4). (See CaWaQS manual)
idRecValue <- 4

# Calculation time-step duration in seconds
tsDuration <- 86400.

# -----------------------------------------------------------------------------------------------

nbYears <- yearEnd-yearStart+1

nbRec <- f_setNbRecOutputs(outputType)
print(paste("Number of expected time-step records for the",outputType,"output files :",nbRec,sep= " "))

# Loading the surface grid's shapefile
SurfShp <- st_read("./Data/ELE_BU.shp", stringsAsFactors = FALSE)

# Storage of the grid cells surface, in order to turn m3/s outputs in mm/day
f_isFileReachable(SurfFile, 0, 1)
SurfTable <- read.table(SurfFile, header = TRUE, na.strings = 'NA')

# Outputs reading loop for all yearly files

# Vector initialization of mean values calculated over pluriannual simulation time period
vecMean <- rep(0,nbSurfaceCells)
    
for (y in (yearStart:(yearEnd-1)))
{
  # Opening yearly binary file
  outputBinFile <- paste(workingDirectory,'Data/WATBAL_MB.',y,y+1,'.bin',sep="")
     
  # Existence test, stops the program if unreachable
  f_isFileReachable(outputBinFile, 0, 1) 
      
  nbDays <- 365
  if ((y+1)%%4 == 0) nbDays <- 366
         
   binfile = file(outputBinFile, 'rb')
   print(paste('Reading for binary file :',outputBinFile,' in progress...'))
        
   # Initialization of the vector of cumulative yearly values (in mm/year)
   vecYearSum <- rep(0,nbSurfaceCells)
     
   for (d in (1:nbDays))
   {
     for (r in (1:nbRec))
     {
        dump = readBin(binfile, integer(), size = 4, endian = 'little')
        recValues = readBin(binfile, double(), n = nbSurfaceCells, size = 8, endian = 'little')
        dump = readBin(binfile, integer(), size = 4, endian = 'little')
     
        if (r == idRecValue) 
        {
          vecYearSum <- vecYearSum + ((recValues*tsDuration)/(SurfTable[,2]))*1000. # Transformation into mm/time-step
        } 
     }
   }
   close(binfile)
   print(paste('Reading for binary file :',outputBinFile,'done. File closed.'))
   
   # --------------------------- PRINTING THE MAP FOR THE CURRENT YEAR
   
   # Creating data frame from spatial join
   df <- data.frame(seq(1, nbSurfaceCells),vecYearSum)
   colnames(df)<- c("ID_ele_BU","Output")
      
   # Joining...
   spatialJoin <- inner_join(df,SurfShp, by = "ID_ele_BU")
      
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
   print(paste('Rendering plot for : ',outputBinFile, sep=""))
         
   figTitle <- paste(varName,y,y+1,sep="_")
      
   pngTitle <- paste(figTitle,".png", sep="")
   png(file = pngTitle, width = 1920, height = 1080, units = "px")
      
   pl <- ggplot(spatialJoin) +
     myGraphOptions +
     # ggtitle(label = figTitle,  subtitle = "") +
     ggtitle(label = figTitle) +
     geom_sf(data = spatialJoin, aes(fill=Output, geometry = geometry), color=NA) +
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
  df <- data.frame(seq(1, nbSurfaceCells),vecMean)
  colnames(df)<- c("ID_ele_BU","Output")
 
 # Joining...
 spatialJoin <- inner_join(df,SurfShp, by = "ID_ele_BU")
 
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
 print('Rendering plot for entire period.')
 
 figTitle <- paste(varName,yearStart,yearEnd,sep="_")
 
 pngTitle <- paste(figTitle,".png", sep="")
 png(file = pngTitle, width = 1920, height = 1080, units = "px")
 
 pl <- ggplot(spatialJoin) +
   myGraphOptions +
   # ggtitle(label = figTitle,  subtitle = "") +
   ggtitle(label = figTitle) +
   geom_sf(data = spatialJoin, aes(fill=Output, geometry = geometry), color=NA) +
   scale_fill_viridis_c(option = "viridis", limits = c(0, 600),direction = -1) +
   theme(legend.key.size = unit(10, 'cm')) + theme(legend.key.height= unit(4, 'cm')) + theme(legend.key.width= unit(2, 'cm')) + 
   theme(legend.title = element_text(size=25)) +   theme(legend.text = element_text(size=15))
 
 print(pl)
 dev.off()
 
print("Done.")
