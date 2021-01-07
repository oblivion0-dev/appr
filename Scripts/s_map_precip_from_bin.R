# Scripts mapping inter-annual values of rainfall (set for a batch of different simulations)

# Setting working directory
#workingDirectory  = 'E:/Github/appr/'                   # For Windows
workingDirectory = '/home/nicolas/GitHub/appr/'           # For Linux
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

# Starting year of MTO files set
yearStart <- 1970

# Ending year of MTO files set
yearEnd <- 2018

# Simulation/scenarios names text file
simNames <- paste(workingDirectory,'Data/liste_scenarios_Aquivar.txt', sep = '')

# Number of MTO cells over the model
nbMtoCells <- 1613

# -----------------------------------------------------------------------------------------------

nbYears <- yearEnd-yearStart+1
maxAllValues <- -9999

# Loading set of simNames
names <- read.table(simNames, header = FALSE, na.strings = 'NA')
nbSim <- length(names[,1])

# Loading the MTO grid's shapefile
mtoShp <- st_read("./Shapefiles/METEO.shp", stringsAsFactors = FALSE)
#str(mtoShp)

# MTO data reading loop for all files and all simulations
for (s in 1:nbSim)
{
  # Vector of mean values calculated over pluriannual length
  vecSceMean <- rep(0,nbMtoCells)
  
  for (y in (yearStart:(yearEnd-1)))
  {
    # Opening yearly binary file
    precipBinFile <- paste(workingDirectory,'Data/PRCP_BIN_CAWAQS_AQUIVAR/',names[s,1],'/prcp.',y,y+1,'.dir',sep="")
    
    # Existence test, stops the program if unreachable
    f_isFileReachable(precipBinFile, 0, 1) 
    
    nbDays <- 365
    if ((y+1)%%4 == 0) nbDays <- 366
       
    binfile = file(precipBinFile, 'rb')
    print(paste('Reading for binary file :',precipBinFile,' in progress...'))
      
    # Vector of cumulative rainfall values
    vecYearSum <- rep(0,nbMtoCells)
     
    for (d in (1:nbDays))
    {
      recValues = readBin(binfile, double(), n = nbMtoCells, size = 4, endian = 'little')   # MTO values read as floats in CaWaQS, not double !
      vecYearSum <- vecYearSum + recValues
    }
    
    close(binfile)
    print(paste('Reading for binary file :',precipBinFile,'done. File closed.'))
      
    vecSceMean <- vecSceMean + vecYearSum
  }
  
  vecSceMean <- vecSceMean / nbYears
  
  #print(min(vecSceMean))
  
  # Creating data frame from spatial join
  df <- data.frame(seq(1, nbMtoCells),vecSceMean)
  colnames(df)<- c("ID_SEINE","PRCP")
  
  # Joining...
  spatialJoin <- inner_join(df,mtoShp, by = "ID_SEINE")
  
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
  print(paste('Rendering plot for :',names[s,1], sep=""))
  
  pngTitle = paste("PRCP_SC_",names[s,1],sep="")
  png(file = pngTitle, width = 1920, height = 1080, units = "px")
  
  figTitle <- pngTitle
  
  pl <- ggplot(spatialJoin) +
    myGraphOptions +
    # ggtitle(label = figTitle,  subtitle = "") +
    ggtitle(label = figTitle) +
    geom_sf(data = spatialJoin, aes(fill=PRCP, geometry = geometry), color=NA) +
    scale_fill_viridis_c(option = "viridis", limits = c(500, 1400),direction = -1) +
    theme(legend.key.size = unit(10, 'cm')) + theme(legend.key.height= unit(4, 'cm')) + theme(legend.key.width= unit(2, 'cm')) + 
    theme(legend.title = element_text(size=25)) +   theme(legend.text = element_text(size=15))

  print(pl)
  dev.off()
  
}

print("Done.")
