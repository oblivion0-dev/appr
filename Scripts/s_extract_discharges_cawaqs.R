# Last revision : jeu. 04 févr. 2021 13:34:30 CET  

# To do (minor things) :
# - Improve f_CustomXticsDatePlot function customization() so we can set an fixed interval or 
#   set the starting day   


workingDirectory = '/home/ngallois/Programmes/appr/' 
setwd(workingDirectory)

# -------------------------------------------------------------------------------------------

# Loading needed packages
library(Metrics)    # For RMSE
library(hydroGOF)   # For KGE, NSE

# Loading packages from the 'tidyverse' core
library(ggplot2) 
library(scales)
library(dplyr)
library(stringr)

# Loading entire homemade R library
files <- list.files(paste(workingDirectory,'Lib', sep = ''), pattern = '^.*[Rr]$', include.dirs = FALSE, full.names = TRUE)
for (f in files) {
 source(f)
 print(paste('Loading',f))
}
 
# Constants definitions (HANDS OFF ! Unless you know what you're messing with.) ----------------------
 
# Total number of statistical criterias
nbCrit <- 7 

# Maximum value of GIS id river element
maxGISeleID <- 30000

# # USER INPUTS ----------------------------------------------------------------------------------
 
# Type of CaWaQS-output file what are being read 
outputType <- "HYD_Q"
 
# Starting year of simulation
yearStart <- 1970
  
# Ending year of simulation
yearEnd <- 2020
  
# Absolute path to the gauging stations attributes text file
caracStations <- '/home/ngallois/Programmes/appr/Data/liste_stations.txt'
f_FileInformation('station_info') # To remind the user the expected structur of the 'caracStations' file
   
# Absolute path to the folder containing observation files
obsFolder <- '/home/ngallois/Programmes/appr/Data/DEBITS_DATA_2020_NAT'
 
# Absolute path to the folder containing the output binary files
simFolder <- '/home/ngallois/Programmes/appr/Data/OUTPUTS_JURASSIQUE_2021_COUPLED_CALIBSOUT_44_1970-2020_CAWAQS_292/RUN_CAL_1/Output_HYD'
 
# Absolute path to the HYD CaWaQS correspondance file
correspFile <- '/home/ngallois/Programmes/appr/Data/HYD_corresp_file.txt'

# CaWaQS version
versionID <- 2.92
 
# Model name
modelName <- 'Seine-8C'
 
# Simulation name
simname <- 'SAFRAN-1970-2020'
 
# Simulation start date (in 'aaaa-mm-dd' format)
dateStart <- '1970-08-01' 
  
# Simulation duration (in days)
nbDays <- 18263 
  
# Starting date for criteria calculations
statStart <- 5845 # 01/08/1986  # (1 = starting day of simulation)
  
# Ending date for criteria calculation (by default, simulation ending date)
statEnd <- nbDays 
  
# Starting date of graphs (1 = first day of simulation)
startGraph <- 13150  # 01/08/2006
  
# Ending date of graphs (by default, until the last day of simulation)
endGraph <- nbDays 
  
# CaWaQS date offfset (correspond to the CaWaQS starting simulation date - 1) (Reference day 0 = 01/01/1850)
dateCawOffset <- 44040    # 1970-07-31
 
# Calculating performance criteria (Yes = 1, No = 0)
onOffCriteria <- 1
  
# END OF USER CORNER --------------------------------------------------------------------------------------
  
# Pdf output filename
outputPdfFile <- paste('output_stations_',simname,'.pdf',sep='')
 
# Setting the number of binary records
nbRecBinFile <- f_setNbRecOutputs(outputType)
 
# Loading stations properties
properties <- read.table(caracStations, header = FALSE, na.strings = 'NA')
nbStations <- length(properties[,1])
print(paste('Informations read for',nbStations,'gauging stations.',sep=' '))
 
# Storage of IDs corresp. table
correspTable <- read.table(correspFile, header = TRUE, na.strings = 'NA')

# Linking ID_GIS -> ID_ABS
link <- matrix(data = NA, nrow = maxGISeleID, ncol = 2)
for (i in (1:maxGISeleID))
{
  link[correspTable[i,3],1] <- correspTable[i,3] # GIS ID
  link[correspTable[i,3],2] <- correspTable[i,2] # ABS ID
}

# Opening pdf output file
pdf(outputPdfFile, height = 7, width = 10)
  
# (Ox) dates management
vecDate <- seq(as.Date(dateStart), as.Date(dateStart)+nbDays-1, by = 'day')
vecX <- seq(1, nbDays)
  
# Various initializations
matStat <- matrix(data = NA, nrow = nbStations, ncol = nbCrit)
matData <- matrix(data = NA, nrow = nbDays, ncol = 2*nbStations+1)
totalDayCounter <- 0
  
# Loading date values in main data matrix
matData[,1] <- vecX
  
# Custom label management of OX axis (xtics is a list of 2 vectors : breakpoints and associated labels)
xtics <- f_CustomXticsDatePlot(vecX,vecDate,yearStart) # return breakpoints and labels every august 1st for now
breakpoints <- as.vector(unlist(xtics['breaks']))
xticsLabels <- as.Date(unlist(xtics['labels']))

# So we can only print out years as xtics...
shortLabels <- c()
for (i in (1:length(xticsLabels)))
{
 shortLabels[i] <- str_sub(xticsLabels[i], 1, 4)
}

# ----------------------------------------------------

# Observation data storage loop
for (i in (1:nbStations))
{
    fileObs <- paste(obsFolder,'/',properties[i,1],'.dat',sep = '')
    exist <- f_isFileReachable(fileObs, 0, 0)
  
    if (exist == 0)
    {
      print(paste('Observation data storage in progress for file :',i,'out of',nbStations,':',fileObs))
      fObs <- read.table(fileObs, h = FALSE)
      
      for (k in (1:length(fObs[,1]))) 
      {
        if (fObs[k,3]-dateCawOffset >= 1 && fObs[k,3] - dateCawOffset <= nbDays)
        {
          matData[fObs[k,3] - dateCawOffset,2*i] <- fObs[k,4]*1.e-3  # Data storage in matrix over the simulation period only, in m3/s !
        }
      }
    }
}  
   
# Simulated data storage loop
for (y in (yearStart:(yearEnd-1)))
{
  fileSim <- paste(simFolder,'/',outputType,'.',y,y+1,'.bin',sep='')
  f_isFileReachable(fileSim, 0, 1)
   
  nbDays <- 365
  if ((y+1)%%4 == 0) nbDays <- 366
      
  binfile = file(fileSim, 'rb')
  print(paste('Reading for binary file :',fileSim,'in progress...'),sep=' ')
      
  for (d in (1:nbDays))
  {
    totalDayCounter = totalDayCounter + 1
 
    for (r in (1:nbRecBinFile))
    {
      nbHydCells = readBin(binfile, integer(), size = 4, endian = 'little')
      recValues = readBin(binfile, double(), n = nbHydCells, size = 8, endian = 'little')
      nbHydCells = readBin(binfile, integer(), size = 4, endian = 'little')
        
      for (p in (1:nbStations))
        matData[totalDayCounter,2*p+1] <- recValues[link[properties[p,3],2]]  # Searching the absolute ID position in the binary record (link finds the correct ABS ID)
       
    }
  }
  close(binfile)
  print(paste('Reading for binary file :',fileSim,'done. File closed.'))
}
  
#Plotting loop 
for (i in (1:nbStations))
{
   if (onOffCriteria == 1)
   {
      # Performance calculations
      statAtt <- f_StatisticCriterias(matData[,2*i],matData[,2*i+1],'discharge',statStart,statEnd)  # Still need to set a minimal threshold on number of observation values
       
      # Graph criteria labeling
      statLabel <-paste('n =',statAtt['n'],' - Mean obs. Q = ',signif(statAtt['mobs'],1),'m3/s - Mean sim. Q = ',signif(statAtt['msim'],1),
                       'm3/s \n Nash = ',signif(statAtt['nash'],3),' - KGE = ',signif(statAtt['kge'],3),'Cpearson = ',signif(statAtt['cpearson'],3))

      # Criteria storage
      matStat[i,1] <- statAtt['n']
      matStat[i,2] <- statAtt['mobs']
      matStat[i,3] <- statAtt['msim']
      matStat[i,4] <- statAtt['nash']
      matStat[i,5] <- statAtt['lnNash']
      matStat[i,6] <- statAtt['kge']
      matStat[i,7] <- statAtt['cpearson']

    }
      
   # Setting title
   figTitle <- paste(properties[i,1],' : ',properties[i,2],' \nObs status : ',properties[i,5],' - Catchment area (km2) : ',properties[i,6])
       
   # Plotting
   dataFrame <- as.data.frame(matData)    
      
    pl =  ggplot(dataFrame) +
          geom_line(aes(x = dataFrame[,1], y = dataFrame[,2*i+1], color = 'Simulated discharge'), size = 0.4, alpha = 0.8)  +
          geom_point(aes(x = dataFrame[,1], y = dataFrame[,2*i], color = 'Measured discharge'), size = 0.6, alpha = 0.3) +
          ggtitle(label = figTitle) +   # subtitle = statLabel
          labs(x = 'Time (markers at August, 1st)', y = 'Discharge (m3/s)', 
          caption = paste('CaWaQS',versionID,' - ',modelName,' application - Simulation : ',simname,sep=''))
    
    # Aesthetic settings
    myGraphOptions <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15), hjust = 0.5),
                      plot.subtitle = element_text(hjust = 0.5),
                      legend.title = element_text(colour = "black",  face = "bold.italic", family = "Helvetica"),
                      legend.text = element_text(face = "italic", colour="black",family = "Helvetica"),
                      legend.position="top", legend.direction="horizontal",
                      axis.title = element_text(family = "Helvetica", size = (13), colour = "black"),
                      axis.text.y = element_text(family = "Helvetica", colour = "black", size = (13)),
                      axis.text.x = element_text(family = "Helvetica", colour = "black", angle = 45, hjust = 1, size = (11)),
                      plot.margin = unit(c(1,1,1,1), "cm"))
      
    # Printing graphs out...
    if (onOffCriteria == 1)
    {
      print(paste("Plotting : ",i,'out of',nbStations," - Main statistics values : ",statLabel))
      
      print(pl + myGraphOptions + scale_color_manual(values=c("red", "blue")) +
            labs(color = "Color code : ") + scale_x_continuous(limits=c(startGraph, endGraph), 
							  breaks=breakpoints,
                              labels=shortLabels) + 
           					  ggtitle(label = figTitle, subtitle = statLabel))
    }
    else 
    {
      print(paste("Plotting : ",i,'out of',nbStations))
      
      print(pl + myGraphOptions + scale_color_manual(values=c("red", "blue")) +
            labs(color = "Color code : ") + scale_x_continuous(limits=c(startGraph, endGraph), 
							  breaks=breakpoints,
                              labels=shortLabels) + 
           					  ggtitle(label = figTitle))
    }
 }

# Closing pdf
dev.off()

# Creating unique data frame to write
df <- data.frame(properties[,1],matStat)
colnames(df)<- c("STAT","n","mobs","msim","nash","lnNash","kge","ccorr")
write.table(df, file = "statistics_stations.txt")

print(paste("Done. Output pdf file located in", workingDirectory,sep=''))
