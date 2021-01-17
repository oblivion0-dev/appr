# Last revision : 17-01-2020 : 12:58:43

# To do :
# - Needs to write the txt file of statistic criterias 
# - Rework the date management on the Ox axis
# - Add more criterias (Rstd, Ccorr, Mean bias)
# - Criteria calculations NEEDS TO BE VERIFIED !

workingDirectory = '/home/ngallois/Programmes/appr/' 
setwd(workingDirectory)

# -------------------------------------------------------------------------------------------

# Loading needed packages
library(Metrics)    # For RMSE
library(hydroGOF)   # For KGE

# Loading packages from the 'tidyverse' core
library(ggplot2) 
library(scales)
library(dplyr)

# Loading entire homemade R library
files <- list.files(paste(workingDirectory,'Lib', sep = ''), pattern = '^.*[Rr]$', include.dirs = FALSE, full.names = TRUE)
for (f in files) {
 source(f)
 print(paste('Loading',f))
}
 
# Constants definitions (HANDS OFF ! Unless you know what you're messing with.) ----------------------

# Total number of statistical criterias
nbCrit <- 5 

# USER INPUTS ----------------------------------------------------------------------------------

# Type of CaWaQS-output file what are being read 
outputType <- "AQ_MB"

# Starting year of simulation
yearStart <- 1970
 
# Ending year of simulation
yearEnd <- 2020
 
# Absolute path to the piezometer attributes text file
caracPiezo <- '/home/ngallois/Programmes/appr/Data/liste_piezo_export.txt'
f_FileInformation('piezo_info') # To remind the user the expected structur of the 'caracPiezo' file
  
# Absolute path to the folder containing observation files
obsFolder <- '/home/ngallois/Programmes/appr/Data/H_donnees_shr_2020'

# Absolute path to the folder containing the output binary files
simFolder <- '/home/ngallois/Programmes/appr/Data'

# CaWaQS version
versionID <- 2.91

# Model name
modelName <- 'Seine-8C'

# Simulation name
simname <- 'SAFRAN_1970-2020'

# Simulation start date (in 'aaaa-mm-dd' format)
dateStart <- '1970-08-01' 
 
# Simulation duration (in days)
nbDays <- 18263 
 
# Starting date for criteria calculations
statStart <- 10746 # 01/01/2000  # (1 = starting day of simulation)
 
# Ending date for criteria calculation (by default, simulation ending date)
statEnd <- nbDays 
 
# Starting date of graphs (1 = first day of simulation)
startGraph <- 1 
 
# Ending date of graphs (by default, until the last day of simulation)
endGraph <- nbDays 
 
# CaWaQS date offfset (correspond to the CaWaQS starting simulation date - 1) (Reference day 0 = 01/01/1850)
dateCawOffset <- 44040    # 1970-07-31

# Calculating performance criteria (Yes = 1, No = 0)
onOffCriteria <- 1
 
# END OF USER CORNER --------------------------------------------------------------------------------------
 
# Pdf output filename
outputPdfFile <- paste('output_piezo_',simname,'.pdf',sep='')

# Setting the number of binary records
nbRecBinFile <- f_setNbRecOutputs(outputType)

# Loading piezometers properties
properties <- read.table(caracPiezo, header = FALSE, na.strings = 'NA')
nbPiezo <- length(properties[,1])
print(paste('Informations read for',nbPiezo,'piezometers.',sep=' '))

# Opening pdf output file
pdf(outputPdfFile, height = 7, width = 10)
 
# (Ox) dates management
vecDate <- seq(as.Date(dateStart), as.Date(dateStart)+nbDays-1, by = 'day')
vecX <- seq(1, nbDays)

# Various initializations
matStat <- matrix(data = NA, nrow = nbPiezo, ncol = nbCrit)
matData <- matrix(data = NA, nrow = nbDays, ncol = 2*nbPiezo+1)
totalDayCounter <- 0
 
# Loading date values in main data matrix
matData[,1] <- vecX
 
# Observation data storage loop
for (i in (1:nbPiezo))
{
   fileObs <- paste(obsFolder,'/',properties[i,3],'.dat',sep = '')
   exist <- f_isFileReachable(fileObs, 0, 0)
 
   if (exist == 0)
   {
     print(paste('Observation data storage in progress for file :',i,'out of',nbPiezo,':',fileObs))
     fObs <- read.table(fileObs, h = FALSE)
     
     for (k in (1:length(fObs[,1]))) 
     {
       if (fObs[k,4]-dateCawOffset >= 1 && fObs[k,4] - dateCawOffset <= nbDays)
       {
         matData[fObs[k,4] - dateCawOffset,2*i] <- fObs[k,5]  # Data storage in matrix over the simulation period only !
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
     nbAqCells = readBin(binfile, integer(), size = 4, endian = 'little')
     recValues = readBin(binfile, double(), n = nbAqCells, size = 8, endian = 'little')
     nbAqCells = readBin(binfile, integer(), size = 4, endian = 'little')
       
     if (r == 1) # 1 = Hydraulic Head
     {
       for (p in (1:nbPiezo))
       {
         matData[totalDayCounter,2*p+1] <- recValues[properties[p,9]]  # Searching the absolute ID position in the binary record
         }        
       } 
     }
   }
   close(binfile)
   print(paste('Reading for binary file :',fileSim,'done. File closed.'))
}
 
# Plotting loop 
for (i in (1:nbPiezo))
{
  if (onOffCriteria == 1)
  {
     # Performance calculations
     statAtt <- f_StatisticCriterias(matData[,2*i],matData[,2*i+1],'piezo',statStart,statEnd)  # Still need to set a minimal threshold on number of observation values
      
     # Graph criteria labeling
     statLabel <-paste('n =',statAtt['n'],' - Mean obs. level = ',signif(statAtt['mobs'],3),'m - Mean sim. level = ',signif(statAtt['msim'],3),
                      'm \n RMSE = ',signif(statAtt['rmse'],3),'m - KGE = ',signif(statAtt['kge'],3))
   
     # Criteria storage
     matStat[i,1] <- statAtt['n']
     matStat[i,2] <- statAtt['mobs']
     matStat[i,3] <- statAtt['msim']
     matStat[i,4] <- statAtt['rmse']
     matStat[i,5] <- statAtt['kge']
   }
     
  # Setting title
  figTitle <- paste('Piezometer',properties[i,2],' : ',properties[i,4],' - Layer : ',properties[i,5])
      
  # Plotting
  dataFrame <- as.data.frame(matData)    
     
   pl =  ggplot(dataFrame) +
         geom_line(aes(x = dataFrame[,1], y = dataFrame[,2*i+1], color = 'Simulated head'), size = 0.4, alpha = 0.8)  +
         geom_point(aes(x = dataFrame[,1], y = dataFrame[,2*i], color = 'Measured head'), size = 0.6, alpha = 0.3) +
         ggtitle(label = figTitle) +   # subtitle = statLabel
         labs(x = 'Time (days)', y = 'Hydraulic head (mNGF)', 
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
     print(paste("Plotting : ",i,'out of',nbPiezo," - Main statistics values : ",statLabel))
     
     print(pl + myGraphOptions + scale_color_manual(values=c("red", "blue")) +
           labs(color = "Color code : ") +
           scale_x_continuous(limits=c(startGraph, endGraph)) + ggtitle(label = figTitle, subtitle = statLabel))
   }
   else 
   {
     print(paste("Plotting : ",i,'out of',nbPiezo))
     
     print(pl + myGraphOptions + scale_color_manual(values=c("red", "blue")) +
           labs(color = "Color code : ") +
           scale_x_continuous(limits=c(startGraph, endGraph)) + ggtitle(label = figTitle))                    
   }
}
 
# Signing off
dev.off()
 
print(paste("Done. Output pdf file located in", workingDirectory,sep=''))