# Clearing everything...
rm(list = ls())

workingDirectory  = 'E:/Github/appr/'
setwd(workingDirectory)

# -------------------------------------------------------------------------------------------

# Loading needed packages
library(Metrics)    # For RMSE
library(hydroGOF)   # For KGE

# Loading packages from the 'tidyverse' core
library(ggplot2) 
library(scales)
library(dplyr)
library(stringr)
library(gridExtra)

# Loading entire homemade R library
files <- list.files(paste(workingDirectory,'Lib', sep = ''), pattern = '^.*[Rr]$', include.dirs = FALSE, full.names = TRUE)
for (f in files) {
 source(f)
 print(paste('Loading',f))
}

# USER INPUTS -----------------------------------------------------------------------------------------

# Scenario name
scenarioName <- "R1.15"

# Simulation name
simname1 <- 'SC_1910_R115_STOP'
simname2 <- 'SC_1910_R115_POMP'
simname3 <- 'SC_1910_R115_TOUS'

# Type of CaWaQS-output file what are being read 
outputType <- "AQ_H"

# Starting year of simulation
yearStart <- 1909

# Ending year of simulation
yearEnd <- 1910

# Number of layers of the application
nbLayers <- 2

# Layers names (in order, from top to bottom)
layerNames <- c("1-PPC","2-Craie")

# Absolute path to the piezometer attributes text file
caracPiezo <- paste(workingDirectory,'Data/liste_piezo_PPC.txt',sep="")

# Absolute path of the MNT elevation
MNTFile <- paste(workingDirectory,'Data/COTE_MNT.txt',sep="")

f_FileInformation('piezo_info') # To remind the user the expected structur of the 'caracPiezo' file

# Absolute path to the folder containing the output binary files
simFolder1 <- paste(workingDirectory,'Data/',simname1,sep ="")
simFolder2 <- paste(workingDirectory,'Data/',simname2,sep ="")
simFolder3 <- paste(workingDirectory,'Data/',simname3,sep ="")

# CaWaQS version
versionID <- 2.93

# Model name
modelName <- 'Appl.PPC'

# Simulation start date (in 'aaaa-mm-dd' format)
dateStart <- '1909-08-01' 

# Simulation duration (in days)
nbDays <- 365 

# Starting date of graphs (1 = first day of simulation)
startGraph <- 100

# Ending date of graphs (by default, until the last day of simulation)
endGraph <- 290 

# CaWaQS date offfset (correspond to the CaWaQS starting simulation date - 1) (Reference day 0 = 01/01/1850)
dateCawOffset <- 21760    # 1909-07-31

# Plotting mean simulated levels vs. observed level (Yes = 1, No = Anything else)
plotMeanLevels <- 0

# Minimun mean value of the hydraulic head over the domain (in mNGF)
piezoMin <- 0

# maximum mean value of the hydraulic head over the domain (in mNGF)
piezoMax <- 60

# END OF USER CORNER --------------------------------------------------------------------------------------
 
# Pdf output filename
outputPdfFile <- paste('output_piezo_',scenarioName,'.pdf',sep='')

# Setting the number of binary records
nbRecBinFile <- f_setNbRecOutputs(outputType)

# Loading piezometers properties
properties <- read.table(caracPiezo, header = FALSE, na.strings = 'NA')
nbPiezo <- length(properties[,1])

# Storage of the MNT elevation
MNT <- read.table(MNTFile, header = TRUE, na.strings = 'NA')

print(paste('Informations read for',nbPiezo,'piezometers.',sep=' '))

# Opening pdf output file
pdf(outputPdfFile, height = 7, width = 10)
 
# (Ox) dates management
vecX <- seq(1, nbDays)
vecDate <- seq(as.Date(dateStart), as.Date(dateStart)+nbDays-1, by = 'day')

# Various initializations
matData1 <- matrix(data = NA, nrow = nbDays, ncol = 2*nbPiezo+1)
matData2 <- matrix(data = NA, nrow = nbDays, ncol = 2*nbPiezo+1)
matData3 <- matrix(data = NA, nrow = nbDays, ncol = 2*nbPiezo+1)

totalDayCounter <- 0
 
# Loading date values in main data matrix
matData1[,1] <- vecX
matData2[,1] <- vecX
matData3[,1] <- vecX

# Custom label management of OX axis (xtics is a list of 2 vectors : breakpoints and associated labels)
# xtics <- f_CustomXticsDatePlot(vecX,vecDate,yearStart) # return breakpoints and labels every august 1st for now
# 
# breakpoints <- as.vector(unlist(xtics['breaks']))
# xticsLabels <- as.Date(unlist(xtics['labels']))
# 
# # So we can only print out years as xtics...
# shortLabels <- c()
# for (i in (1:length(xticsLabels)))
# {
#  shortLabels[i] <- str_sub(xticsLabels[i], 1, 4)
# }

# -------------------------------------------------------------------------------------

# Simulated data storage loop
for (y in (yearStart:(yearEnd-1)))
{
 fileSim1 <- paste(simFolder1,'/',outputType,'.',y,y+1,'.bin',sep='')
 fileSim2 <- paste(simFolder2,'/',outputType,'.',y,y+1,'.bin',sep='')
 fileSim3 <- paste(simFolder3,'/',outputType,'.',y,y+1,'.bin',sep='')
 
 f_isFileReachable(fileSim1, 0, 1)
 f_isFileReachable(fileSim2, 0, 1)
 f_isFileReachable(fileSim3, 0, 1)
 
 nbDays <- 365
 if ((y+1)%%4 == 0) nbDays <- 366
     
 binfile1 = file(fileSim1, 'rb')
 binfile2 = file(fileSim2, 'rb')
 binfile3 = file(fileSim3, 'rb')
 
 print(paste('Reading for binary files in progress...'),sep=' ')
     
 for (d in (1:nbDays))
 {
   totalDayCounter = totalDayCounter + 1

   for (r in (1:nbRecBinFile))
   {
     nbAqCells = readBin(binfile1, integer(), size = 4, endian = 'little')
     recValues1 = readBin(binfile1, double(), n = nbAqCells, size = 8, endian = 'little')
     nbAqCells = readBin(binfile1, integer(), size = 4, endian = 'little')
     
     nbAqCells = readBin(binfile2, integer(), size = 4, endian = 'little')
     recValues2 = readBin(binfile2, double(), n = nbAqCells, size = 8, endian = 'little')
     nbAqCells = readBin(binfile2, integer(), size = 4, endian = 'little')
       
     nbAqCells = readBin(binfile3, integer(), size = 4, endian = 'little')
     recValues3 = readBin(binfile3, double(), n = nbAqCells, size = 8, endian = 'little')
     nbAqCells = readBin(binfile3, integer(), size = 4, endian = 'little')
     
     if (r == 1) # 1 = Hydraulic Head
     {
       for (p in (1:nbPiezo)) 
       {
         matData1[totalDayCounter,2*p+1] <- recValues1[properties[p,9]]  # Searching the absolute ID position in the binary record
         matData2[totalDayCounter,2*p+1] <- recValues2[properties[p,9]]
         matData3[totalDayCounter,2*p+1] <- recValues3[properties[p,9]]
         }        
       } 
     }
   }
   close(binfile1)
   close(binfile2)  
   close(binfile3)
   print(paste('Reading for binary files done. Files closed.'))
}
 
# Plotting loop 
for (i in (1:nbPiezo))
{
   matMNT <- matrix(data = NA, nrow = nbDays, ncol = 4)
   
   matMNT[,1] <- vecX
   matMNT[,2] <- MNT[properties[i,9],2]
   matMNT[,3] <- MNT[properties[i,9],3]
   matMNT[,4] <- MNT[properties[i,9],4]
      
  # Setting title
  figTitle <- paste("Scenario :",scenarioName,"-",properties[i,4],sep=" ")
      
  # Plotting
  dataFrame1 <- as.data.frame(matData1)    
  dataFrame2 <- as.data.frame(matData2)    
  dataFrame3 <- as.data.frame(matData3)    
  dataFrameMNT <- as.data.frame(matMNT)
  
   pl =  ggplot(dataFrame1) +
         geom_line(aes(x = dataFrame1[,1], y = dataFrame1[,2*i+1], color = '(1) Arrêt ZINI'), size = 0.7, alpha = 0.9)  +
         geom_line(aes(x = dataFrame2[,1], y = dataFrame2[,2*i+1], color = '(2) Pompages persistants'), size = 0.4, alpha = 0.5)  +
         geom_line(aes(x = dataFrame3[,1], y = dataFrame3[,2*i+1], color = '(3) Arrêt global des pompages'), size = 0.4, alpha = 0.5)  +
         geom_line(aes(x = dataFrameMNT[,1], y = dataFrameMNT[,2], color = '(4) MNT - Cote min.'), size = 0.4, alpha = 0.5, linetype = "dashed")  +
         geom_line(aes(x = dataFrameMNT[,1], y = dataFrameMNT[,3], color = '(5) MNT - Cote max.'), size = 0.4, alpha = 0.5, linetype = "dashed")  +
         geom_line(aes(x = dataFrameMNT[,1], y = dataFrameMNT[,4], color = '(6) MNT - Cote moyenne'), size = 0.8, alpha = 0.8)  +
         geom_label(label=paste(signif(dataFrameMNT[,2],3),"mNGF",sep=" "), x=285,y=dataFrameMNT[,2],label.padding = unit(0.30, "lines"), label.size = 0.25, color = "black", fill="white") +
         geom_label(label=paste(signif(dataFrameMNT[,3],3),"mNGF",sep=" "), x=285,y=dataFrameMNT[,3],label.padding = unit(0.30, "lines"), label.size = 0.25, color = "black", fill="white") +
         geom_label(label=paste(signif(dataFrameMNT[,4],3),"mNGF",sep=" "), x=285,y=dataFrameMNT[,4],label.padding = unit(0.30, "lines"), label.size = 0.25, color = "black", fill="white") +
      
         ggtitle(label = figTitle) + 
         labs(x = 'Temps (jours)', y = 'Charge hydraulique (mNGF)', 
         caption = paste('CaWaQS',versionID,' - ',modelName,sep=''))

   # Aesthetic settings
   myGraphOptions <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15), hjust = 0.5),
                     plot.subtitle = element_text(hjust = 0.5, size = (13)),
                     legend.title = element_text(colour = "black",  face = "bold.italic", family = "Helvetica"),
                     legend.text = element_text(face = "italic", colour="black",family = "Helvetica"),
                     legend.position="top", legend.direction="horizontal",
                     axis.title = element_text(family = "Helvetica", size = (13), colour = "black"),
                     axis.text.y = element_text(family = "Helvetica", colour = "black", size = (13)),
                     axis.text.x = element_text(family = "Helvetica", colour = "black", angle = 45, hjust = 1, size = (9)),
                     plot.margin = unit(c(1,1,1,1), "cm"))
     
   # Printing graphs out...
   print(paste("Plotting : ",i,'out of',nbPiezo))
     
   print(pl + myGraphOptions + scale_color_manual(values=c("blue", "red","green4","grey10","grey10","black")) +
         labs(color = "Code couleur : ") +
         scale_x_continuous(limits=c(startGraph, endGraph),breaks=c(154,161,168,175,182,189,196,203,210,217,224,231,238),
                            labels=c("01/01/1910","08/01/1910","15/01/1910","22/01/1910","29/01/1910","05/02/1910","12/02/1910",
                                     "19/02/1910","26/02/1910","05/03/1910","12/03/1910","19/03/1910","26/03/1910")) +
         ggtitle(label = figTitle))
   
   remove(matMNT)
}

# Closing pdf
dev.off()

print(paste("Done. Output pdf file located in", workingDirectory,sep=''))
