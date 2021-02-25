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

# Simulation name
simname1 <- 'SC_1910_R060_STOP'
simname2 <- 'SC_1910_R070_STOP'
simname3 <- 'SC_1910_R080_STOP'
simname4 <- 'SC_1910_R090_STOP'
simname5 <- 'SC_1910_R100_STOP'
simname6 <- 'SC_1910_R115_STOP'

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

f_FileInformation('piezo_info') # To remind the user the expected structur of the 'caracPiezo' file

# Absolute path of the MNT elevation
MNTFile <- paste(workingDirectory,'Data/COTE_MNT.txt',sep="")

# Absolute path to the folder containing the output binary files
simFolder1 <- paste(workingDirectory,'Data/',simname1,sep ="")
simFolder2 <- paste(workingDirectory,'Data/',simname2,sep ="")
simFolder3 <- paste(workingDirectory,'Data/',simname3,sep ="")
simFolder4 <- paste(workingDirectory,'Data/',simname4,sep ="")
simFolder5 <- paste(workingDirectory,'Data/',simname5,sep ="")
simFolder6 <- paste(workingDirectory,'Data/',simname6,sep ="")

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
outputPdfFile <- paste('output_piezo.pdf',sep='')

# Setting the number of binary records
nbRecBinFile <- f_setNbRecOutputs(outputType)

# Loading piezometers properties
properties <- read.table(caracPiezo, header = FALSE, na.strings = 'NA')
nbPiezo <- length(properties[,1])

print(paste('Informations read for',nbPiezo,'piezometers.',sep=' '))

# Opening pdf output file
pdf(outputPdfFile, height = 7, width = 10)
 
# (Ox) dates management
vecX <- seq(1, nbDays)
vecDate <- seq(as.Date(dateStart), as.Date(dateStart)+nbDays-1, by = 'day')

# Storage of the MNT elevation
MNT <- read.table(MNTFile, header = TRUE, na.strings = 'NA')

# Various initializations
matData1 <- matrix(data = NA, nrow = nbDays, ncol = 2*nbPiezo+1)
matData2 <- matrix(data = NA, nrow = nbDays, ncol = 2*nbPiezo+1)
matData3 <- matrix(data = NA, nrow = nbDays, ncol = 2*nbPiezo+1)
matData4 <- matrix(data = NA, nrow = nbDays, ncol = 2*nbPiezo+1)
matData5 <- matrix(data = NA, nrow = nbDays, ncol = 2*nbPiezo+1)
matData6 <- matrix(data = NA, nrow = nbDays, ncol = 2*nbPiezo+1)

totalDayCounter <- 0
 
# Loading date values in main data matrix
matData1[,1] <- vecX
matData2[,1] <- vecX
matData3[,1] <- vecX
matData4[,1] <- vecX
matData5[,1] <- vecX
matData6[,1] <- vecX

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
 fileSim4 <- paste(simFolder4,'/',outputType,'.',y,y+1,'.bin',sep='')
 fileSim5 <- paste(simFolder5,'/',outputType,'.',y,y+1,'.bin',sep='')
 fileSim6 <- paste(simFolder6,'/',outputType,'.',y,y+1,'.bin',sep='')
 
 f_isFileReachable(fileSim1, 0, 1)
 f_isFileReachable(fileSim2, 0, 1)
 f_isFileReachable(fileSim3, 0, 1)
 f_isFileReachable(fileSim4, 0, 1)
 f_isFileReachable(fileSim5, 0, 1)
 f_isFileReachable(fileSim6, 0, 1)
 
 nbDays <- 365
 if ((y+1)%%4 == 0) nbDays <- 366
     
 binfile1 = file(fileSim1, 'rb')
 binfile2 = file(fileSim2, 'rb')
 binfile3 = file(fileSim3, 'rb')
 binfile4 = file(fileSim4, 'rb')
 binfile5 = file(fileSim5, 'rb')
 binfile6 = file(fileSim6, 'rb')
 
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

     nbAqCells = readBin(binfile4, integer(), size = 4, endian = 'little')
     recValues4 = readBin(binfile4, double(), n = nbAqCells, size = 8, endian = 'little')
     nbAqCells = readBin(binfile4, integer(), size = 4, endian = 'little')
     
     nbAqCells = readBin(binfile5, integer(), size = 4, endian = 'little')
     recValues5 = readBin(binfile5, double(), n = nbAqCells, size = 8, endian = 'little')
     nbAqCells = readBin(binfile5, integer(), size = 4, endian = 'little')
     
     nbAqCells = readBin(binfile6, integer(), size = 4, endian = 'little')
     recValues6 = readBin(binfile6, double(), n = nbAqCells, size = 8, endian = 'little')
     nbAqCells = readBin(binfile6, integer(), size = 4, endian = 'little')
     
     if (r == 1) # 1 = Hydraulic Head
     {
       for (p in (1:nbPiezo)) 
       {
         matData1[totalDayCounter,2*p+1] <- recValues1[properties[p,9]]  # Searching the absolute ID position in the binary record
         matData2[totalDayCounter,2*p+1] <- recValues2[properties[p,9]]
         matData3[totalDayCounter,2*p+1] <- recValues3[properties[p,9]]
         matData4[totalDayCounter,2*p+1] <- recValues4[properties[p,9]]
         matData5[totalDayCounter,2*p+1] <- recValues5[properties[p,9]]
         matData6[totalDayCounter,2*p+1] <- recValues6[properties[p,9]]
         
         }        
       } 
     }
   }
   close(binfile1)
   close(binfile2)  
   close(binfile3)
   close(binfile4)
   close(binfile5)
   close(binfile6)
   
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
  figTitle <- paste("Lieu :",properties[i,4],sep=" ")
      
  # Plotting
  dataFrame1 <- as.data.frame(matData1)    
  dataFrame2 <- as.data.frame(matData2)    
  dataFrame3 <- as.data.frame(matData3)    
  dataFrame4 <- as.data.frame(matData4)    
  dataFrame5 <- as.data.frame(matData5)    
  dataFrame6 <- as.data.frame(matData6)    
  dataFrameMNT <- as.data.frame(matMNT)
  
   pl =  ggplot(dataFrame1) +
         geom_line(aes(x = dataFrame1[,1], y = dataFrame1[,2*i+1], color = '(1) R0.60'), size = 0.2, alpha = 1.0)  +
         geom_line(aes(x = dataFrame2[,1], y = dataFrame2[,2*i+1], color = '(2) R0.70'), size = 0.3, alpha = 1.0)  +
         geom_line(aes(x = dataFrame3[,1], y = dataFrame3[,2*i+1], color = '(3) R0.80'), size = 0.4, alpha = 1.0)  +
         geom_line(aes(x = dataFrame4[,1], y = dataFrame4[,2*i+1], color = '(4) R0.90'), size = 0.5, alpha = 1.0)  +
         geom_line(aes(x = dataFrame5[,1], y = dataFrame5[,2*i+1], color = '(5) R1.00'), size = 0.6, alpha = 1.0)  +
         geom_line(aes(x = dataFrame6[,1], y = dataFrame6[,2*i+1], color = '(6) R1.15'), size = 0.7, alpha = 1.0)  +
         geom_line(aes(x = dataFrameMNT[,1], y = dataFrameMNT[,2], color = '(7) MNT - Cote min.'), size = 0.4, alpha = 0.5, linetype = "dashed")  +
         geom_line(aes(x = dataFrameMNT[,1], y = dataFrameMNT[,3], color = '(8) MNT - Cote max.'), size = 0.4, alpha = 0.5, linetype = "dashed")  +
         geom_line(aes(x = dataFrameMNT[,1], y = dataFrameMNT[,4], color = '(9) MNT - Cote moyenne'), size = 0.8, alpha = 0.8)  +
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
                     legend.position="right", legend.direction="vertical",
                     axis.title = element_text(family = "Helvetica", size = (13), colour = "black"),
                     axis.text.y = element_text(family = "Helvetica", colour = "black", size = (13)),
                     axis.text.x = element_text(family = "Helvetica", colour = "black", angle = 45, hjust = 1, size = (9)),
                     plot.margin = unit(c(1,1,1,1), "cm"))
     
   # Printing graphs out...
   print(paste("Plotting : ",i,'out of',nbPiezo))
     
   print(pl + myGraphOptions + scale_color_manual(values=c("#ffc265", "#e88e05","#FF5733","#C70039","#900C3F","#581845","grey10","grey10","black")) +
         labs(color = "Code couleur : ") +
         scale_x_continuous(limits=c(startGraph, endGraph),breaks=c(154,161,168,175,182,189,196,203,210,217,224,231,238),
                            labels=c("01/01/1910","08/01/1910","15/01/1910","22/01/1910","29/01/1910","05/02/1910","12/02/1910",
                                     "19/02/1910","26/02/1910","05/03/1910","12/03/1910","19/03/1910","26/03/1910")) + 
         ggtitle(label = figTitle) +
         ggtitle(label = figTitle))
   remove(matMNT)
   
}
# Closing pdf
dev.off()

print(paste("Done. Output pdf file located in", workingDirectory,sep=''))
