
workingDirectory = "E:/Github/appr/"                # For Windows
#workingDirectory = "home/pdell/github/appr/"       # For Linux

setwd(workingDirectory)

# -------------------------------------------------------------------------------------------

# Loading needed packages
library(Metrics)    # For RMSE
library(hydroGOF)   # For KGE

# Loading packages from "tidyverse" core
library(ggplot2) 
library(scales)

# Loading entire homemade R library
files <- list.files(paste(workingDirectory,"Lib/"), pattern = "^.*[Rr]$", include.dirs = FALSE, full.names = TRUE)
for (f in files) source(f)

# User inputs ----------------------------------------------------------------------------------

# Starting year of simulation
yearStart <- 2018

# Ending year of simulation
yearEnd <- 2020

# Piezometer attributes textfile
caracPiezo <- paste(workingDirectory,"Data/liste_piezos.txt", sep = "")

# Pdf output filename
outputPdfFile <- "output_piezo.pdf" 

# Simulation start date
dateStart <- "2018-08-01" 

# Simulation length (in days)
nbDays <- 731 

# Starting date for criteria calculation 
statStart <- 1 

# Ending date for criteria calculation (31/07/2020)
statEnd <- nbDays 

# Total number of criterias
nbCrit <- 5 

# Starting date of graphs
startGraph <- 1 

# Ending date of graphs
endGraph <- startGraph + nbDays 

# CaWaQS date offfset (correspond to the CaWaQS starting simulation date-1)
dateCawOffset <- 61572   # 31/07/2018

# Number of binary records in CaWaQS MB_AQ file
nbRecAqMbFile <- 16   

# -----------------------------------------------------------------------------------------------

# Loading piezometers properties
properties <- read.table(caracPiezo, header = FALSE, na.strings = "NA")
#nbPiezo <- length(properties[,1])  
nbPiezo <- 3

# Opening pdf output file
pdf(outputPdfFile, height=7,width=10)

# (Ox) dates management
vecDate <- seq(as.Date(dateStart), as.Date(dateStart)+nbDays-1, by = "day")
vecX <- seq(startGraph,startGraph+nbDays)

totalDayCounter <- 0

# Matrix initializations
matStat <- matrix(data = NA, nrow = nbPiezo, ncol = nbCrit)
matData <- matrix(data = NA, nrow = nbDays, ncol = 2*nbPiezo+1)

# Observation data storage loop
for (i in (1:nbPiezo))
{
  fileObs <- paste(workingDirectory,"Data/H_SEINE/",properties[i,3],".dat",sep = "")
  
  f_isFileReachable(fileObs)
    
  fObs <- read.table(fileObs, h=FALSE)
  for (k in (1:length(fObs[,1]))) {
    if (fObs[k,4]-dateCawOffset >= 1 && fObs[k,4]-dateCawOffset <= nbDays){
      matData[fObs[k,4]-dateCawOffset,2*i] <- fObs[k,5]                      # Data storage in matrix over the simulation period only !
    }
  }
  print(paste('Observation data storage in progress for file : ',i,'/',nbPiezo,' : ',fileObs))
}  
 
# Simulated data storage loop
for (y in (yearStart:(yearEnd-1)))
{
  fileSim <- paste('~/github/appr/Data/AQ_MB.',y,y+1,'.bin',sep="")
  f_isFileReachable(fileSim)
 
  nbDays <- 365
  if ((y+1)%%4 == 0){
    nbDays <- 366
  }
    
  binfile = file(fileSim, "rb")
  print(paste('Reading in progress for binary file : ',fileSim,'...'))
    
  for (d in (1:nbDays))
  {
    #print(paste('Reading in progress for day : ',d,'out of',nbDays))
    totalDayCounter = totalDayCounter + 1

    for (r in (1:nbRecAqMbFile))
    {
      nbAqCells = readBin(binfile, integer(), size=4, endian = "little")
      recValues = readBin(binfile, double(), n = nbAqCells, size=8, endian = "little")
      nbAqCells = readBin(binfile, integer(), size=4, endian = "little")
      
      # il reste a aller chercher la bonne valeur pour l'ensemble des piezos  ###################################
      matData[totalDayCounter,3] <- recValues[1]                        
    }
  }
  close(binfile)
  print(paste('Reading for binary file : ',fileSim,'done. File closed.'))
}

# Plotting main loog  
for (i in (1:nbPiezo))
{
  # Statistic criterias calculations
  statAtt <- f_StatisticCriterias(matData[,2*i],matData[,2*i+1],'piezo',statStart,statEnd)

  # Graph criterias labelling
  statLabel <-paste('n =',statAtt['n'],' - Mean obs. level = ',signif(statAtt['mobs'],3),'m - Mean sim. level = ',signif(statAtt['msim'],3),
                   'm \n RMSE = ',signif(statAtt['rmse'],3),'m - KGE = ',signif(statAtt['kge'],3))

   # Setting title
   figTitle <- paste("Piezometer ",properties[i,2]," : ",properties[i,4]," - Layer : ",properties[i,5])

   # Plotting...
   pl = ggplot(matData) +
     geom_line(aes(x = results[,1], y = matData[,2*i+1], color = "Simulated head"), size = 0.4, alpha = 0.8) +
     geom_point(aes(x = results[,1], y = matData[,2*i], color = "Measured head"), size = 0.6, alpha = 0.3) +
     ggtitle(label = figTitle, subtitle = statLabel) +
     labs(x = "Time (years)", y = "Hydraulic head (mNGF)", caption = "")  # if needed

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

   # Printing... and fiddling a bit more.
   print(pl + myGraphOptions +
           scale_color_manual(values=c("red", "blue")) +
           labs(color = "Color code : ") +
           scale_x_continuous(limits=c(startGraph, endGraph))) #, breaks=c(45000,50000,55000), labels=c(vecDate[vecX[45000]], "five", "eight"))) <- a finir

   # Criteria storage
   matStat[i,1] <- statAtt['n']
   matStat[i,2] <- statAtt['mobs']
   matStat[i,3] <- statAtt['msim']
   matStat[i,4] <- statAtt['rmse']
   matStat[i,5] <- statAtt['kge']

   print(paste("Plotting : ",i, " - Main statistics values : ",statLabel))
 };

# Closing output .pdf
dev.off()

print("Done.")
