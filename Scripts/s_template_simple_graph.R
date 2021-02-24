# Script générique pour un tracé de graphique

workingDirectory = 'E:/Github/appr/' 
setwd(workingDirectory)

# -------------------------------------------------------------------------------------------

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

# Graph name
graphName <- "toto"

# Path to the file containing the values to plot
datafile <- "Data/cotetest.txt"

# # END OF USER CORNER --------------------------------------------------------------------------------------
  
# output filename
outputPdfFile <- paste(graphName,'.pdf')
 
f_isFileReachable(datafile, 0, 0)

# Loading data table cointaining the values to plot
data <- read.table(datafile, header = TRUE, na.strings = 'NA')

# Opening pdf output file
pdf(outputPdfFile, height = 7, width = 10)
  

# # (Ox) dates management
# vecX <- seq(1, nbDays)
# vecDate <- seq(as.Date(dateStart), as.Date(dateStart)+nbDays-1, by = 'day')
# 
# # Various initializations
# matStat <- matrix(data = NA, nrow = nbPiezo, ncol = nbCrit)
# matData <- matrix(data = NA, nrow = nbDays, ncol = 2*nbPiezo+1)
# totalDayCounter <- 0
#  
# # Loading date values in main data matrix
# matData[,1] <- vecX
#  
# # Custom label management of OX axis (xtics is a list of 2 vectors : breakpoints and associated labels)
# xtics <- f_CustomXticsDatePlot(vecX,vecDate,yearStart) # return breakpoints and labels every august 1st for now
# breakpoints <- as.vector(unlist(xtics['breaks']))
# xticsLabels <- as.Date(unlist(xtics['labels']))
# 
# # So we can only print out years as xtics...
# shortLabels <- c()
# for (i in (1:length(xticsLabels)))
# {
#  shortLabels[i] <- str_sub(xticsLabels[i], 1, 4)
# }
# 
# # -------------------------------------------------------------------------------------

# Setting title
figTitle <- paste("toto")
     
# Plotting
dataFrame <- as.data.frame(data)    
      
pl =  ggplot(dataFrame) +
      geom_line(aes(x = dataFrame[,2], y = dataFrame[,2], color = 'test1'), size = 0.4, alpha = 0.8)  +
      ggtitle(label = figTitle) + 
      labs(x = 'X', y = 'Y', 
      caption = paste('Simulation : XXX',sep=''))
 
   
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
      
# Printing out...
print(paste("Plotting..."))
      
print(pl + myGraphOptions + scale_color_manual(values=c("red", "blue")) +
      labs(color = "Color code : ") 
       #   scale_x_continuous(limits=c(startGraph, endGraph)) 
				#	  breaks=breakpoints,
       #                  labels=shortLabels)
 							  + ggtitle(label = figTitle))


# Closing pdf
dev.off()
 
print(paste("Done. Output pdf file located in", workingDirectory,sep=''))