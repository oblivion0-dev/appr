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
graphName <- "HYDRARIV_AUSTERLITZ"

# Path to the file containing the values to plot
datafile <- "Data/Cote_HYDRARIV_AUSTERLITZ.txt"

# Number of days of the time series
nbDays <- 59
  
# # END OF USER CORNER --------------------------------------------------------------------------------------
  
# output filename
fileTitle <- paste(graphName,'.pdf')
 
f_isFileReachable(datafile, 0, 0)

# Opening pdf output file
pdf(fileTitle, height = 7, width = 10)

# Loading data table cointaining the values to plot
data <- read.table(datafile, header = TRUE, na.strings = 'NA')

# Starting day of graph
dateStart <- "1910-01-10"

# (Ox) dates management
vecX <- seq(1, nbDays)
vecDate <- seq(as.Date(dateStart), as.Date(dateStart)+nbDays-1, by = 'day')

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
figTitle <- paste("Cotes HYDRARIV : la Seine à Paris-Austerlitz")
     
# Plotting
dataFrame <- as.data.frame(data)    
axeX <- as.data.frame(vecX)

pl =  ggplot(dataFrame) +
      geom_line(aes(x = axeX[,1], y = dataFrame[,2], color = 'R0.6'), size = 0.4, alpha = 1.0)  +
      geom_line(aes(x = axeX[,1], y = dataFrame[,3], color = 'R0.7'), size = 0.4, alpha = 1.0)  +
      geom_line(aes(x = axeX[,1], y = dataFrame[,4], color = 'R0.8'), size = 0.4, alpha = 1.0)  +
      geom_line(aes(x = axeX[,1], y = dataFrame[,5], color = 'R0.9'), size = 0.4, alpha = 1.0)  +
      geom_line(aes(x = axeX[,1], y = dataFrame[,6], color = 'R1.00'), size = 0.4, alpha = 1.0)  +
      geom_line(aes(x = axeX[,1], y = dataFrame[,7], color = 'R1.15'), size = 0.4, alpha = 1.0)  +
      ggtitle(label = figTitle) + 
      labs(x = 'Temps (jours)', y = 'Cote en rivière (m NGF)', 
      caption = paste('Simulation : CaW2.93-Appl.PPC',sep=''))
 
   
# Aesthetic settings
myGraphOptions <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15), hjust = 0.5),
                  plot.subtitle = element_text(hjust = 0.5, size = (13)),
                  legend.title = element_text(colour = "black",  face = "bold.italic", family = "Helvetica"),
                  legend.text = element_text(face = "italic", colour="black",family = "Helvetica"),
                  legend.position="top", legend.direction="horizontal",
                  axis.title = element_text(family = "Helvetica", size = (13), colour = "black"),
                  axis.text.y = element_text(family = "Helvetica", colour = "black", size = (18)),
                  axis.text.x = element_text(family = "Helvetica", colour = "black", angle = 45, hjust = 1, size = (8)),
                  plot.margin = unit(c(1,1,1,1), "cm"))
      

# Printing out...
print(paste("Plotting..."))
      
print(pl + myGraphOptions + scale_color_manual(values=c("#ffc265", "#e88e05","#FF5733","#C70039","#900C3F","#581845")) +
        labs(color = "Code couleur : ") + 
        scale_x_continuous(
          limits=c(1, 60), 
          breaks=c(1,16,17,18,19,20,21,22,23,59), 
          label=c("10-janv","25-janv","26-janv","27-janv","28-janv","29-janv","30-janv","31-janv","1-fevr","9-mars")) +
        ggtitle(label = figTitle)) 


# Closing pdf
dev.off()
 
print(paste("Done. Output pdf file located in", workingDirectory,sep=''))