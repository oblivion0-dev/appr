setwd("~/R/Scripts")       

# Loading needed packages
library(Metrics)    # For RMSE
library(hydroGOF)   # For KGE

# Loading packages from "tidyverse" core
library(ggplot2) 
library(scales)

# Loading homemade needed functions
source("~/R/Lib/f_StatisticCriterias.R")
source("~/R/Lib/f_read_AqMbFile.R")

# User inputs ----------------------------------------------------------------------------------
nbPiezo <- 1                               # Total number of piezometers
filePiezo <- "~/R/Data/Output_obs_sim.txt"    # ... to be changed (to me read directly from binary output)
caracPiezo <- "~/R/Data/liste_piezos.txt"     # Piezometer attributes textfile
outputPdfFile <- "output_piezo.pdf"           # Pdf output filename
dateStart <- "1970-08-01"                     # Simulation start date
nbDays <- 18263                               # Simulation length (in days)
statStart <- 10746                            # Starting date for criteria calculation (01/01/2000)
statEnd <- nbDays                             # Endinf date for criteria calculation (31/07/2020)
nbCrit <- 5                                   # Total number of criterias
startGraph <- 44041                           # Starting date of graphs
endGraph <- startGraph + nbDays               # Ending date of graphs
# -----------------------------------------------------------------------------------------------

# TEST
f_readAqMbFile('~/R/Data/',2019,2020,1)

# Loading data
results <- read.table(filePiezo, header = FALSE, na.strings = "NA") 
properties <- read.table(caracPiezo, header = FALSE, na.strings = "NA")

# Opening pdf output file
pdf(outputPdfFile, height=7,width=10)

# (Ox) dates management
vecDate <- seq(as.Date(dateStart), as.Date(dateStart)+nbDays-1, by = "day")
vecX <- seq(startGraph,startGraph+nbDays)

# Statistic criterias matrix initialization
matStat <- matrix(data = NA, nrow = nbPiezo, ncol = nbCrit)

for (i in (1:nbPiezo)) 
{
  # Statistic criterias calculations
  statAtt <- f_StatisticCriterias(results[,2*i],results[,2*i+1],'piezo',statStart,statEnd)
  
  # Graph criterias labelling
  statLabel <-paste('n =',statAtt['n'],' - Mean obs. level = ',signif(statAtt['mobs'],3),'m - Mean sim. level = ',signif(statAtt['msim'],3),
                    'm \n RMSE = ',signif(statAtt['rmse'],3),'m - KGE = ',signif(statAtt['kge'],3))
  
  # Setting title
  figTitle <- paste("Piezometer ",properties[i,2]," : ",properties[i,4]," - Layer : ",properties[i,5])

  # Plotting...
  pl = ggplot(results) + 
    geom_line(aes(x = results[,1], y = results[,2*i+1], color = "Simulated head"), size = 0.4, alpha = 0.8) +
    geom_point(aes(x = results[,1], y = results[,2*i], color = "Measured head"), size = 0.6, alpha = 0.3) +
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
