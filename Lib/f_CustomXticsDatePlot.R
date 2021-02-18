# Function : Creating custom breaks and xtics labels for plots using dates

# Inputs : VecX : vector of n integers ranging from 1 to n (number of days of simulation)
#          VecDate : vector of corresponding dates in aaaa-mm-dd format
 
f_CustomXticsDatePlot<-function(vecX,vecDate,startYear){
  
  breakpoints <- vector()
  xLabels <- vector()

  pos <- 1

  while (pos < length(vecX))
  {
    nbDays <- 365
    if ((startYear+1)%%4 == 0) nbDays <- 366
    pos <- pos + nbDays
    breakpoints <- append(breakpoints,pos)
    xLabels <- append(xLabels,vecDate[pos])
    startYear <- startYear + 1
  }
  returnList <- list("breaks" = breakpoints[1:length(breakpoints)-1], "labels" = xLabels[1:length(xLabels)-1])

  return(returnList)
}
