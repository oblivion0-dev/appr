#Function

f_storePiezObsToMatrix<-function(ObsFilename,dateCawOffset,nbDays,i,matrixObj){
  
  if (file.access(ObsFilename) < 0) {
    print(paste('Observation file ',ObsFilename,' unreachable. Exiting.'))
    stop()
  } 
  else {
    fObs <- read.table(ObsFilename, h=FALSE)
    for (k in (1:length(fObs[,1]))) {
      if (fObs[k,4]-dateCawOffset >= 1 && fObs[k,4]-dateCawOffset <= nbDays){
        matrixObj[fObs[k,4]-dateCawOffset,2*i] <- fObs[k,5]                      # Data storage in matrix over the simulation period only !
      }
    }
    print(paste('Observation data storage in progress for file : ',i,'/',nbPiezo,' : ',fileObs))
  }
  return(matrixObj)
}
