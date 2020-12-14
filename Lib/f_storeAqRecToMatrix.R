# Function :

f_storeAqRecToMatrix<-function(path,yearStart,yearEnd,idRecExtract,matrixObj,countDays) {

  # Constants
  nbRecAq <- 16   # Number of binary records in CaWaQS MB_AQ file
  
  simFilename <- paste(path,'AQ_MB.',yearStart,yearEnd,'.bin',sep="")
  
  if (file.access(simFilename) < 0) {
    print(paste('Simulation file ',simFilename,' unreachable. Exiting.'))
    stop()
  } else 
  {
    nbDays <- 365
    if (yearEnd%%4 == 0){
      nbDays <- 366
    }
    
    binfile = file(simFilename, "rb")
    print(paste('Reading in progress for binary file : ',simFilename,'...'))
    
    for (i in (1:nbDays))
    {
      print(paste('Reading in progress for day : ',i,'out of',nbDays))
      countDays = countDays + 1
      print(countDays)
      for (j in (1:nbRecAq))
      {
        nbAqCells = readBin(binfile, integer(), size=4, endian = "little")
        recValues = readBin(binfile, double(), n = nbAqCells, size=8, endian = "little")
        nbAqCells = readBin(binfile, integer(), size=4, endian = "little")
      }
    }
    close(binfile)
    print(paste('Reading for binary file : ',simFilename,'done. File closed.'))
  }
  
  return (matrixObj)
}