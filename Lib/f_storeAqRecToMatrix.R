# Function :

f_readAqMbFile<-function(path,yearStart,yearEnd,idRecExtract){

  # Constants / Parameters
  nbRecAq <- 16                  # Number of binary records in CaWaQS AQ File
  
  filename <- paste(path,'AQ_MB.',yearStart,yearEnd,'.bin',sep="")
  
  nbDays <- 365
  if (yearEnd%%4 == 0){
    nbDays <- 366
  }
  
  binfile = file(filename, "rb")
  print(paste('Reading in progress for binary file : ',filename,'...'))
  
  for (i in (1:nbDays))
  {
    print(paste('Reading in progress for day : ',i,'out of',nbDays))
    
    for (j in (1:nbRecAq))
    {
      nbAqCells = readBin(binfile, integer(), size=4, endian = "little")
      recValues = readBin(binfile, double(), n = nbAqCells, size=8, endian = "little")
      nbAqCells = readBin(binfile, integer(), size=4, endian = "little")
    }
  }
  close(binfile)
  print(paste('Reading for binary file : ',filename,'done. File closed.'))
  
}