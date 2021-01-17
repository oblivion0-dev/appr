# Function : Returns the appropriate number of binary records which are written for 
# each day of simulation in CaWaqs output files
# Please, refer to the CaWaqs user guide for more informations.

f_setNbRecOutputs <- function(binTypeFile)
{
  nbRec <- -999
  
  nbRec = switch(   
    binTypeFile,  
    "AQ_MB"= 16,
    "AQ_H"= 1,   
    "NONSAT_MB"= 4,   
    "WATBAL_MB"= 10,   
    "HYD_H"= 2,   
    "HYD_Q"= 1,   
    "HYD_MB"= 9,   
    "HDERM_Q"= 1,   
    "HDERM_MB"= 9,   
    "WATBAL_TRANSPORT"= -999,   
    "NSAT_TRANSPORT"= -999,   
    "HDERM_TRANSPORT"= -999,   
    "HYD_TRANSPORT"= -999,   
    "AQ_TRANSPORT"= -999   
  ) 
  if (nbRec < 0){
    print(paste(binTypeFile,'type file unknown or number of binary records not set yet.',sep=" "))
    stop()
  }
  print(paste('Expected number of records for each time step :',nbRec,sep=' '))

  return (nbRec)
}