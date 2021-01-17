# Function : For a given AQ cell, compute the corresponding absolute ID from the couple (idLay, idInt)

# idLay and idInt need to start at 0.
# return value : Absolute ID

# still to be verified 
f_Int2AbsAqID <- function(CellsPerLayer, idLay, idInt)
{
  nbLayer <- length(CellsPerLayer)
  cumIDs <- vector(mode = "integer", length = nbLayer)
  sum <- 0
  absValue <- 0
  
  for (i in (1:nbLayer))
  {
    sum <- sum + 1
    cumIDs[i] <- sum
    sum <- sum+CellsPerLayer[i]-1
  } 
  absValue <- cumIDs[idLay+1] + idInt
  return(absValue)
}