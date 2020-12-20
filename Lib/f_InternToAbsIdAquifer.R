# Function : Computes absolute starting and ending IDs 
# for a given layerID

# Inputs :
# CellsPerLayer : vector of number of cells per AQ layer
# layerID : aquifer layer ID (starts at 1)

# Outputs : 
# vector of 2 elements : starting and ending cell IDs

f_InternToAbsIdAquifer <- function(CellsPerLayer, layerID)
{
  values <- c(-999,-999)
  
  nbLayer <- length(CellsPerLayer)
  sum <- 0
  
  for (i in (1:(layerID)))
  {
    sum = sum + CellsPerLayer[i]
  }
  values[1] = sum-CellsPerLayer[layerID]+1
  values[2] = sum
  
  return(values)
}