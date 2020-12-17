# Function : Help function providing some structure information about main input files

f_FileInformation <- function(filetype)
{
  print(paste(" Structure information for file:",filetype)) 
  
  if (filetype == 'piezo_info')
  {
   print(paste('Required information : piezometer main informations :'))
   print(paste('Column 1 : Piezometer ID'))
   print(paste('Column 2 : Full BSS code'))
   print(paste('Column 3 : First ten characters of the BSS code (used to create the name of the observation data file)'))
   print(paste('Column 4 : City name'))
   print(paste('Column 5 : Model layer name'))
   print(paste('Column 6 : Cell sub-lithology (if needed)'))
   print(paste('Column 7 : Cell ID'))
   print(paste('Column 8 : Layer ID'))
   print(paste('Column 9 : Absolute cell ID'))
   print(paste('Column 10 : Above soil dominant texture type'))
   print(paste('Column 11 : -'))
   print(paste('Column 12 : -'))

  }
}