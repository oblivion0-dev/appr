# Function : Simple shortcut to test out if a file does exist or not. 
# Returns 0 if success, -1 if failure

# Possible mode values : 
# 0 : test for existence.
# 1 : test for execute permission.
# 2 : test for write permission.
# 4 : test for read permission.

# Possible security values : 
# 0 : Ignore the file if the file does not exist
# 1 : Stops the program if the file does not exist

f_isFileReachable <- function(filename, modeValue, securityValue)
{
  returnValue <- file.access(filename, mode = modeValue)
  
  if (returnValue < 0) {
    if (securityValue == 0) {
      print(paste('File',filename,'unreachable. Ignoring for now but might cause problems later !'))
    } else 
    {
      print(paste('File',filename,'unreachable. Exiting.'))
      stop()
    }
  }
  
  return(returnValue)
}