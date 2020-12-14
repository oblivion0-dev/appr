# Function : Simple shortcut to test out if a file does exist or not. Program stops if function fails.

f_isFileReachable <- function(filename){
  if (file.access(filename) < 0) {
    print(paste('File ',filename,' unreachable. Exiting.'))
    stop()
  }
}