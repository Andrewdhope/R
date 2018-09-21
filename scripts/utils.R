# description: download a file given a relative directory path, url, and filename
# TODO: error handling, test
# revision date:
#   2/2018 - created # adh
#   2/2018 - added unzip option
grab <- function(fromurl, folder, filename, unzip = FALSE, read = FALSE, compression) {
  path <- paste("./", folder, sep = "")
  if(!file.exists(path)) {
      dir.create(path)
  }
  filepath <- paste(path, "/", filename, sep = "")
  download.file(fromurl, destfile = filepath)
  if(unzip == TRUE) {
    unzip(filepath, exdir = path)
  }
  if(read == TRUE) {
      df <- read.csv(filepath)
      return(df)
  }
}

convertWinPath <- function(path) {
   
}

