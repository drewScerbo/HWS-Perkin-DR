




# This program will read in Bias, Flat, and Dark Files and make a master Flat for each filter; G,I,R,Z, and Y.
# Andrew Scerbo

# install.packages("FITSio")
library(FITSio)

# get all the .fits files
s <-
  "C:/Users/drews/OneDrive/Documents/Hobart 16-17/Astronomy/error images"

files <-
  list.files(
    path = s,
    pattern = "*.fits",
    full.names = T,
    recursive = TRUE
  )
print(paste("number of files:", length(files)), quote = FALSE)

Y <- readFITS(files[[1]])

# xNumber <- 3352
# yNumber <- 2532
xNumber <- strtoi(Y$hdr[which(Y$hdr == "NAXIS1") + 1])
yNumber <- strtoi(Y$hdr[which(Y$hdr == "NAXIS2") + 1])


# set up lists to hold file names
darkFrameFiles <- list()
gFlatFiles <- list()
iFlatFiles <- list()
rFlatFiles <- list()
zFlatFiles <- list()
yFlatFiles <- list()

masterBias <-
  array(0, dim = c(xNumber, yNumber)) # make into constants
counter <- 0
count <- length(files)
for (x in files) {
  print(x)
  counter <- counter + 1
  Y <- readFITS(x)
  s <- Y$hdr[which(Y$hdr == "IMAGETYP") + 1]
  
  if (s == "Bias Frame") {
    # average all biases into a master bias field
    masterBias <- masterBias + Y$imDat
    
  } else if (s == "Dark Frame") {
    darkFrameFiles[[length(darkFrameFiles) + 1]] <- x
    
  } else if (s == "Flat Field") {
    filter <- Y$hdr[which(Y$hdr == "FILTER") + 1]
    
    if (filter == "g''") {
      gFlatFiles[[length(gFlatFiles) + 1]] <- x
    } else if (filter == "i''") {
      iFlatFiles[[length(iFlatFiles) + 1]] <- x
    } else if (filter == "r''") {
      rFlatFiles[[length(rFlatFiles) + 1]] <- x
    } else if (filter == "y''") {
      yFlatFiles[[length(yFlatFiles) + 1]] <- x
    } else if (filter == "z''") {
      zFlatFiles[[length(zFlatFiles) + 1]] <- x
    } else {
      print("NO FILTER: file should have a filter")
      stopifnot(FALSE)
    }
  }
  print(paste("Read in", counter, "of", count))
}

remove(files)

if (counter > 0) {
  masterBias <- masterBias / counter
}
# Finished masterBias
##### sort darks by exosure time
##### no darks subbed from flats
masterDark <-
  array(0, dim = c(xNumber, yNumber)) # make into constants
counter <- 0
count <- length(darkFrameFiles)
for (x in darkFrameFiles) {
  Y <- readFITS(x)
  img <- Y$imDat - masterBias
  counter <- counter + 1
  masterDark <- masterDark + img - masterBias
  print(paste("Read in", counter, "of", count))
}

if (counter > 0) {
  masterDark <- masterDark / counter
  remove(darkFrameFiles)
}
# Finished masterDark

# Average the value of each flat field and sort by filters
# master flat for each filter
flatFunction <- function(f) {
  # Get average count of all pixels in an image
  masterFlat <- array(0, dim = c(xNumber, yNumber))
  flatCounter <- 0
  for (file in f) {
    Y <- readFITS(file)
    img <- Y$imDat
    avg <- 0
    counter <- 0
    flatCounter <- flatCounter + 1
    
    # normalize the image
    avg <- mean(img - masterBias)
    masterFlat <- masterFlat + (img - masterBias)/avg
    
    ###### avg into normed master
    ###### histogram for masters
    ###### add master bias field into header
    
    # check if the numbers in the image are real
    if (is.nan(img[nrow(img) / 2, ncol(img) / 2]) == FALSE) {
      img <- img / (avg / counter)
    } else {
      f[[which(f == img)]] <- NULL
    }
  }
  remove(Y)
  return (masterFlat)
}

masterGFlat <- flatFunction(gFlatFiles)
masterRFlat <- flatFunction(rFlatFiles)
masterIFlat <- flatFunction(iFlatFiles)
masterYFlat <- flatFunction(yFlatFiles)
masterZFlat <- flatFunction(zFlatFiles)
remove(gFlatFiles,rFlatFiles,iFlatFiles,yFlatFiles,zFlatFiles)
# Finished making master flats