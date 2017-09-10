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

xNumber <- 3352
yNumber <- 2532


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
##### sort darks by exosure time, list of each time, write out at end
##### alert for no dark with time as a science
## darks are linear

masterDark <-
  array(0, dim = c(xNumber, yNumber)) # make into constants
counter <- 0
count <- length(darkFrameFiles)
for (x in darkFrameFiles) {
  Y <- readFITS(x)
  counter <- counter + 1
  masterDark <- masterDark + Y$imDat - masterBias
  print(paste("Read in", counter, "of", count))
}

if (counter > 0) {
  masterDark <- masterDark / counter
  remove(darkFrameFiles)
}
# Finished masterDark

# Average the value of each flat field and sort by filters
flatFunction <- function(f) {
  # Get average count of all pixels in an image
  masterFlat <- array(0, dim = c(xNumber, yNumber))
  flatCounter <- 0
  for (file in f) {
    Y <- readFITS(file)
    img <- Y$imDat
    avg <- 0
    
    # normalize the image
    avg <- mean(img - masterBias)
    masterFlat <- masterFlat + ((img - masterBias) / avg)
    flatCounter <- flatCounter + 1
    
    ###### add master bias field into header
    
  }
  # if (exists(Y)) remove(Y)
  return (masterFlat / flatCounter)
}

# master flat for each filter
masterGFlat <- flatFunction(gFlatFiles)
masterRFlat <- flatFunction(rFlatFiles)
masterIFlat <- flatFunction(iFlatFiles)
masterYFlat <- flatFunction(yFlatFiles)
masterZFlat <- flatFunction(zFlatFiles)
remove(gFlatFiles, rFlatFiles, iFlatFiles, yFlatFiles, zFlatFiles)

colors <-
  c(rgb(1, 0, 0, 0.5),
    rgb(0, 0, 1, 0.5),
    rgb(0, 1, 0, 0.5),
    rgb(1, 0, 1, 0.5),
    rgb(1, 1, 0, 0.5))

masterFlats <- list(masterGFlat,
                    masterRFlat,
                    masterIFlat,
                    masterYFlat,
                    masterZFlat)


for (i in length(masterFlats):1) {
  if (is.nan(masterFlats[[i]][xNumber / 2, yNumber / 2])) {
    masterFlats[[i]] <- NULL
  }
}

xmin <- .Machine$integer.max
xmax <- .Machine$integer.min
for (i in length(masterFlats):1) {
  xmin <- min(xmin, c(masterFlats[[i]]))
  xmax <- max(xmax, c(masterFlats[[i]]))
}

xmax <- xmax - xmax / 5
xmin <- xmin + xmin / 10

hist(
  masterFlats[[1]],
  col = colors[1],
  xlim = c(xmin, xmax),
  main = "Master flats"
)

for (i in 2:length(masterFlats)) {
  hist(masterFlats[[i]],
       col = colors[[i]],
       add = T)
}