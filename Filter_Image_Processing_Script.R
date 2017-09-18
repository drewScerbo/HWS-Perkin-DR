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

xNumber <- 3352
yNumber <- 2532

# set up lists to hold file names
darkFrameFiles <- list()
gFlatFiles <- list()
iFlatFiles <- list()
rFlatFiles <- list()
zFlatFiles <- list()
yFlatFiles <- list()
neededExposureTimes <- list()

masterBias <-
  array(0, dim = c(xNumber, yNumber)) # make into constants
counter <- 0
count <- length(files)
for (x in files) {
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
  } else if (s == "Light Frame") {
    expTime <- Y$hdr[which(Y$hdr == "EXPTIME") + 1]
    if (which(neededExposureTimes == expTime) == 0)
      neededExposureTimes[[length(neededExposureTimes) + 1]] <-
        expTime
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

counter <- 0
count <- length(darkFrameFiles)
exposureTimes <- c()

for (x in darkFrameFiles) {
  Y <- readFITS(x)
  s <- Y$hdr[which(Y$hdr == "EXPTIME") + 1]
  if (!is.element(s, exposureTimes)) {
    exposureTimes <- c(exposureTimes, s)
  }
  i <- which(neededExposureTimes == s)
  if (length(i) > 0) neededExposureTimes[[i]] <- NULL
}


exposureTimeFiles <-
  array(list(), dim = c(1, length(exposureTimes)))

for (x in darkFrameFiles) {
  Y <- readFITS(x)
  s <- Y$hdr[which(Y$hdr == "EXPTIME") + 1]
  counter <- counter + 1
  for (y in 1:length(exposureTimes)) {
    if (exposureTimes[[y]] == s) {
      exposureTimeFiles[[y]][[length(exposureTimeFiles[[y]]) + 1]] <- x
    }
  }
  
  print(paste("Read in", counter, "dark frames of", count))
}


darkCounter <- function(files) {
  masterDark <-
    array(0, dim = c(xNumber, yNumber))
  for (count in 1:length(files)) {
    if (count == 1)
      next
    counter <- counter + 1
    Y <- readFITS(x)
    masterDark <- masterDark + Y$imDat - masterBias
  }
  masterDark <- masterDark / counter
  return(masterDark)
}

masterDarks <- list()

for (i in 1:length(exposureTimeFiles)) {
  masterDarks[[length(masterDarks) + 1]] <-
    darkCounter(exposureTimeFiles[[i]])
}

# Finished masterDarks


if (length(neededExposureTimes) > 0) {
  for (x in neededExposureTimes) {
    difference <- c(.Machine$integer.max,-1)
    for (y in exposureTimes) {
      print(paste(x,y))
      diff <- abs(strtoi(y) - strtoi(x))
      if (diff < difference) difference <- c(diff,y)
    }
    ####
  }
}

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
    
    
  }
  
  ###### add master bias field into header #####################
  # count <- length(Y$hdr)
  # Y$hdr[[count + 1]] <- "Master Bias"
  # Y$hdr[[count + 2]] <- ""
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
  list(rgb(1, 0, 0, 0.5),
       rgb(0, 0, 1, 0.5),
       rgb(0, 1, 0, 0.5),
       rgb(1, 0, 1, 0.5),
       rgb(1, 1, 0, 0.5))

masterFlats <- list(masterGFlat,
                    masterRFlat,
                    masterIFlat,
                    masterYFlat,
                    masterZFlat)

scienceFrameFilters <- list("g Filter",
                            "r Filter",
                            "i Filter",
                            "y Filter",
                            "z Filter")

for (i in length(masterFlats):1) {
  if (is.nan(masterFlats[[i]][xNumber / 2, yNumber / 2])) {
    masterFlats[[i]] <- NULL
    colors[[i]] <- NULL
    scienceFrameFilters[[i]] <- NULL
  }
}

xmin <- .Machine$integer.max # start at maximum possible value
xmax <- .Machine$integer.min # start at minimum possible value
for (i in length(masterFlats):1) {
  xmin <- min(xmin, c(masterFlats[[i]]))
  xmax <- max(xmax, c(masterFlats[[i]]))
}

hist(
  masterFlats[[1]],
  col = colors[[1]],
  xlim = c(xmin, xmax),
  main = "Master Flats",
  breaks = 100
)

for (i in 2:length(masterFlats)) {
  hist(masterFlats[[i]],
       col = colors[[i]],
       breaks = 100,
       add = T)
}

legend("topright",
       c(unlist(scienceFrameFilters)),
       col = c(unlist(colors)),
       lwd = 5)
