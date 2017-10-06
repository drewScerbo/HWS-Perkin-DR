# This program will read in Bias, Flat, and Dark Files and make a master Flat for each filter; G,I,R,Z, and Y.
# Andrew Scerbo

# install.packages("FITSio")
library(FITSio)

# get all the .fits files
s <- "/Users/drewScerbo/Desktop/ObservingImages/20170423/tester"
# s <-
#   "C:/Users/drews/OneDrive/Documents/Hobart 16-17/Astronomy/images"

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
lightFiles <- list()
gFlatFiles <- list()
iFlatFiles <- list()
rFlatFiles <- list()
zFlatFiles <- list()
yFlatFiles <- list()
neededExposureTimes <- list()

# sort each file into Bias frame, Dark frame, or Flat Field and by filter
# if flat filed. Also takes in all needed exposure times of light frames
masterBias <-
  array(0, dim = c(xNumber, yNumber))
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
    
    if (filter == "g''" || filter == "gp") {
      gFlatFiles[[length(gFlatFiles) + 1]] <- x
    } else if (filter == "i''" || filter == "ip") {
      iFlatFiles[[length(iFlatFiles) + 1]] <- x
    } else if (filter == "r''" || filter == "rp") {
      rFlatFiles[[length(rFlatFiles) + 1]] <- x
    } else if (filter == "y''" || filter == "yp") {
      yFlatFiles[[length(yFlatFiles) + 1]] <- x
    } else if (filter == "z''" || filter == "zp") {
      zFlatFiles[[length(zFlatFiles) + 1]] <- x
    } else {
      print("NO FILTER: file should have a filter")
      stopifnot(FALSE)
    }
  } else if (s == "Light Frame") {
    expTime <- Y$hdr[which(Y$hdr == "EXPTIME") + 1]
    if (!is.element(expTime, neededExposureTimes)) {
      neededExposureTimes[[length(neededExposureTimes) + 1]] <-
        expTime
    }
    lightFiles[[length(lightFiles) + 1]] <- x
  }
  print(paste("Read in", counter, "of", count,":",s))
}

remove(files)

# average the master bias
if (counter > 0) {
  masterBias <- masterBias / counter
}
# Finished masterBias

counter <- 0
count <- length(darkFrameFiles)
exposureTimes <- c() # list of exposure times of the darks

# check if any light frame exposure times don't have a dark
# with the same exposure time
for (x in darkFrameFiles) {
  Y <- readFITS(x)
  s <- Y$hdr[which(Y$hdr == "EXPTIME") + 1]
  if (!is.element(s, exposureTimes)) {
    exposureTimes <- c(exposureTimes, s)
  }
  i <- which(neededExposureTimes == s)
  if (length(i) > 0)
    neededExposureTimes[[i]] <- NULL
}

exposureTimeDarkFiles <-
  array(list(), dim = c(1, length(exposureTimes)))

# sort dark frames by exposure time and
for (x in darkFrameFiles) {
  Y <- readFITS(x)
  s <- Y$hdr[which(Y$hdr == "EXPTIME") + 1]
  counter <- counter + 1
  i <- which(exposureTimes == s)
  
  exposureTimeDarkFiles[[i]][[length(exposureTimeDarkFiles[[i]]) + 1]] <-
    x
  
  print(paste("Read in", counter, "dark frames of", count))
}

# function to average dark frames into a master dark (and bias) frame
darkCounter <- function(files) {
  masterDark <-
    array(0, dim = c(xNumber, yNumber))
  for (count in 1:length(files)) {
    # if (count == 1) # skip the
    #   next
    counter <- counter + 1
    Y <- readFITS(x)
    masterDark <- masterDark + Y$imDat - masterBias
  }
  masterDark <- masterDark / counter
  return(masterDark)
}

masterDarks <- list()
if (length(exposureTimeDarkFiles) > 0) {
  for (i in 1:length(exposureTimeDarkFiles)) {
    masterDarks[[length(masterDarks) + 1]] <-
      darkCounter(exposureTimeDarkFiles[[i]])
  }
}
# Finished masterDarks

# Average the value of each flat field and sort by filters
flatFunction <- function(f) {
  # Get average count of all pixels in an image
  masterFlat <- array(0, dim = c(xNumber, yNumber))
  flatCounter <- 0
  for (file in f) {
    Y <- readFITS(file)
    img <- Y$imDat
    
    # normalize the image
    avg <- mean(img - masterBias)
    masterFlat <- masterFlat + ((img - masterBias) / avg)
    flatCounter <- flatCounter + 1
  }
  
  if (is.nan(masterFlat[xNumber / 2, yNumber / 2]))
    return(NULL)
  else
    return(masterFlat / flatCounter)
}

###### add master bias field into header #####################
# count <- length(Y$hdr)
# Y$hdr[[count + 1]] <- "Master Bias"
# Y$hdr[[count + 2]] <- ""
# master flat for each filter
if (length(gFlatFiles) > 0)
  masterGFlat <- flatFunction(gFlatFiles)
if (length(rFlatFiles) > 0)
  masterRFlat <- flatFunction(rFlatFiles)
if (length(iFlatFiles) > 0)
  masterIFlat <- flatFunction(iFlatFiles)
if (length(yFlatFiles) > 0)
  masterYFlat <- flatFunction(yFlatFiles)
if (length(zFlatFiles) > 0)
  masterZFlat <- flatFunction(zFlatFiles)
remove(gFlatFiles, rFlatFiles, iFlatFiles, yFlatFiles, zFlatFiles)

writeCalScience <- function (science, x, Y) {
  fName <- basename(x)
  dirName <- dirname(x)
  bzero <- Y$hdr[which(Y$hdr == "BZERO") + 1]
  bscale <- Y$hdr[which(Y$hdr == "BSCALE") + 1]
  f <- paste(dirName,"/mod_",fName,sep = "")
  writeFITSim(
    calScience,
    file = f,
    axDat = Y$axDat,
    header = Y$header,
    bscale = strtoi(bscale)
  )
}

counter <- 0
count <- length(lightFiles)
# if (length(lightFiles) > 0) {
  for (x in lightFiles) {
    counter <- counter + 1
    Y <- readFITS(x)
    expTime <- Y$hdr[which(Y$hdr == "EXPTIME") + 1]
    difference <- .Machine$integer.max
    times <- c()
    a <- strtoi(substr(expTime, 1, nchar(expTime) - 1))
    for (y in exposureTimes) {
      b <- strtoi(substr(y, 1, nchar(y) - 1))
      diff <- abs(a - b)
      if (diff < difference) {
        difference <- diff
        times <- c(a, b)
      }
    }
    filter <- Y$hdr[which(Y$hdr == "FILTER") + 1]
    
    if ((filter == "g''" || filter == "gp") && exists("masterGFlat")) {
      masterFlat <- masterGFlat  
    } else if ((filter == "i''" || filter == "ip") && exists("masterIFlat")) {
      masterFlat <- masterIFlat  
    } else if ((filter == "r''" || filter == "rp") && exists("masterRFlat")) {
      masterFlat <- masterRFlat  
    } else if ((filter == "y''" || filter == "yp") && exists("masterYFlat")) {
      masterFlat <- masterYFlat  
    } else if ((filter == "z''" || filter == "zp") && exists("masterZFlat")) {
      masterFlat <- masterZFlat  
    } else {
      print(paste("there is no",filter,"filter"))
      stopifnot(FALSE)
    } 
    
    # times = [needed exposure time, dark exposure time]
    # dark = closest dark frame
    expTime <- paste(times[[2]],".",sep = "")
    dark <- masterDarks[[which(exposureTimes == expTime)]]
    calScience <-
      Y$imDat - masterBias - times[[1]] * dark / times[[2]]
    calScience <- calScience/masterFlat
    writeCalScience(calScience,x,Y)
    print(paste("Wrote",counter,"of",count,"light files"))
  }
# }

# asks for other nights that have calibration images