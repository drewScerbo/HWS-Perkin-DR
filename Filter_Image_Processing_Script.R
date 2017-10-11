# This program will read in Bias, Flat, and Dark Files and make a master Flat for each filter; G,I,R,Z, and Y.
# Andrew Scerbo

# install.packages("FITSio")
library(FITSio)

## get all the .fits files
p <- "/Users/drewScerbo/Desktop/ObservingImages/tester"
# p <-
#   "C:/Users/drews/OneDrive/Documents/Hobart 16-17/Astronomy/images"
files <-
  list.files(
    path = p,
    pattern = "*.fits",
    full.names = T,
    recursive = TRUE
  )
print(paste("number of files:", length(files)), quote = FALSE)

## number of pixels in each image
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
# if flat field. Also takes in all needed exposure times of light frames
masterBias <-
  array(0, dim = c(xNumber, yNumber))
counter <- 0
biasCounter <- 0
modCounter <- 0
count <- length(files)
for (x in files) {
  counter <- counter + 1
  if (length(grep('mod_',basename(x))) > 0) {
    modCounter <- modCounter + 1
    print(paste("Skipped",modCounter,"calibrated images."))
    next
  }
  Y <- readFITS(x)
  s <- Y$hdr[which(Y$hdr == "IMAGETYP") + 1]
  
  if (s == "Bias Frame") {
    # average all biases into a master bias field
    masterBias <- masterBias + Y$imDat
    biasCounter <- biasCounter + 1
    
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
  print(paste("Read in", counter, "of", count, "fits files:", s))
}

remove(files)

## search in near folders for missing data


# average the master bias
if (biasCounter > 0) {
  masterBias <- masterBias / biasCounter
} else
  masterBias <- NULL
# Finished masterBias


# check if any light frame exposure times don't have a dark
# with the same exposure time
exposureTimes <- c() # list of exposure times of the darks
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

for (exp in neededExposureTimes) {
  # for each exposure time without a dark ask for a filepath with more
  # dark files of the same exposure time
  question <- "Enter a file path for darks with exposure time '"
  x <-
    readline(prompt = paste(question, exp, "':", sep = ''))
  if (!file.exists(x)) next
  files <-
    list.files(
      path = p,
      pattern = "*.fits",
      full.names = T,
      recursive = TRUE
    )
  
  # add the new darks with the right exposure time
  addedFrames <- FALSE
  counter <- 0
  for (f in files) {
    Y <- readFITS(f)
    s <- Y$hdr[which(Y$hdr == "IMAGETYP") + 1]
    expTime <- Y$hdr[which(Y$hdr == "EXPTIME") + 1]
    if (s == 'Dark Frame' && expTime == exp) {
      addedFrames <- TRUE
      darkFrameFiles[[length(darkFrameFiles) + 1]] <- f
      counter <- counter + 1
    }
  }
  print(paste('added', counter, 'dark frames of exposure time', exp))
  if (addedFrames) {
    neededExposureTimes[[which(neededExposureTimes == exp)]] <- NULL
  }
}

# create an array of list of lists, where each list holds all the files
# of dark frames that share the same exposure time
exposureTimeDarkFiles <-
  array(list(), dim = c(1, length(exposureTimes)))
count <- length(darkFrameFiles)
counter <- 0

# sort dark frames by exposure time
for (x in darkFrameFiles) {
  Y <- readFITS(x)
  s <- Y$hdr[which(Y$hdr == "EXPTIME") + 1]
  counter <- counter + 1
  i <- which(exposureTimes == s)
  
  # add to the particular inner list that holds the same exposure time
  exposureTimeDarkFiles[[i]][[length(exposureTimeDarkFiles[[i]]) + 1]] <-
    x
  
  print(paste("Read in", counter, "dark frames of", count))
}

# function to average dark frames into a master dark frame
darkCounter <- function(files) {
  masterDark <-
    array(0, dim = c(xNumber, yNumber))
  for (count in 1:length(files)) {
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

# Create master flat for each filter
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

dir.create(file.path(p, 'modified images'))

writeCalScience <- function (science, x, Y) {
  fName <- paste("mod_",basename(x),sep = '')
  dirName <- file.path(dirname(x),'modified images')
  dir.create(dirName,showWarnings = FALSE)
  bzero <- Y$hdr[which(Y$hdr == "BZERO") + 1]
  bscale <- Y$hdr[which(Y$hdr == "BSCALE") + 1]
  f <- file.path(dirName,fName)
  writeFITSim(
    calScience,
    file = f,
    axDat = Y$axDat,
    header = Y$header,
    bscale = strtoi(bscale)
  )
  return(fName)
}

counter <- 0
count <- length(lightFiles)
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
  
  if ((filter == "g''" ||
       filter == "gp") && exists("masterGFlat")) {
    masterFlat <- masterGFlat
  } else if ((filter == "i''" ||
              filter == "ip") && exists("masterIFlat")) {
    masterFlat <- masterIFlat
  } else if ((filter == "r''" ||
              filter == "rp") && exists("masterRFlat")) {
    masterFlat <- masterRFlat
  } else if ((filter == "y''" ||
              filter == "yp") && exists("masterYFlat")) {
    masterFlat <- masterYFlat
  } else if ((filter == "z''" ||
              filter == "zp") && exists("masterZFlat")) {
    masterFlat <- masterZFlat
  } else {
    print(paste("there is no", filter, "filter"))
    stopifnot(FALSE)
  }
  
  # times = [needed exposure time, dark exposure time]
  # dark = closest dark frame
  expTime <- paste(times[[2]], ".", sep = "")
  dark <- masterDarks[[which(exposureTimes == expTime)]]
  calScience <-
    Y$imDat - masterBias - times[[1]] * dark / times[[2]]
  calScience <- calScience / masterFlat
  fName <- writeCalScience(calScience, x, Y)
  print(paste("Wrote", counter, "of", count, "light files as",fName))
}

# write master images
# writeFITSim(masterBias,file = file.path(p,"master_bias.fits"))

remove(a,b,count,counter,diff,difference,
       exp,exposureTimes,expTime,filter,
       i,modCounter,s,times,x,y,
       fName,p,Y,dark,
       neededExposureTimes,calScience,
       exposureTimeDarkFiles)
