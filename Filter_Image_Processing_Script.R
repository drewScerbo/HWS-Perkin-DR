# This program will read in Bias, Flat, and Dark Files and make a master Flat for each filter; G,I,R,Z, and Y.
# Author Andrew Scerbo
# October 22, 2017

### NOTES
# add to header: dir found cal images in

# install.packages("FITSio")
library(FITSio)
script.dir <- dirname(sys.frame(1)$ofile)
source(file.path(script.dir, 'Image_Processing_Functions.R'))
print(paste("1.", script.dir))
print(paste("2.", getwd()))
print(paste("3. [will input path myself]"))
p <- readline("Which path has the image files? [1,2,3] ")

if (p == '2')
  script.dir <- getwd()
if (p == '3')
  script.dir <- readline("What is the path to the directory? ")
while (!file.exists(script.dir)) {
  print(paste(script.dir, "doesn't exist"))
  script.dir <- readline("What is the path to the directory? ")
}
print(paste("Calibrating images in", script.dir))
interactive <-
  readline(prompt = "Would you like an interactive run? [y/n] ")
interactive <- ifelse(interactive == 'y', TRUE, FALSE)
if (file.exists(file.path(script.dir, "modified images"))) {
  overwrite <-
    readline(prompt = "Would you like to overwrite already modified files? [y/n] ")
  overwrite <- ifelse(overwrite == 'y', TRUE, FALSE)
  if (overwrite) {
    each_one <-
      readline(prompt = "Would you like to choose to overwrite each modified file [y]
               or just overwrite them all? [n] ")
    each_one <- ifelse(each_one == 'y', TRUE, FALSE)
  } else
    each_one <- FALSE
} else {
  each_one <- FALSE
  overwrite <- TRUE
}

## get all the .fits files
files <-
  list.files(
    path = script.dir,
    pattern = "*.fits",
    full.names = T,
    recursive = TRUE
  )
print(paste("number of files:", length(files)), quote = FALSE)

## number of pixels in each image
xNumber <- 3352
yNumber <- 2532

# set up lists to hold file names
biasFrameFiles <- list()
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
modCounter <- 0
count <- length(files)
flatFilters <- c()
lightFilters <- c()
moddedFiles <- c()
if (!overwrite) {
  for (x in files) {
    if (grepl('mod_', basename(x)))
      moddedFiles <-
        c(moddedFiles, substr(basename(x), 5, nchar(basename(x))))
  }
}
biasDirectory <- c()
darkDirectory <- c()
gFlatDirectory <- c()
iFlatDirectory <- c()
rFlatDirectory <- c()
yFlatDirectory <- c()
zFlatDirectory <- c()
for (x in files) {
  counter <- counter + 1
  if (grepl('mod_', basename(x)))
    next
  if (grepl('calibration masters', x))
    next
  if (each_one && basename(x) %in% moddedFiles) {
    each <- readline(prompt = paste("Overwrite? [y/n]", basename(x), ":"))
    if (each == 'n')
      next
  }
  if (!overwrite && basename(x) %in% moddedFiles)
    next
  zz <- file(description = x, open = "rb")
  header <- readFITSheader(zz)
  hdr <- parseHdr(header)
  s <- hdr[which(hdr == "IMAGETYP") + 1]
  
  if (hdr[which(hdr == "NAXIS1") + 1] != xNumber)
    next
  if (hdr[which(hdr == "NAXIS2") + 1] != yNumber)
    next
  
  if (s == "Bias Frame") {
    # average all biases into a master bias field
    biasFrameFiles[[length(biasFrameFiles) + 1]] <- x
    biasDirectory <- basename(script.dir)
    
  } else if (s == "Dark Frame") {
    darkFrameFiles[[length(darkFrameFiles) + 1]] <- x
    darkDirectory <- basename(script.dir)
    
  } else if (s == "Flat Field") {
    filter <- hdr[which(hdr == "FILTER") + 1]
    if (!is.element(filter, flatFilters))
      flatFilters <- c(flatFilters, filter)
    if (filter == "g''" || filter == "gp") {
      gFlatFiles[[length(gFlatFiles) + 1]] <- x
      gFlatDirectory <- basename(script.dir)
    } else if (filter == "i''" || filter == "ip") {
      iFlatFiles[[length(iFlatFiles) + 1]] <- x
      iFlatDirectory <- basename(script.dir)
    } else if (filter == "r''" || filter == "rp") {
      rFlatFiles[[length(rFlatFiles) + 1]] <- x
      rFlatDirectory <- basename(script.dir)
    } else if (filter == "Y''" || filter == "Yp") {
      yFlatFiles[[length(yFlatFiles) + 1]] <- x
      yFlatDirectory <- basename(script.dir)
    } else if (filter == "z''" || filter == "zp") {
      zFlatFiles[[length(zFlatFiles) + 1]] <- x
      zFlatDirectory <- basename(script.dir)
    } else {
      print("NO FILTER: file should have a filter")
      stopifnot(FALSE)
    }
  } else if (s == "Light Frame") {
    expTime <- hdr[which(hdr == "EXPTIME") + 1]
    filter <- hdr[which(hdr == "FILTER") + 1]
    if (!is.element(expTime, neededExposureTimes)) {
      neededExposureTimes[[length(neededExposureTimes) + 1]] <-
        expTime
    }
    if (!is.element(filter, lightFilters))
      lightFilters <- c(lightFilters, filter)
    lightFiles[[length(lightFiles) + 1]] <- x
  }
  close(zz)
  print(paste("Read in", counter, "of", count, "fits files:", s))
}

# remove modded files if not overwriting

## search in near folders for missing data
original <- basename(script.dir)
other_directories = list.dirs(dirname(script.dir), recursive = FALSE)
other_directories = lapply(other_directories, basename)
other_directories[[which(other_directories == original)]] <- NULL

# near by directories in order by closest to original
other_directories <- sort_files(other_directories, original)

if (!interactive && length(biasFrameFiles) < 5) {
  biasCounter <- 0
  for (dir in other_directories[[1]]) {
    p2 <- file.path(dirname(script.dir), dir)
    biasFiles <- get_files_of_type(p2, 'Bias Frame', NULL)
    biasCounter <- biasCounter + length(biasFiles)
    biasFrameFiles <- append(biasFrameFiles, biasFiles)
    if (length(biasFiles))
      biasDirectory <- c(biasDirectory, basename(p2))
    if (biasCounter > 4)
      break
  }
} else if (length(biasFrameFiles) < 5) {
  p2 <-
    readline(prompt = "Enter a file path for bias Frames: ")
  while (!file.exists(p2)) {
    print(paste(p2, "doesn't exist"))
    p2 <-
      readline(prompt = "Enter a file path for bias Frames: ")
  }
  
  files <-
    list.files(
      path = p2,
      pattern = "*.fits",
      full.names = T,
      recursive = TRUE
    )
  for (x in files) {
    zz <- file(description = x, open = "rb")
    header <- readFITSheader(zz)
    hdr <- parseHdr(header)
    s <- hdr[which(hdr == "IMAGETYP") + 1]
    
    if (s == "Bias Frame") {
      biasDirectory <- c(biasDirectory, basename(p2))
      biasFrameFiles[[length(biasFrameFiles) + 1]] <- x
    }
  }
}

# average the master bias
count <- length(biasFrameFiles)
counter <- 0
for (x in biasFrameFiles) {
  counter <- counter + 1
  Y <- readFITS(x)
  masterBias <- masterBias + Y$imDat
  print(paste('Averaging', counter, 'of', count, 'bias files'))
}
if (length(biasFrameFiles) > 0) {
  masterBias <- masterBias / length(biasFrameFiles)
  remove(biasFrameFiles)
} else
  masterBias <- array(0, dim = c(xNumber, yNumber))
# print master mean and stdv
print(paste("Master bias mean:", mean(masterBias)))
print(paste("Master bias std dv:", sd(masterBias)))

# Finished masterBias

# check if any light frame exposure times don't have a dark
# with the same exposure time
exposureTimes <- c() # list of exposure times of the darks
for (x in darkFrameFiles) {
  zz <- file(description = x, open = "rb")
  header <- readFITSheader(zz)
  hdr <- parseHdr(header)
  s <- hdr[which(hdr == "EXPTIME") + 1]
  if (!is.element(s, exposureTimes)) {
    exposureTimes <- c(exposureTimes, s)
  }
  i <- which(neededExposureTimes == s)
  if (length(i) > 0)
    neededExposureTimes[[i]] <- NULL
  close(zz)
}

for (exp in neededExposureTimes) {
  if (as.numeric(exp) < 30)
    neededExposureTimes <-
      neededExposureTimes[neededExposureTimes != exp]
}

if (length(neededExposureTimes) || !length(darkFrameFiles)) {
  # look or ask for dark frames in nearby directories
  if (interactive) {
    for (exp in neededExposureTimes) {
      question <- "Enter a file path for darks with exposure time '"
      p2 <-
        readline(prompt = paste(question, exp, "':", sep = ''))
      if (!file.exists(p2))
        next
      files <-
        list.files(
          path = p2,
          pattern = "*.fits",
          full.names = T,
          recursive = TRUE
        )
      
      # add the new darks with the right exposure time
      addedFrames <- FALSE
      counter <- 0
      for (x in files) {
        zz <- file(description = x, open = "rb")
        header <- readFITSheader(zz)
        hdr <- parseHdr(header)
        exp <- hdr[which(hdr == "EXPTIME") + 1]
        s <- hdr[which(hdr == "IMAGETYP") + 1]
        if (s == 'Dark Frame' && expTime == exp) {
          addedFrames <- TRUE
          darkFrameFiles[[length(darkFrameFiles) + 1]] <- x
          counter <- counter + 1
          darkDirectory <- c(darkDirectory, basename(p2))
        }
        close(zz)
      }
      print(paste('added', counter, 'dark frames of exposure time', exp))
      if (addedFrames) {
        neededExposureTimes[[which(neededExposureTimes == exp)]] <- NULL
      }
    }
  } else {
    for (dir in other_directories[[1]]) {
      p2 <- file.path(dirname(script.dir), dir)
      darkFiles <- get_dark_files(p2, neededExposureTimes)
      darkFrameFiles <-
        append(darkFrameFiles, darkFiles)
      
      if (length(darkFiles)) {
        exposureTimes <- c() # list of exposure times of the darks
        for (x in darkFrameFiles) {
          zz <- file(description = x, open = "rb")
          header <- readFITSheader(zz)
          hdr <- parseHdr(header)
          s <- hdr[which(hdr == "EXPTIME") + 1]
          if (!is.element(s, exposureTimes)) {
            exposureTimes <- c(exposureTimes, s)
          }
          i <- which(neededExposureTimes == s)
          if (length(i) > 0)
            neededExposureTimes[[i]] <- NULL
          close(zz)
        }
        darkDirectory <- c(darkDirectory, basename(p2))
      }
      if (!length(neededExposureTimes))
        break
    }
  }
}

if (FALSE %in% (lightFilters %in% flatFilters)) {
  # Look or ask for flat fields in the needed filters
  if (!interactive) {
    for (dir in other_directories[[1]]) {
      neededFilters <- lightFilters[!lightFilters %in% flatFilters]
      
      if (is.null(neededFilters))
        break
      
      p2 <- file.path(dirname(script.dir), dir)
      flatFiles <-
        get_files_of_type(p2, 'Flat Field', neededFilters)
      
      gFlatFiles <- append(gFlatFiles, flatFiles[[1]])
      iFlatFiles <- append(iFlatFiles, flatFiles[[2]])
      rFlatFiles <- append(rFlatFiles, flatFiles[[3]])
      yFlatFiles <- append(yFlatFiles, flatFiles[[4]])
      zFlatFiles <- append(zFlatFiles, flatFiles[[5]])
      
      if (length(flatFiles[[1]])) {
        gFlatDirectory <- c(gFlatDirectory, basename(p2))
      }
      if (length(flatFiles[[2]])) {
        iFlatDirectory <- c(iFlatDirectory, basename(p2))
      }
      if (length(flatFiles[[3]])) {
        rFlatDirectory <- c(rFlatDirectory, basename(p2))
      }
      if (length(flatFiles[[4]])) {
        yFlatDirectory <- c(yFlatDirectory, basename(p2))
      }
      if (length(flatFiles[[5]])) {
        zFlatDirectory <- c(zFlatDirectory, basename(p2))
      }
      
      flatFilters <- c()
      if (length(gFlatFiles) > 2) {
        flatFilters <- c(flatFilters, "g''", "gp")
      }
      if (length(iFlatFiles) > 2) {
        flatFilters <- c(flatFilters, "i''", "ip")
      }
      if (length(rFlatFiles) > 2) {
        flatFilters <- c(flatFilters, "r''", "rp")
      }
      if (length(yFlatFiles) > 2) {
        flatFilters <- c(flatFilters, "Y''", "Yp")
      }
      if (length(zFlatFiles) > 2) {
        flatFilters <- c(flatFilters, "z''", "zp")
      }
    }
  } else {
    for (f in lightFilters[!lightFilters %in% flatFilters]) {
      neededFilters <- lightFilters[!lightFilters %in% flatFilters]
      if (!f %in% neededFilters)
        next
      if (is.null(neededFilters))
        break
      
      question <- "Enter a file path for flat fields of filter "
      p2 <- readline(prompt = paste(question, f, " :", sep = ''))
      if (!file.exists(p2))
        next
      print(paste("Looking in:", p2))
      new_files <-
        list.files(
          path = p2,
          pattern = "*.fits",
          full.names = T,
          recursive = TRUE
        )
      flatCounter <- c(0, 0, 0, 0, 0)
      print(paste("Found", length(new_files), "new files"))
      for (x in new_files) {
        # print(paste("looking at",x))
        if (grepl('mod_', basename(x)))
          next
        if (grepl('calibration masters', x))
          next
        
        zz <- file(description = x, open = "rb")
        header <- readFITSheader(zz)
        hdr <- parseHdr(header)
        filter <- hdr[which(hdr == "FILTER") + 1]
        s <- hdr[which(hdr == "IMAGETYP") + 1]
        if (s == 'Flat Field' && filter %in% neededFilters) {
          i <- get_filter_index(filter)
          if (i == 1) {
            gFlatFiles <- append(gFlatFiles, x)
          }
          if (i == 2) {
            iFlatFiles <- append(iFlatFiles, x)
          }
          if (i == 3) {
            rFlatFiles <- append(rFlatFiles, x)
          }
          if (i == 4) {
            yFlatFiles <- append(yFlatFiles, x)
          }
          if (i == 5) {
            zFlatFiles <- append(zFlatFiles, x)
          }
          flatCounter[[i]] <- flatCounter[[i]] + 1
          print(paste(
            "Added",
            flatCounter[[i]],
            "flat Fields of",
            filter,
            "filter"
          ))
        }
        close(zz)
      }
      
      remove(new_files)
    }
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
  zz <- file(description = x, open = "rb")
  header <- readFITSheader(zz)
  hdr <- parseHdr(header)
  exp <- hdr[which(hdr == "EXPTIME") + 1]
  counter <- counter + 1
  i <- which(exposureTimes == exp)
  # add to the particular inner list that holds the same exposure time
  exposureTimeDarkFiles[[i]][[length(exposureTimeDarkFiles[[i]]) + 1]] <-
    x
  close(zz)
}

masterDarks <- list()
count <- length(exposureTimeDarkFiles)
if (length(exposureTimeDarkFiles)) {
  for (i in 1:length(exposureTimeDarkFiles)) {
    masterDarks[[length(masterDarks) + 1]] <-
      darkCounter(exposureTimeDarkFiles[[i]])
    print(paste("Averaging", i, "of", count, "master darks"))
    print(paste("exposure time:", exposureTimes[[i]], "seconds"))
  }
} else
  print("Found no darks of a needed exposure time")
# print master mean and stdv
if (length(exposureTimes)) {
  for (i in 1:length(exposureTimes)) {
    print(paste(
      "Master Dark of ",
      exposureTimes[[i]],
      "mean",
      mean(masterDarks[[i]])
    ))
    print(paste("Master Dark of ", exposureTimes[[i]], "std dv", sd(masterDarks[[i]])))
  }
}
# Finished masterDarks


# Create master flat for each filter
if (length(gFlatFiles) > 0) {
  masterGFlat <- flatFunction(gFlatFiles, 'g')
  print(paste("master g flat mean", mean(masterGFlat)))
  print(paste("master g flat std dv", sd(masterGFlat)))
}
if (length(iFlatFiles) > 0) {
  print(paste("master i flat mean", mean(masterIFlat)))
  print(paste("master i flat std dv", sd(masterIFlat)))
  masterIFlat <- flatFunction(iFlatFiles, 'i')
}
if (length(rFlatFiles) > 0) {
  masterRFlat <- flatFunction(rFlatFiles, 'r')
  print(paste("master r flat mean", mean(masterRFlat)))
  print(paste("master r flat std dv", sd(masterRFlat)))
}
if (length(yFlatFiles) > 0) {
  masterYFlat <- flatFunction(yFlatFiles, 'Y')
  print(paste("master Y flat mean", mean(masterYFlat)))
  print(paste("master Y flat std dv", sd(masterYFlat)))
}
if (length(zFlatFiles) > 0) {
  masterZFlat <- flatFunction(zFlatFiles, 'z')
  print(paste("master z flat mean", mean(masterZFlat)))
  print(paste("master z flat std dv", sd(masterZFlat)))
}
remove(gFlatFiles, rFlatFiles, iFlatFiles, yFlatFiles, zFlatFiles)
# Finished master flats

# apply the calibration images to each light image and
# write out the modified image into subfolder 'modified images'
options(warn = -1)
dir.create(file.path(script.dir, 'modified images'))
options(warn = 0)
counter <- 0
count <- length(lightFiles)
for (x in lightFiles) {
  counter <- counter + 1
  Y <- readFITS(x)
  expTime <- Y$hdr[which(Y$hdr == "EXPTIME") + 1]
  difference <- .Machine$integer.max
  times <- c()
  a <- as.numeric(expTime)
  for (y in exposureTimes) {
    b <- strtoi(substr(y, 1, nchar(y) - 1))
    diff <- abs(a - b)
    if (diff < difference) {
      difference <- diff
      times <- c(a, b)
    }
  }
  filter <- Y$hdr[which(Y$hdr == "FILTER") + 1]
  
  # find the corresponding master flat field
  if ((filter == "g''" ||
       filter == "gp") && exists("masterGFlat")) {
    masterFlat <- masterGFlat
  } else if ((filter == "i''" ||
              filter == "ip") && exists("masterIFlat")) {
    masterFlat <- masterIFlat
  } else if ((filter == "r''" ||
              filter == "rp") && exists("masterRFlat")) {
    masterFlat <- masterRFlat
  } else if ((filter == "Y''" ||
              filter == "Yp") && exists("masterYFlat")) {
    masterFlat <- masterYFlat
  } else if ((filter == "z''" ||
              filter == "zp") && exists("masterZFlat")) {
    masterFlat <- masterZFlat
  } else {
    masterFlat <- array(1, dim = c(xNumber, yNumber))
  }
  
  # times = [needed exposure time, dark exposure time]
  # dark = closest dark frame
  expTime <- paste(times[[2]], ".", sep = "")
  if (grepl("NoAutoDark", x) || !length(exposureTimes)) {
    dark <- 0
  } else {
    i <- which(exposureTimes == expTime)
    dark <- masterDarks[[i]]
  }
  
  # don't scale for darks if scaled exp time mean(dark) < mad(bias)
  if (is.null(times))
    times <- c(0, 1)
  if (mean(times[[1]] * dark / times[[2]]) < mad(masterBias)){
    dark <- 0
    i <- -1
  }
  calScience <-
    Y$imDat - masterBias - times[[1]] * dark / times[[2]]
  calScience <- calScience / masterFlat
  fName <- writeCalScience(calScience, x, Y,i)
  print(paste("Wrote", counter, "of", count, "light files as", fName))
}

# write master calibration images
options(warn = -1)
script.dir <- file.path(script.dir, 'calibration masters')
options(warn = 0)
dir.create(script.dir)
writeFITSim(masterBias,
            file = file.path(script.dir, "master_bias.fits"))
print(paste("Writing master flats used"))
if (exists('masterGFlat'))
  writeFITSim(masterGFlat,
              file = file.path(script.dir, "master_GFlat.fits"))
if (exists('masterIFlat'))
  writeFITSim(masterIFlat,
              file = file.path(script.dir, "master_IFlat.fits"))
if (exists('masterRFlat'))
  writeFITSim(masterRFlat,
              file = file.path(script.dir, "master_RFlat.fits"))
if (exists('masterYFlat'))
  writeFITSim(masterYFlat,
              file = file.path(script.dir, "master_YFlat.fits"))
if (exists('masterZFlat'))
  writeFITSim(masterZFlat,
              file = file.path(script.dir, "master_ZFlat.fits"))

if (length(masterDarks)) {
  for (i in 1:length(masterDarks)) {
    print(paste("Writing master dark of exposure time", exposureTimes[[i]]))
    s <-
      paste(as.numeric(exposureTimes[[i]]),
            '_sec_master_dark.fits',
            sep = "")
    writeFITSim(masterDarks[[i]], file = file.path(script.dir, s))
  }
}
remove(a,b,count,counter,diff,difference,
       exp,exposureTimes,expTime,filter,i,
       modCounter,s,times,x,y,comment,
       fName,script.dir,Y,dark,neededExposureTimes,
       calScience,exposureTimeDarkFiles,
       neededFilters,lightFiles,zz,lightFilters,
       p2,original,xNumber,yNumber,p,
       header,hdr,flatFiles,flatFilters,
       dir,darkFrameFiles,files,moddedFiles,
       overwrite,interactive,each_one,darkFiles,
       gFlatDirectory,iFlatDirectory,rFlatDirectory,
       yFlatDirectory,zFlatDirectory,biasFiles,
       biasDirectory,darkDirectory,biasCounter)
