## functions used in Filter_Imgae_Processing_Script.R

# function to compare two date directories
# return > 0 if dir1 came first
# return < 0 if dir2 came first
# return 0 otherwise
compare_dates <- function(dir1, dir2) {
  dir1 <- paste(strtoi(substr(dir1, 1, 4)),
                strtoi(substr(dir1, 5, 6)),
                strtoi(substr(dir1, 7, 8)),
                sep = '-')
  d1 <- as.Date(dir1, "%Y-%m-%d")
  
  dir2 <- paste(strtoi(substr(dir2, 1, 4)),
                strtoi(substr(dir2, 5, 6)),
                strtoi(substr(dir2, 7, 8)),
                sep = '-')
  d2 <- as.Date(dir2, "%Y-%m-%d")
  return(as.numeric(d1 - d2))
}

# sorts 'basenames' directories by difference
# in time to the original directory
sort_files <- function(basenames, original) {
  differences <- c()
  files2 <- c()
  for (f in basenames) {
    comp <- compare_dates(f, original)
    
    if (!is.na(comp)) {
      differences <- c(differences, abs(as.numeric(comp)))
      files2 <- c(files2, f)
    }
  }
  df <- data.frame(file = files2, differnce = differences)
  df <- df[order(differences),]
  return(df)
}

# given a type of image to find and a directory to look
# in, will return list of all files of that type
# will sort by filter if type is 'Flat Field'
get_files_of_type <- function(p.dir, type, filters) {
  print(paste('looking for', type, 'in:', p.dir))
  if (type == 'Flat Field')
    print(paste('Filters:', filters))
  if (!file.exists(p.dir)) {
    print(paste(p.dir, "doesn't exists"))
    return(list())
  }
  new_files <-
    list.files(
      path = p.dir,
      pattern = "*.fits",
      full.names = T,
      recursive = TRUE
    )
  counter <- 0
  modCounter <- 0
  fileCounter <- 0
  count <- length(new_files)
  fileList <- list()
  print(paste('Found', count, 'more files'))
  
  if (type == 'Flat Field') {
    flatCounter <- list(0, 0, 0, 0, 0)
    gFiles <- list()
    iFiles <- list()
    rFiles <- list()
    yFiles <- list()
    zFiles <- list()
    flatFieldFilters <-
      list(gFiles, iFiles, rFiles, yFiles, zFiles)
  }
  
  for (x in new_files) {
    fileCounter <- fileCounter + 1
    
    zz <- file(description = x, open = "rb")
    header <- readFITSheader(zz)
    hdr <- parseHdr(header)
    s <- hdr[which(hdr == "IMAGETYP") + 1]
    filter <- hdr[which(hdr == "FILTER") + 1]
    
    # sort by filter if flat field
    if (s == type && type == 'Flat Field') {
      if (filter %in% filters) {
        flatCounter[[get_filter_index(filter)]] <-
          flatCounter[[get_filter_index(filter)]] + 1
        counter <- counter + 1
        print(paste('Found', counter, 'new flat files of filter', filter))
        filter <- Y$hdr[which(Y$hdr == "FILTER") + 1]
        if (filter == "g''" || filter == "gp") {
          gfiles[[length(gFiles) + 1]] <- x
        } else if (filter == "i''" || filter == "ip") {
          iFiles[[2]][[length(iFiles) + 1]] <- x
        } else if (filter == "r''" || filter == "rp") {
          rFiles[[length(gFiles) + 1]] <- x
        } else if (filter == "Y''" || filter == "Yp") {
          yFiles[[4]][[length(yFiles) + 1]] <- x
        } else if (filter == "z''" || filter == "zp") {
          zFiles[[5]][[length(zFiles) + 1]] <- x
        } else {
          print("NO FILTER: file should have a filter")
          stopifnot(FALSE)
        }
      }
    } else if (s == type) {
      counter <- counter + 1
      print(paste('Found', counter, 'new files of type', s))
      fileList[[length(fileList) + 1]] <- x
    }
    if (counter > 10)
      break
  }
  
  if (type == 'Flat Field') {
    return(list(gFiles, iFiles, rFiles, yFiles, zFiles))
  } else{
    print(paste('Found in total', length(fileList), 'more', type, 'files'))
    return(fileList)
  }
}

# Look for dark files of given types in the given directory
get_dark_files <- function(p.dir, expTimes) {
  print(paste('looking for darks in:', p.dir))
  print(paste('of exposure times:', expTimes))
  if (!file.exists(p.dir)) {
    print(paste(p.dir, "doesn't exists"))
    return(list())
  }
  new_files <-
    list.files(
      path = p.dir,
      pattern = "*.fits",
      full.names = T,
      recursive = TRUE
    )
  counter <- 0
  fileCounter <- 0
  fileList <-
    array(list(), dim = c(1, length(expTimes)))
  count <- length(new_files)
  print(paste('Found', count, 'more files'))
  
  for (x in new_files) {
    fileCounter <- fileCounter + 1
    print(paste('Looking at', fileCounter, 'of', count, 'files'))
    
    zz <- file(description = x, open = "rb")
    header <- readFITSheader(zz)
    hdr <- parseHdr(header)
    s <- hdr[which(hdr == "IMAGETYP") + 1]
    exp <- hdr[which(hdr == "EXPTIME") + 1]
    
    # sort by filter if flat field
    if (s == 'Dark Frame' && exp %in% expTimes) {
      l <- length(fileList[[which(expTimes == exp)]])
      fileList[[which(expTimes == exp)]] <-
        fileList[[which(expTimes == exp)]][[l + 1]] <- x
      counter <- counter + 1
      print(paste('Found', counter, 'new files of type', s))
    }
  }
  
  print(paste('Found in total', length(fileList), 'more Dark Frame files'))
  return(fileList)
}


# get the associated index given a specific filter
# this order of filters is used everywhere
get_filter_index <- function(filter) {
  if (filter == "g''" || filter == "gp")
    return(1)
  if (filter == "i''" || filter == "ip")
    return(2)
  if (filter == "r''" || filter == "rp")
    return(3)
  if (filter == "Y''" || filter == "Yp")
    return(4)
  if (filter == "z''" || filter == "zp")
    return(5)
  return(-1)
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


writeCalScience <- function (science, x, Y) {
  fName <- paste("mod_", basename(x), sep = '')
  dirName <- file.path(dirname(x), 'modified images')
  dir.create(dirName, showWarnings = FALSE)
  bzero <- Y$hdr[which(Y$hdr == "BZERO") + 1]
  bscale <- Y$hdr[which(Y$hdr == "BSCALE") + 1]
  f <- file.path(dirName, fName)
  writeFITSim(
    calScience,
    file = f,
    axDat = Y$axDat,
    header = Y$header,
    bscale = strtoi(bscale)
  )
  return(fName)
}