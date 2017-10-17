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
  df <- df[order(differences), ]
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
    print(paste('Looking at', fileCounter, 'of', count, 'files'))
    
    Y <- readFITS(x)
    s <- Y$hdr[which(Y$hdr == "IMAGETYP") + 1]
    
    # sort by filter if flat field
    if (s == type && type == 'Flat Field') {
      # print(paste(filter,'??',filters))
      # print(filter %in% filters)
      if (filter %in% filters) {
        flatCounter[[get_filter_index(filter)]] <-
          flatCounter[[get_filter_index(filter)]] + 1
        counter <- counter + 1
        print(paste('Found', counter, 'new flat files of type', s))
        filter <- Y$hdr[which(Y$hdr == "FILTER") + 1]
        if (filter == "g''" || filter == "gp") {
          flatFieldFilters[[1]][[length(gFiles) + 1]] <- x
        } else if (filter == "i''" || filter == "ip") {
          flatFieldFilters[[2]][[length(gFiles) + 1]] <- x
        } else if (filter == "r''" || filter == "rp") {
          flatFieldFilters[[3]][[length(gFiles) + 1]] <- x
        } else if (filter == "Y''" || filter == "Yp") {
          flatFieldFilters[[4]][[length(gFiles) + 1]] <- x
        } else if (filter == "Z''" || filter == "Zp") {
          flatFieldFilters[[5]][[length(gFiles) + 1]] <- x
        } else {
          print("NO FILTER: file should have a filter")
          stopifnot(FALSE)
        }
      }
      done <- TRUE
      for (f in filters) {
        if (flatCounter[[get_filter_index(f)]] < 3)
          done <- FALSE
      }
      if (done)
        break
    } else if (s == type) {
      counter <- counter + 1
      print(paste('Found', counter, 'new files of type', s))
      fileList[[length(fileList) + 1]] <- x
    }
    
    if (type == 'Flat Field') {
      done <- TRUE
      for (f in filters) {
        if (flatCounter[[get_filter_index(f)]] < 3)
          done <- FALSE
      }
      if (done)
        break
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

get_filter_index <- function(filter) {
  if (filter == "g''" || filter == "gp")
    return(1)
  if (filter == "i''" || filter == "ip")
    return(2)
  if (filter == "r''" || filter == "rp")
    return(3)
  if (filter == "Y''" || filter == "Yp")
    return(4)
  if (filter == "Z''" || filter == "Zp")
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