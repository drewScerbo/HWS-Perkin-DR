## functions used in Filter_Imgae_Processing_Script.R

numberOfDays <- function(date) {
  m <- format(date, format="%m")
  
  while (format(date, format="%m") == m) {
    date <- date + 1
  }
  
  return(as.integer(format(date - 1, format="%d")))
}


# function to compare two date directories
# return > 0 if dir1 came first
# return < 0 if dir2 came first
# return 0 otherwise
compare_dates <- function(dir1,dir2){
  dir1 <- paste(strtoi(substr(dir1,1,4)),
                strtoi(substr(dir1,5,6)),
                strtoi(substr(dir1,7,8)),
                sep = '-')
  d1 <- as.Date(dir1,"%Y-%m-%d")
  
  dir2 <- paste(strtoi(substr(dir2,1,4)),
                strtoi(substr(dir2,5,6)),
                strtoi(substr(dir2,7,8)),
                sep = '-')
  d2 <- as.Date(dir2,"%Y-%m-%d")
  return(as.numeric(d1 - d2))
}

sort_files <- function(basenames,original){
  differences <- c()
  files2 <- c()
  for (f in basenames){
    comp <- compare_dates(f,original)
    
    if (!is.na(comp)) {
      differences <- c(differences,abs(as.numeric(comp)))
      files2 <- c(files2,f)
    }
  }
  df <- data.frame(file = files2,differnce = differences)
  df <- df[order(differences),]
  return(df)
}


get_files_of_type <- function(p,type,fileList){
  if (!file.exists(p)) return(fileList)
  files <-
    list.files(
      path = p,
      pattern = "*.fits",
      full.names = T,
      recursive = TRUE
    )
  counter <- 0
  modCounter <- 0
  count <- length(files)
  print(paste('Found',count,'more files in;'))
  print(p)
  
  if (type == 'Flat Field'){
    gFiles <- list()
    iFiles <- list()
    rFiles <- list()
    zFiles <- list()
    yFiles <- list() 
  }
  
  for (x in files) {
    if (length(grep('mod_',basename(x))) > 0) {
      modCounter <- modCounter + 1
      print(paste("Skipped",modCounter,"calibrated images."))
      next
    }
    
    Y <- readFITS(x)
    s <- Y$hdr[which(Y$hdr == "IMAGETYP") + 1]
    
    # sort by filter if flat field
    if (type == 'Flat Field' && s == type){
      filter <- Y$hdr[which(Y$hdr == "FILTER") + 1]
      
      if (filter == "g''" || filter == "gp") {
        gFiles[[length(gFiles) + 1]] <- x
      } else if (filter == "i''" || filter == "ip") {
        iFiles[[length(iFiles) + 1]] <- x
      } else if (filter == "r''" || filter == "rp") {
        rFiles[[length(rFiles) + 1]] <- x
      } else if (filter == "y''" || filter == "yp") {
        yFiles[[length(yFiles) + 1]] <- x
      } else if (filter == "z''" || filter == "zp") {
        zFiles[[length(zFiles) + 1]] <- x
      } else {
        print("NO FILTER: file should have a filter")
        stopifnot(FALSE)
      }
    } else if (s == type){
      fileList[[length(fileList) + 1]] <- x
    }
  }
  if (type == 'Flat Field'){
    return(list(gFiles,iFiles,rFiles,yFiles,zFiles))
  } else{
    return(fileList)
  }
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