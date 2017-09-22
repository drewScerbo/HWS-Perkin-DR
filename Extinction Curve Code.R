#install.packages("FITSio")
library(FITSio)

# give file path to directory containing images
s <- "C:/Users/drews/OneDrive/Documents/Hobart_16-17/Astronomy"

# get all the .fits files
files <- list.files(path=s,pattern="*.fits", full.names=T, recursive=TRUE)
print(paste("number of files:",length(files)),quote = FALSE)

Y <- readFITS(files[[1]])

# xNumber <- 3352
# yNumber <- 2532
xNumber <- strtoi(Y$hdr[which(Y$hdr=="NAXIS1")+1])
yNumber <- strtoi(Y$hdr[which(Y$hdr=="NAXIS2")+1])

centerStarX <- 2248
centerStarY <- 498
# centerStarX <- 2125
# centerStarY <- 1265

leftStar <- centerStarX - 42
rightStar <- centerStarX + 42
topStar <- centerStarY - 42
bottomStar <- centerStarY + 42

gLightFiles <- list()
iLightFiles <- list()
rLightFiles <- list()
zLightFiles <- list()
yLightFiles <- list()

bCounter <- 0
dCounter <- 0
counter <- 0 

i <- 0
for (x in files){
  print(x)
  i <- i + 1
  if (i > 65) break
  Y <- readFITS(x)
  s <- Y$hdr[which(Y$hdr=="IMAGETYP")+1]

  if ( s == "Light Frame"){
    
    #sort science fields into filters
    
    filter <- Y$hdr[which(Y$hdr=="FILTER")+1]
    if (filter == "g''"){
      gLightFiles[[length(gLightFiles)+1]] <- Y
    } else if (filter == "i''"){
      iLightFiles[[length(iLightFiles)+1]] <- Y
    }else if (filter == "r''"){
      rLightFiles[[length(rLightFiles)+1]] <- Y
    }else if (filter == "y''"){
      yLightFiles[[length(yLightFiles)+1]] <- Y 
    }else if (filter == "z''"){
      zLightFiles[[length(zLightFiles)+1]] <- Y
    } else {
      print("NO FILTER",quote = FALSE)
    }
  } else{
    next
  }
  print(paste("Read in",str(i)),quote = FALSE)
}

#airmass <- Y$hdr[which(Y$hdr=="AIRMASS")+1]
airmassFunc <- function (f,armss){
  for (x in f){
    Y <- readFITS(x)
    s <- Y$hdr[which(Y$hdr=="DATE-OBS")+1]
    g <- strsplit(s,"T")
    date <- g[[1]][1] # date
    time <- g[[1]][2] # time, parse to integer
    
    s <- Y$hdr[which(Y$hdr=="CENTALT")+1]
    as.double(s)
    z <- 90 - as.double(s)
    z <- z * (pi / 180) # zenith angle in radians
    X <- (1/cos(z)) * (1 - 0.0012 * ((1/cos(z))^2 - 1))
    armss[[length(armss)+1]] <- X
  }
  return(armss)
}

# calculate airmass
gAirmass <- airmassFunc(gLightFiles,gAirmass)
rAirmass <- airmassFunc(rLightFiles,rAirmass)
iAirmass <- airmassFunc(iLightFiles,iAirmass)
yAirmass <- airmassFunc(yLightFiles,yAirmass)
zAirmass <- airmassFunc(zLightFiles,zAirmass)
print("Finished calculating airmasses",quote = FALSE)

getNStar <- function (f,masterFlat,NStar){
  i <- length(f)
  skyBackground <- c()
  for ( Y in f){
    i <- i - 1
    print(paste(i,"images left to count"))
    
    img <- (Y$imDat  - masterBias - masterDark) / masterFlat
    counter <- 0
    count <- 0
    
    
    for (x in leftStar:rightStar){
      for (y in topStar:bottomStar){
        # if (y < 42 ){
        #   if (42 - x < y && 42 + x) count <- count + img[x,y]
        # } else {
        #   if (42 - x < bottomStar - y &&  bottomStar - y < 42 + x ) count <- count + img[x,y]
        # }
        # totalCount <- totalCount + img[x,y]
        
        count <- count + img[x,y]
        counter <- counter + 1
      }
    }
    
    
    
    ###
    # skyBackground <- get median value for whole img
    # subtract 84^2 * skyBackground instead of averaging
    # histogram optional
    # circular?
    if (counter > 0){
      print(count)
      print(median(img))
      print(counter)
      NStar[[length(NStar)+1]] <- count -(median(img)*counter)
      # print((median(img)*counter^2))
    }
  }
  return(NStar)
}

gLightCount <- list()
rLightCount <- list()
iLightCount <- list()
yLightCount <- list()
zLightCount <- list()


gLightCount <- getNStar(gLightFiles,masterGFlat,gLightCount)
rLightCount <- getNStar(rLightFiles,masterRFlat,rLightCount)
iLightCount <- getNStar(iLightFiles,masterIFlat,iLightCount)
yLightCount <- getNStar(yLightFiles,masterYFlat,yLightCount)
zLightCount <- getNStar(zLightFiles,masterZFlat,zLightCount)

ymin <- min(unlist(gLightCount),unlist(rLightCount),unlist(iLightCount),unlist(yLightCount),unlist(zLightCount))
ymax <- max(unlist(gLightCount),unlist(rLightCount),unlist(iLightCount),unlist(yLightCount),unlist(zLightCount))
xmin <- min(unlist(gAirmass),unlist(rAirmass),unlist(iAirmass),unlist(yAirmass),unlist(zAirmass))
xmax <- max(unlist(gAirmass),unlist(rAirmass),unlist(iAirmass),unlist(yAirmass),unlist(zAirmass))

plotLine <- function(coeffs,color){
  p1 <- c(xmin,xmax)
  p2 <- c(coeffs[[1]] + coeffs[[2]]*xmin,coeffs[[1]] + coeffs[[2]]*xmax)
  lines(p1,p2,type = "l",col = color)
}

plot(gAirmass,gLightCount,col="green",type = "b",xlab = "Airmass",ylab = "Light Count",ylim = c(ymin,ymax), xlim = c(xmin,xmax))
lines(rAirmass,rLightCount,col="red",type = "b")
lines(iAirmass,iLightCount,col="brown",type = "b")
lines(yAirmass,yLightCount,col="blue",type = "b")
lines(zAirmass,zLightCount,col="deeppink",type = "b")

### error bars, xmin to xmax: 1 to 3.5

gDataFrame <- data.frame(gLightCount,gAirmass)
rDataFrame <- data.frame(rLightCount,rAirmass)
iDataFrame <- data.frame(iLightCount,iAirmass)
yDataFrame <- data.frame(yLightCount,yAirmass)
zDataFrame <- data.frame(zLightCount,zAirmass)

path <- "C:/Users/drews/OneDrive/Documents/Hobart_16-17/Astronomy/Extinction Coefficients.txt"
file.create(path)
fileCoef <- file(path)
if (length(gLightCount) > 0){
  linearG <- lm(c(unlist(gLightCount)) ~ c(unlist(gAirmass)), data = gDataFrame)
  coefG <- coefficients(linearG)
  sigG <- summary(linearG)$sigma
  rSquaredG <- summary(linearG)[9]
  g <- paste("GFilter coefficients:",coefG[[2]],":",coefG[[1]],":",sigG,":",rSquaredG,sep="")
  plotLine(coefG,"green")
} else g <- "EMPTY"

if (length(rLightCount) > 0){
  linearR <- lm(c(unlist(rLightCount)) ~ c(unlist(rAirmass)), data = rDataFrame)
  summary(linearR)
  coefR <- coefficients(linearR)
  sigR <- summary(linearR)$sigma
  rSquaredR <- summary(linearR)[9]
  r <- paste("RFilter coefficients:",coefR[[2]],":",coefR[[1]],":",sigR,":",rSquaredR,sep="")
  plotLine(coefR,"red")
} else r <- "EMPTY"

if (length(iLightCount) > 0){
  linearI <- lm(c(unlist(iLightCount)) ~ c(unlist(iAirmass)), data = iDataFrame)
  coefI <- coefficients(linearI)
  sigI <- summary(linearI)$sigma
  rSquaredI <- summary(linearI)[9]
  i <- paste("IFilter coeffiecients:",coefI[[2]],":",coefI[[1]],":",sigI,":",rSquaredI,sep="")
  plotLine(coefI,"brown")
} else i <- "EMPTY"

if (length(yLightCount) > 0){
  linearY <- lm(c(unlist(yLightCount)) ~ c(unlist(yAirmass)), data = yDataFrame)
  coefY <- coefficients(linearY)
  sigY <- summary(linearY)$sigma
  rSquaredY <- summary(linearY)[9]
  y <- paste("YFilter coefficients:",coefY[[2]],":",coefY[[1]],":",sigY,":",rSquaredY,sep="")
  plotLine(coefY,"blue")
} else y <- "EMPTY"

if (length(zLightCount) > 0){
  linearZ <- lm(c(unlist(zLightCount)) ~ c(unlist(zAirmass)), data = zDataFrame)
  coefZ <- coefficients(linearZ)
  sigZ <- summary(linearZ)$sigma
  rSquaredZ <- summary(linearZ)[9]
  z <- paste("ZFilter coefficients:",coefZ[[2]],":",coefZ[[1]],":",sigZ,":",rSquaredZ,sep="")
  plotLine(coefZ,"deeppink")
} else z <- "EMPTY"

writeLines(paste("filter:b:m:sigma:adj. r squared",g,r,i,y,z,sep='\n'),fileCoef)
close(fileCoef)

remove(gLightCount,gAirmass)
remove(rLightCount,rAirmass)
remove(iLightCount,iAirmass)
remove(yLightCount,yAirmass)
remove(zLightCount,zAirmass)

# END