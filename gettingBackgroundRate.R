# directory = path to ~/20171018/modified images, the processed images taken on Oct 18, 2017
# There was a New moon.
# percentloss = 50% at 600 sec // outside 0 to 400
# sd = 39.5
# median = 169.5

# directory = path to ~/20170203/modified images, the processed images taken on Feb 3, 2017
# There was a half moon (43%). 19 images
# percentloss = 0.42% at 5 sec // outside -50 to 100
# sd = 11.02
# median = 26

# directory = path to ~/20171005/modified images, the processed images taken on Oct 18, 2017
# There was a full moon. 11 images
# percentloss = 23% at 600 sec // outside 9400 to 10200
# sd = 82.4
# median = 9757.5


# Settings for finding each background rate
# new
directory = "/Users/drewScerbo/Desktop/ObservingImages/20171018/modified images"
best <- '600.'
low <- 0
high <- 400
  
# half
# directory = "/Users/drewScerbo/Desktop/ObservingImages/20170203/modified images"
# best <- '5.'
# low <- -50
# high <- 100

# full
# directory = "/Users/drewScerbo/Desktop/ObservingImages/20171005/modified images"
# best <- '600.'
# low <- 9400
# high <- 10200

library(FITSio)

files <-
  list.files(
    path = directory,
    pattern = "*.fits",
    full.names = T,
    recursive = TRUE
  )

xNumber <- 3352
yNumber <- 2532

expTimes <- c()
for (x in files){
  zz <- file(description = x, open = "rb")
  header <- readFITSheader(zz)
  hdr <- parseHdr(header)
  s <- hdr[which(hdr == "EXPTIME") + 1]

  print(s)
  expTimes <- c(expTimes,s)
}

arr <-   array(0, dim = c(xNumber, yNumber))
counter <- 0
for (x in files){
  Y <- readFITS(x)
  if (Y$hdr[which(Y$hdr == "EXPTIME") + 1] == best) {
    arr <- arr + Y$imDat
    counter <- counter + 1
  }
}

arr <- arr/counter

# cut off wings
arrL <- arr[which(arr > low)]
arr2 <- arr[which(arr > low)]
percentLossLow <- 100*(length(arr) -length(arrL))/length(arr)

arrH <- arr[which(arr < high)]
arr2 <- arr2[which(arr2 < high)]
percentLossHigh <- 100*(length(arr) -length(arrH))/length(arr)

hist(arr2,breaks = 200,
     freq = TRUE,
     xlim = c(low,high),
     main = "Frequency of General sky background",
     xlab = "Value of pixel [counts]")

gain <- 0.37
med <- median(arr2)
backgroundRate <- median(arr2)*gain/as.numeric(best)
uncertainty <- sd(arr2)
