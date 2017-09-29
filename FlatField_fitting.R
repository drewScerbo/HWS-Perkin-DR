# FlatField fitting to a histogram
# Must run Filter_Image_Processing_Script.R to get masterflats

colors <- list()
scienceFrameFilters <- list()
masterFlats <- list()
if (exists("masterGFlat")) {
  masterFlats[[length(masterFlats) + 1]] <- masterGFlat
  colors[[length(colors) + 1]] <- rgb(1, 0, 0, 0.5)
  scienceFrameFilters[[length(scienceFrameFilters) + 1]] <-
    "g Filter"
}
if (exists("masterRFlat")) {
  masterFlats[[length(masterFlats) + 1]] <- masterRFlat
  colors[[length(colors) + 1]] <- rgb(0, 0, 1, 0.5)
  scienceFrameFilters[[length(scienceFrameFilters) + 1]] <-
    "r Filter"
}
if (exists("masterIFlat")) {
  masterFlats[[length(masterFlats) + 1]] <- masterIFlat
  colors[[length(colors) + 1]] <- rgb(0, 1, 0, 0.5)
  scienceFrameFilters[[length(scienceFrameFilters) + 1]] <-
    "i Filter"
}
if (exists("masterYFlat")) {
  masterFlats[[length(masterFlats) + 1]] <- masterYFlat
  colors[[length(colors) + 1]] <- rgb(1, 0, 1, 0.5)
  scienceFrameFilters[[length(scienceFrameFilters) + 1]] <-
    "y Filter"
}
if (exists("masterZFlat")) {
  masterFlats[[length(masterFlats) + 1]] <- masterZFlat
  colors[[length(colors) + 1]] <- rgb(0, 1, 1, 0.5)
  scienceFrameFilters[[length(scienceFrameFilters) + 1]] <-
    "z Filter"
}

ymax <- 0
for (i in length(masterFlats):1) {
  if (is.nan(masterFlats[[i]][xNumber / 2, yNumber / 2])) {
    # masterFlats[[i]] <- NULL
    colors[[i]] <- NULL
    scienceFrameFilters[[i]] <- NULL
  }
}

ymax <- 0

histOfFlats <- list()
histOfFlats[[1]] <- hist(
  masterFlats[[1]],
  col = colors[[1]],
  xlim = c(.9, 1.1),
  main = "Master Flats",
  breaks = 100,
  freq = TRUE
)

for (i in 2:length(masterFlats)) {
  histOfFlats[[i]] <- hist(
    masterFlats[[i]],
    col = colors[[i]],
    breaks = 100,
    freq = TRUE,
    add = T
  )
}

for (i in 1:length(histOfFlats)) {
  a <- max(histOfFlats[[i]]$counts)
  if (a > ymax)
    ymax <- a
}

histOfFlats <- list()
histOfFlats[[1]] <- hist(
  masterFlats[[1]],
  col = colors[[1]],
  xlim = c(.9, 1.1),
  ylim = c(0, ymax),
  main = "Master Flats",
  breaks = 200,
  freq = TRUE
)

for (i in 2:length(masterFlats)) {
  histOfFlats[[i]] <- hist(
    masterFlats[[i]],
    col = colors[[i]],
    breaks = 200,
    freq = TRUE,
    add = T
  )
}

legend("topright",
       c(unlist(scienceFrameFilters)),
       col = c(unlist(colors)),
       lwd = 5)



percentageOut <- list()
# path <-
#   "C:/Users/drews/OneDrive/Documents/Hobart 16-17/Astronomy/FlatFieldSummary.txt"
path <- "/Users/drewScerbo/Desktop/20170911/FlatFieldSummary.txt"

file.create(path)
fileSummary <- file(path)
titles <- c("Min:", "1st Q:", "Med:", "Mean:", "3rd Q:", "Max:")
lines <- "FlatFlield Summary:\n"

stDevs <- list()
summarys <- list()

for (i in 1:length(masterFlats)) {
  total <- sum(unlist(histOfFlats[[i]]$counts))
  
  indexes <-
    which(histOfFlats[[i]]$mids < 0.9)
  count <- sum(unlist(histOfFlats[[i]]$counts[indexes]))
  percentageOut[[length(percentageOut) + 1]] <- count / total
  
  indexes <- which(histOfFlats[[i]]$mids > 1.1)
  count <- sum(unlist(histOfFlats[[i]]$counts[indexes]))
  percentageOut[[length(percentageOut) + 1]] <- count / total
  
  indexes <- which(histOfFlats[[i]]$mids < 1.1
                   & histOfFlats[[i]]$mids > 0.9)
  
  lo <- loess(histOfFlats[[i]]$counts[indexes]
              ~ histOfFlats[[i]]$mids[indexes], family = "gaussian")
  lines(
    histOfFlats[[i]]$mids[indexes],
    predict(lo),
    type = "l",
    col = colors[[i]],
    lwd = 7
  )
  
  summarys[[i]] <- summary(masterFlats[[i]])
  stDevs[[i]] <- sd(masterFlats[[i]])
  
  lines <- paste(lines, scienceFrameFilters[[i]], sep = "\n")
  for (j in 1:6) {
    lines <- paste(lines, summarys[[i]][[j]], sep = "\n")
  }
  lines <- paste(lines, paste("St. Dev:", stDevs[[i]]), sep = "\n")
  
  lines <- paste(
    lines,
    "Percentage outside graph:",
    paste("Below 0.9:", format(
      percentageOut[[2 * i - 1]], digits =
        4, trim = TRUE
    )),
    paste("Above 1.1:", format(
      percentageOut[[2 * i]], digits =
        4, trim = TRUE
    )),
    "",
    sep = "\n"
  )
}
writeLines(lines, con = fileSummary)
close(fileSummary)

f <- function(par) {
  m1 <- par[1]
  m2 <- par[4]
  
  sd1 <- par[2]
  sd2 <- par[5]
  
  k1 <- par[3]
  k2 <- par[6]
  modelY <- k1 * exp(-0.5 * ((x - m1) / sd1) ^ 2)
  modelY <- modelY + k2 * exp(-0.5 * ((x - m2) / sd2) ^ 2)
  
  plot(x, y, xlim = c(.8, 1.2))
  lines(x, modelY, col = 'red')
  return(modelY)
}

f2 <- function(par) {
  m1 <- par[1]
  m2 <- par[4]
  
  sd1 <- par[2]
  sd2 <- par[5]
  
  k1 <- par[3]
  k2 <- par[6]
  modelY <- k1 * exp(-0.5 * ((x - m1) / sd1) ^ 2)
  modelY <- modelY + k2 * exp(-0.5 * ((x - m2) / sd2) ^ 2)
  sum((y - modelY) ^ 2)
}
x <- histOfFlats[[1]]$mids
y <- histOfFlats[[1]]$counts
guess1 <- c(.98, 0.005, 3.1e5, 1.04, 0.005, 2e5)
opt <-
  optim(guess1,
        f2,
        method = "CG",
        control = list(reltol = 1e-11))
pdf(
  paste(
    "/Users/drewScerbo/Desktop/20170911/",
    scienceFrameFilters[[1]],
    ".pdf",
    sep = ""
  ),
  width = 400,
  height = 400
)
yModel1 <- f(opt$par)
dev.off()

x <- histOfFlats[[2]]$mids
y <- histOfFlats[[2]]$counts
guess2 <- c(1, 0.01, 2.6e6, 1, 0.01, 1.6e6)
opt <-
  optim(guess2,
        f2,
        method = "CG",
        control = list(reltol = 1e-11))
pdf(
  paste(
    "/Users/drewScerbo/Desktop/20170911/",
    scienceFrameFilters[[2]],
    ".pdf",
    sep = ""
  ),
  width = 400,
  height = 400
)
yModel2 <- f(opt$par)
dev.off()

x <- histOfFlats[[3]]$mids
y <- histOfFlats[[3]]$counts
guess3 <- c(1, 0.05, 1.5e6, 1, 0.01, 1.4e6)
opt <-
  optim(guess3,
        f2,
        method = "CG",
        control = list(reltol = 1e-11))
pdf(
  paste(
    "/Users/drewScerbo/Desktop/20170911/",
    scienceFrameFilters[[3]],
    ".pdf",
    sep = ""
  ),
  width = 400,
  height = 400
)
yModel3 <- f(opt$par)
dev.off()