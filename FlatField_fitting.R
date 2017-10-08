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
  print("added g")
}
if (exists("masterRFlat")) {
  masterFlats[[length(masterFlats) + 1]] <- masterRFlat
  colors[[length(colors) + 1]] <- rgb(0, 0, 1, 0.5)
  scienceFrameFilters[[length(scienceFrameFilters) + 1]] <-
    "r Filter"
  print("added r")
}
if (exists("masterIFlat")) {
  masterFlats[[length(masterFlats) + 1]] <- masterIFlat
  colors[[length(colors) + 1]] <- rgb(0, 1, 0, 0.5)
  scienceFrameFilters[[length(scienceFrameFilters) + 1]] <-
    "i Filter"
  print("added i")
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

for (i in length(masterFlats):1) {
  if (is.nan(masterFlats[[i]][xNumber / 2, yNumber / 2])) {
    # masterFlats[[i]] <- NULL
    colors[[i]] <- NULL
    scienceFrameFilters[[i]] <- NULL
  }
}

dev.new()
histOfFlats <- list()
histOfFlats[[1]] <- hist(
  masterFlats[[1]],
  col = colors[[1]],
  xlim = c(.9, 1.1),
  main = "Master Flats",
  breaks = 200,
  freq = TRUE
)
print(paste("added",1))

for (i in 2:length(masterFlats)) {
  histOfFlats[[i]] <- hist(
    masterFlats[[i]],
    col = colors[[i]],
    breaks = 200,
    freq = TRUE,
    add = T
  )
  print(paste("added",i))
}
dev.off()

ymax <- 0
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
length(histOfFlats)
legend("topright",
       c(unlist(scienceFrameFilters)),
       col = c(unlist(colors)),
       lwd = 5)

percentageOut <- list()
# p <-
#   "C:/Users/drews/OneDrive/Documents/Hobart 16-17/Astronomy/"

p <- "/Users/drewScerbo/Desktop/20170911/"

file.create(file.path(p, "FlatFieldSummary.txt"))
fileSummary <- file.path(p, "FlatFieldSummary.txt", sep = "")
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
  
  # indexes <- which(histOfFlats[[i]]$mids < 1.1
  #                  & histOfFlats[[i]]$mids > 0.9)
  # 
  # lo <- loess(histOfFlats[[i]]$counts[indexes]
  #             ~ histOfFlats[[i]]$mids[indexes], family = "gaussian")
  # lines(
  #   histOfFlats[[i]]$mids[indexes],
  #   predict(lo),
  #   type = "l",
  #   col = colors[[i]],
  #   lwd = 7
  # )
  
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

f <- function(par,i) {
  m1 <- par[1]
  m2 <- par[4]
  
  sd1 <- par[2]
  sd2 <- par[5]
  
  k1 <- par[3]
  k2 <- par[6]
  modelY <- k1 * exp(-0.5 * ((x - m1) / sd1) ^ 2)
  modelY <- modelY + k2 * exp(-0.5 * ((x - m2) / sd2) ^ 2)
  
  plot(x, y, xlim = c(.8, 1.2),main = scienceFrameFilters[[i]])
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

remove(masterFlats)

x <- histOfFlats[[1]]$mids
y <- histOfFlats[[1]]$counts
guess1 <- c(.98, 5e-3, 3.1e5, 1.04, 1e-3, 1.9e5)
opt <-
  optim(guess1,
        f2,
        method = "CG",
        control = list(reltol = 1e-11))
# pdf(
#   paste(p,
#         scienceFrameFilters[[1]],
#         ".pdf",
#         sep = ""),
#   width = 400,
#   height = 400
# )
yModel1 <- f(opt$par,1)
# dev.off()

x <- histOfFlats[[2]]$mids
y <- histOfFlats[[2]]$counts
guess2 <- c(1, 0.03, 2.8e6, 1.04, 1e-3, 2.8e6)
opt <-
  optim(guess2,
        f2,
        method = "CG",
        control = list(reltol = 1e-11))
# pdf(
#   paste(p,
#         scienceFrameFilters[[2]],
#         ".pdf",
#         sep = ""),
#   width = 400,
#   height = 400
# )
yModel2 <- f(opt$par,2)
# dev.off()
# 

x <- histOfFlats[[3]]$mids
y <- histOfFlats[[3]]$counts
guess3 <- c(1, 0.05, 1.5e6, 1, 0.01, 1.4e6)
opt <-
  optim(guess3,
        f2,
        method = "CG",
        control = list(reltol = 1e-11))
# pdf(
#   paste(p,
#         scienceFrameFilters[[3]],
#         ".pdf",
#         sep = ""),
#   width = 400,
#   height = 400
# )
yModel3 <- f(opt$par,3)
# dev.off()