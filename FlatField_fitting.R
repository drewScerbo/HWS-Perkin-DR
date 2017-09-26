# FlatField fitting to a histogram
# Must run Filter_Image_Processing_Script.R to get masterflats

colors <-
  list(rgb(1, 0, 0, 0.5),
       rgb(0, 0, 1, 0.5),
       rgb(0, 1, 0, 0.5),
       rgb(1, 0, 1, 0.5),
       rgb(1, 1, 0, 0.5))

masterFlats <- list(masterGFlat,
                    masterRFlat,
                    masterIFlat,
                    masterYFlat,
                    masterZFlat)

scienceFrameFilters <- list("g Filter",
                            "r Filter",
                            "i Filter",
                            "y Filter",
                            "z Filter")

for (i in length(masterFlats):1) {
  if (is.nan(masterFlats[[i]][xNumber / 2, yNumber / 2])) {
    masterFlats[[i]] <- NULL
    colors[[i]] <- NULL
    scienceFrameFilters[[i]] <- NULL
  }
}

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

legend("topright",
       c(unlist(scienceFrameFilters)),
       col = c(unlist(colors)),
       lwd = 5)

percentageOut <- list()
path <-
  "C:/Users/drews/OneDrive/Documents/Hobart 16-17/Astronomy/FlatFieldSummary.txt"
# path <- "/Users/drewScerbo/Desktop/20170911/FlatFieldSummary.txt"

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
              ~ histOfFlats[[i]]$mids[indexes])
  lines(histOfFlats[[i]]$mids[indexes],
        predict(lo),
        type = "l",
        col = colors[[i]],
        lwd = 7)
  
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
