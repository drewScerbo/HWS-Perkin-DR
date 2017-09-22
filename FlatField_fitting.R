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

# xmin <- .Machine$integer.max # start at maximum possible value
# xmax <- .Machine$integer.min # start at minimum possible value
# for (i in length(masterFlats):1) {
#   xmin <- min(xmin, c(masterFlats[[i]]))
#   xmax <- max(xmax, c(masterFlats[[i]]))
# }


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

counts <- list()
stDevs <- list()
summarys <- list()

for (i in length(histOfFlats)) {
  counts[[i]] <- histOfFlats[[i]]$counts
  summarys[[i]] <- summary(counts[[i]])
  stDevs[[i]] <- sd(counts[[i]])
}

path <-
  "C:/Users/drews/OneDrive/Documents/Hobart 16-17/Astronomy"

file.create(path)
fileSummary <- file(path)
titles <- c("Min:","1st Q:","Med:","Mean:","3rd Q:","Max:")

for (i in length(histOfFlats)) {
  # writeLines(scienceFrameFilters[[i]],fileSummary)
  print(paste(scienceFrameFilters[[i]],fileSummary))
  for (j in range(1,6)) {
    # writeLines(paste(titles[[j]],summarys[[i]][[j]]),fileSummary)
    print(paste(titles[[j]],summarys[[i]][[j]]))
  }
  # writeLines(paste("St. Dev:",stDevs[[i]]),fileSummary)
  print(paste("St. Dev:",stDevs[[i]]))
  # writeLines("",fileSummary) # write a blank line to seperate flats
}
# close(fileSummary)
