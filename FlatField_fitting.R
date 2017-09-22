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

counts <- list()
stDevs <- list()
summarys <- list()

for (i in 1:length(masterFlats)) {
  summarys[[i]] <- summary(masterFlats[[i]])
  stDevs[[i]] <- sd(masterFlats[[i]])
}

  # "C:/Users/drews/OneDrive/Documents/Hobart 16-17/Astronomy"
path <- "/Users/drewScerbo/Desktop/20170911/FlatFieldSummary.txt"

file.create(path)
fileSummary <- file(path)
titles <- c("Min:","1st Q:","Med:","Mean:","3rd Q:","Max:")
# cat("FlatFlield Summary;",file = fileSummary)
lines <- "FlatFlield Summary;\n"

for (i in 1:length(masterFlats)) {
  lines <- paste(lines,scienceFrameFilters[[i]],sep = "\n")
  for (j in 1:6) {
    lines <- paste(lines,summarys[[i]][[j]],sep = "\n")
  }
  lines <- paste(lines,paste("St. Dev:",stDevs[[i]]),"",sep = "\n")
}
writeLines(lines,con = fileSummary)
close(fileSummary)
