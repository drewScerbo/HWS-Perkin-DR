df <- read.csv('/Users/drewScerbo/Documents/AstronomyF17/hd219134_08082017_measurements2.csv')

names <- c('Source.Sky_C2','Source.Sky_C3','Source.Sky_C4',
           'Source.Sky_C5','Source.Sky_C6')
fitsCoeffs <- list()
for (n in names){
  source <- df[n]
  
  source <- log(df[n]/max(df[n]))*-2.5
  
  df2 <- data.frame(df['AIRMASS'],source)
  
  plot(df2)
  y <- .2
  lines(c(1,1.10),c(y,y))
  
  df2[n] <- df2[n]
  
  i <- which(df2[n] >= y)
  
  source <- df2[n][[1]][i]
  airmass <- df2["AIRMASS"][[1]][i]
  df3 <- data.frame("AIRMASS" = airmass,"SOURCE" = source)
  fit <- lm(SOURCE ~ AIRMASS,data = df3)
  predicted <- 10^(predict(fit)/(-2.5))
  # f <- function(x) {return(coefficients(fit)[2]*x + coefficients(fit)[1])}
  fitsCoeffs <- append(fitsCoeffs,c(coefficients(fit)[2],coefficients(fit)[1]))
  plot(df3)
  
  plot(airmass,predicted,col='red')
  predicted <- predict(fit,newdata = data.frame("AIRMASS" = seq(1,3,.01)))
  predicted <- 10^(predicted/(-2.5))
  plot(seq(1,3,.01),predicted,xlab = "Airmass",ylab = "Predicted Coefficient", type = 'l')
}

m <- mean(as.numeric(fitsCoeffs[seq(1,10,2)]))
b <- mean(as.numeric(fitsCoeffs[seq(2,10,2)]))
f <- function(x) {return(m*x + b)}
predicted <- 10^(f(seq(1,3,.01))/(-2.5))
plot(seq(1,3,.01),predicted,main = "Extinction Coefficient",xlab = "Airmass",ylab = "Predicted Coefficient", type = 'l')
