library(shiny, data.table)



ui <- fluidPage(
  h1("Perkin Observatory Exposure Time Calculator"),
  br(),
  tags$ul(
    tags$li(strong("Enter two values for S/N, Magnitude, or Exposure Time,
                   leaving the desired value 0")),
    tags$li(
      strong("Select the telescope, filter, moon phase, and airmass."),
    tags$li(strong("Click Calculate")))
  ),
  tags$style(HTML("hr {border-top: 1px solid #000000;}")),
  hr(),
  
  wellPanel(
    h4("Input Values"),
    fluidRow(
      column(3, "Input two out of three of the following"),
      column(
        3,
        numericInput(
          "signal_noiseIN",
          "S/N:",
          0,
          min = 0,
          max = 10000,
          step = 10,
          width = "100px"
        )
      ),
      column(
        3,
        numericInput(
          "magnitudeIN",
          "Magnitude:",
          0,
          min = -50,
          max = 100,
          step = 1,
          width = "100px"
        )
      ),
      column(
        3,
        numericInput(
          "exp_timeIN",
          "Exp Time:",
          0,
          min = 0,
          max = 100,
          step = 1,
          width = "100px"
        )
      )
    ),
    hr(),
    fluidRow(column(3),
             column(
               3,
               selectInput(
                 inputId = "tele_inst",
                 "Telescope/Instrument:",
                 c("Perkin/17in"),
                 width = "200px"
               )
             ),
             column(
               3, selectInput(
                 inputId = "filter",
                 "Filter:",
                 c("g", "r", "i", "Y", "z"),
                 width = "100px"
               )
             )),
    fluidRow(column(3),
             column(
               3,
               selectInput(
                 inputId = "moon_phase",
                 "Moon phase:",
                 c("New", "Half", "Full"),
                 width = "100px"
               )
             ),
             column(
               3,
               numericInput(
                 "airmass",
                 "Airmass:",
                 1.3,
                 min = 1,
                 max = 3,
                 step = 0.1,
                 width = "100px"
               )
             )),
    fluidRow(
      column(3),
      column(2, actionButton("calc", "Calculate")),
      column(3, "Press the button to display updated values")
    ),
    hr(),
    h4("Calculated Values"),
    fluidRow(
      column(3),
      column(2, strong("S/N: ")),
      column(2, strong("Magnitude: ")),
      column(2, strong("Exp Time: ")),
      column(2, strong("PkDN: "))
    ),
    fluidRow(
      column(3),
      column(2, verbatimTextOutput("signal_noiseOUT", TRUE)),
      column(2, verbatimTextOutput("magnitudeOUT", TRUE)),
      column(2, verbatimTextOutput("exp_timeOUT", TRUE)),
      column(2, verbatimTextOutput("peak", TRUE))
    )
  ),
  tags$button(
    id = 'close',
    type = "button",
    class = "btn action-button",
    onclick = "setTimeout(function(){window.close();},500);",
    # close the browser
    "Stop App"
  )
)

server <- function(input, output) {
  
  observeEvent(input$calc, {
    signal <- input$signal_noiseIN
    mag <- input$magnitudeIN
    expTime <- input$exp_timeIN
    tele <- input$tele_inst
    filter <- input$filter
    moon <- input$moon_phase
    airmass <- input$airmass
    
    gain <- 0.37
    npix <- pi*100
    # p_b <- pi*(30^2 - 20^2)
    p_b <- 3352 * 2532 # we used the whole image as the background rate
    a_b <- 1 + npix/p_b
    readN <- 9.3
    
    # New = 20 processed images taken on Oct 18, 2017, Completely New moon
    # Full = 11 processed images, taken on Oct 5, 2017
    # Half2.0 = 19 processed images, taken on Feb 3, 2017 (1 day before First Quarter, 43%)
    backgroundRate <- switch(moon,"New" = 0.1096789,"Half" = 1.948203,"Full" = 6.01724)
    
    # dark rate was calculated from the median of 1200 sec dark / 1200
    darkRate <- 0.003444444*gain
    
    # To recalibrate, switch the following numbers for the given filter
    # These numbers are the rate
    super_mag <- switch(filter,"g" = 13.5362, "r" = 0, "i" = 0, "Y" = 0, "z" = 0) # we only have data for g filter
    super_counts <- switch(filter,"g" = 900.983, "r" = 0, "i" = 0, "Y" = 0, "z" = 0)
    
    wavelength <- switch(filter,"g" = 4770, "r" = 6231, "i" = 7625, "Y" = 0, "z" = 9134)
    extCo <- 0.14
    
    if (!signal){
      star_counts <- (10^((mag - super_mag + extCo*airmass)/(-2.5)))*super_counts
      
      signal <- star_counts*expTime
      denom <- (star_counts + npix*a_b*(backgroundRate + darkRate))*expTime
      denom <- sqrt(denom + npix*a_b*(readN^2))
      
      signal <- signal/denom
      remove(denom)
      
    } else if (!mag){
      # backgroundRate <- 1036.333*gain/expTime
      # mag <- 2.5*log(expTime) - 2.5*log(star_counts - backgroundRate)
      
      a <- npix*a_b*(backgroundRate - darkRate)
      b <- npix*a_b*(readN^2)
      
      A <- expTime^2
      B <- -(signal^2)*expTime
      C <- -(signal^2)*(a*expTime + b)
      
      star_counts <- B + sqrt(B^2 - 4*A*C)
      star_counts <- star_counts/(2*A)
      
      mag <- super_mag - 2.5*log10(star_counts/super_counts) - extCo*airmass
      
      remove(a,b,A,B,C)
      
    } else if (!expTime){
      star_counts <- (10^((mag - super_mag + extCo*airmass)/(-2.5)))*super_counts
      
      A <- (star_counts^2) / signal^2
      B <- star_counts + npix*a_b*(backgroundRate + darkRate)
      C <- npix*a_b*readN^2
      
      expTime <- B + sqrt(B^2 + 4*A*C)
      expTime <- expTime/(2*A)
      remove(A,B,C)
    } 
    
    # star_counts <- (10^((mag - super_mag + extCo*airmass)/(-2.5)))*super_counts*expTime
    # fwhm <- 2.355*sd
    # peak <- star_counts/(1.13*(fwhm^2))
    peak <- 1
    
    output$signal_noiseOUT <- renderText(signal)
    output$magnitudeOUT <- renderText(mag)
    output$exp_timeOUT <- renderText(expTime)
    output$peak <- renderText(peak)
  })
  observe({
    if (input$close > 0)
      stopApp()
  })
}

shinyApp(ui = ui, server = server)