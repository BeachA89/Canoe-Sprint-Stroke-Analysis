# Define server logic to read selected file ----
server <- function(input, output) {
  output$projectplot2 <- renderPlot({
  headers <- names(read.csv(input$file1$datapath,skip=3, nrows=1))
  StrokeData <- read.csv(input$file1$datapath, header = F, skip=4, sep=",")
  colnames(StrokeData) = headers
  
  
  Acc = StrokeData['Acc.1.']
  Vel = StrokeData['V.Acc.']
  Vel2 = as.numeric(unlist(Vel))
  
  Time = StrokeData['Time']
  Time = as.numeric(unlist(Time))
  
  
  a <- peakdet(Vel2, .1, x = NULL)
  
  
  Peaks <- (a[["maxtab"]][["pos"]])
  Valleys <- (a[["mintab"]][["pos"]])
  
  output$projectplot <- renderPlot({
    plot(Vel2, xlab = "frame", ylab = "Velocity (m/s)", main = "Race Profile (Forward Velocity)")
  
  abline(v = Peaks, col="green")
  abline(v = Valleys, col="red")
  
  })
  
  
  StrokeAccdata <- list()
  for(i in 2:length(Valleys)){
    StrokeAccdata[[i-1]] <- StrokeData$Acc.1[Valleys[i-1]:Valleys[i]]
  }
  
  interp_output <- list()
  for (i in 1:length(StrokeAccdata)){
    xvalues = 1:length(StrokeAccdata[[i]])
    yvalues = StrokeAccdata[[i]]
    output_x_vals <- seq(1,length(xvalues),length.out=101)
    #
    # compute the interpolated values; this would be done for each input time series
    #
    #interp_output[[,i]]<- 
    interp_output[[i]] <- approx(x=xvalues, y=yvalues, xout=output_x_vals)$y
  }
  
  
  
  
  
  plot(interp_output[[1]], type = "o", col = "red", xlab = "% of stroke", ylab = "Acc(x)", main = "Stroke Acceleration Cycle")
  lines(interp_output[[2]], type = "o", col = "blue")
  lines(interp_output[[3]], type = "o", col = "green")
  })
  
  
}