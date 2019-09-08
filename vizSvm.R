vizSvm <- function(model, data, resolution = 100){
  #visualize decision boundary
  #data is labeled 2D dataset with labels as 3rd (last) column
  #grid resolution is 100 points per axis by default 

  cl <- data[,3]
  kk <- length(unique(cl))
  color <- c("blue","violetred")
  shape <- c(20,17) #dot, triangle
  
  #find x-, y- limits of data
  mn <- apply(data[,1:2],2,min)
  mx <- apply(data[,1:2],2,max)
  xmin <- mn[1]; ymin <- mn[2]
  xmax <- mx[1]; ymax <- mx[2]
  low <- min(xmin,ymin)-3 #add margin
  high <- max(xmax,ymax)+3 #add margin
  
  #plot data objects in blue
  plot(data[,1:2], col = color[1], pch = shape[as.integer(cl)+1L],
       xlim = c(low,high),ylim = c(low,high),xlab("Feature 1"),ylab("Feature 2"))
  
  #set-up grid
  xs <- seq(low, high, length.out = resolution)
  ys <- seq(low, high, length.out = resolution)
  g <- cbind(rep(xs, each=resolution), rep(ys, time = resolution))
  g <- as.data.frame(g)
  colnames(g) <- colnames(data[,1:2])
  
  L <- predict(model, g)
  if (class(L)=="logical") L <- (as.integer(!L)) #one-class SVM case
  else if (class(L)=="factor") L <- as.numeric(as.character(L)) #two-class SVM case   
  
  #plot grid points in colors reflecting their predicted labels
  points(g, col = color[as.integer(L)+1L], pch = ".")
  
  #add decision boundary contour
  z <- matrix(L+1L, nrow = resolution, byrow = TRUE)
  contour(xs, ys, z, add = TRUE, drawlabels = FALSE,
          lwd = 2, levels = (1:(kk-1))+.5)
}
