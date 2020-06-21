viz <- function(data, xlimits, ylimits, resolution = 100, classifier, ...){
  #visualize decision boundary
  #data: labeled 2D dataset with labels as 3rd (last) column
  #grid resolution is 100 points per axis by default 
  
  cl <- data[,3] #class labels; normally just training objects with label '0'
  kk <- length(unique(cl)) #number of classes
  color <- c("blue","violetred") 
  shape <- c(20,17) #dot, triangle
  
  # #find x-, y- limits of data
  # mn <- apply(data[,1:2],2,min)
  # mx <- apply(data[,1:2],2,max)
  # xmin <- mn[1]; ymin <- mn[2]
  # xmax <- mx[1]; ymax <- mx[2]
  # low <- min(xmin,ymin)-3 #add margin
  # high <- max(xmax,ymax)+3 #add margin
  
  #plot data objects in blue
  plot(data[,1:2],col = color[1], pch = shape[as.integer(cl)+1L],
       xlim = xlimits, ylim = ylimits, xlab = "Feature 1" , ylab = "Feature 2")
  
  #set-up grid
  xs <- seq(xlimits[1], xlimits[2], length.out = resolution)
  ys <- seq(ylimits[1], ylimits[2], length.out = resolution)
  g <- cbind(rep(xs, each=resolution), rep(ys, time = resolution))
  g <- cbind(g, rep(0,nrow(g))) #add dummy labels
  g <- as.data.frame(g)
  colnames(g) <- colnames(data)
  
  #call classifier to predict grid point labels
  if (classifier == "gauss")  out <- gaussocc(data,g,...)
  else if (classifier == "parzen") out <- parzenocc(data,g,...)
  else if (classifier == "knn") out <- knnocc(data,g,...)
  else if (classifier == "rknnd") out <- rknndocc(data,g,...)
#  else if (classifier == "rknnd2") out <- rknndocc2(data,g,...)
  else if (classifier == "rdc") out <- rdocc(data,g,...)
  else if (classifier == "rdocc_scalable") out <- rdocc_scalable(data,g,...)
  else if (classifier == "mocc_scalable") out <- mocc_scalable(data,g,...)
  else if (classifier == "kmeans") out <- kmeansocc(data,g,...)
  else if (classifier == "som") out <- somocc(data,g,...)
  else if (classifier == "gmm") out <- gmmocc(data,g,...)
  else if (classifier == "iForest") out <- iForest(data,g,...)
  else if (classifier == "lof") out <- lof_wrapper(data,g,...)
  else message("Classifier specified is unknown!")
  L <- out[[1]]
  #  P <- o[[2]]
  
  #plot grid points in colors reflecting their predicted labels
  points(g, col = color[as.integer(L)+1L], pch = ".")
  
  #add decision boundary contour
  z <- matrix(L+1L, nrow = resolution, byrow = TRUE)
  contour(xs, ys, z, add = TRUE, drawlabels = FALSE,
          lwd = 2, levels = (1:(kk-1))+.5)
}

