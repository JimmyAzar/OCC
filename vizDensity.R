vizDensity <- function(data, h, fracrej, resolution = 100){
  #Visualize Parzen density estimation plot over 2D scatterplot 
  
  if (!require("plotly")) install.packages("plotly") 
  library("plotly")
  
  #find data range
  mn <- apply(data[,1:2],2,min)
  mx <- apply(data[,1:2],2,max)
  xmin <- mn[1]; ymin <- mn[2]
  xmax <- mx[1]; ymax <- mx[2]
  low <- min(xmin,ymin)-3 #add margin
  high <- max(xmax,ymax)+3 #add margin
  
  #grid
  xs <- seq(low, high, length.out = resolution)
  ys <- seq(low, high, length.out = resolution)
  g <- cbind(rep(xs, each=resolution), rep(ys, time = resolution))
  g <- cbind(g, rep(0,nrow(g))) #add dummy labels
  g <- as.data.frame(g)
  colnames(g) <- colnames(data)
  
  out <- parzenocc(data,g,h,fracrej) #label each grid point in the domain and obtain the probability-outputs
  L <- out[[1]]
  P <- out[[2]]
  
  #use the normalized probability-output, P, to create a 3D plot.
  plot_ly(x = xs, y = ys, z = matrix(P,nrow=length(xs))) %>%  add_surface(
    contours = list(
      z = list(
        show=TRUE,
        usecolormap=TRUE,
        highlightcolor="#ff0000",
        project=list(z=TRUE)
      )
    )
  ) %>%
    layout(
      scene = list(
        camera=list(
          eye = list(x=1.87, y=0.88, z=-0.64)
        )
      )
    )
}
