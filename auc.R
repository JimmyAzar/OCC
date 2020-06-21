auc <- function(x,y){
  #computes area under ROC curve by summing areas of trapezoids 
  dx <- diff(x)
  base1 <- y[1:(length(y)-1)] #base 1 of trapezium
  base2 <- y[2:length(y)]
  trapezium_areas <- (base1+base2)*dx/2
  auc <- sum(trapezium_areas)
  return(auc)
}