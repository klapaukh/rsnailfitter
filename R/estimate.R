estimateW <- function(theta, r_theta, r_0) {
  (r_theta/r_0) ^ ( (2*pi) / theta )
}

estimateT <- function(theta, y_theta, r_0, W){
  y_theta / ( r_0 * ( (W ^ (theta / (2*pi))) - 1 ))
}
