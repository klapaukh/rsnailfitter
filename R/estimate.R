#' Calculate W based on a measured increase in the radius of a snail. Note that
#' this assumes that the measurements are of the centroid of the snail shell.
#' 
#' @param theta The angle across which the measuring has been done in radians
#' @param r_theta The distance from the coiling axis at the end of the measured
#'        section
#' @param r_0 The distance from the coiling axis the start of the measured 
#'        section
#' @export
estimateW <- function(theta, r_theta, r_0) {
  (r_theta/r_0) ^ ( (2*pi) / theta )
}

#' Calculate T based on a measured section of snail (where W has already been
#' estimated). This uses two points measured on a snail. Note that this assumes
#' that the measurements are of the centroid of the snail shell.
#'
#' @param theta The angle difference between the two measured points in radians
#' @param y_theta The difference in y between the two measured points
#' @param r_0 The distance from the coilind axis at the first point
#' @param W The calculated W value for the spiral
#' @export
estimateT <- function(theta, y_theta, r_0, W){
  y_theta / ( r_0 * ( (W ^ (theta / (2*pi))) - 1 ))
}

#' Estimate W and T based on every pair of points. Since W and T can be
#' estimated from any sub part of the shell, this estimates W and T based on
#' every possible point being an end point. For each end point, every theta
#' value smaller than it acts as a starting point. 
#'
#' The data frame that is used in this method must have three columns named
#' 'theta', 'r', and 'y'. The theta column must be measured in radians. The
#' other measurements can be in any set of units as long as they are all the
#' same. Note that this method does not work with data.table due to some scoping
#' issue.
#' 
#' @param dt The data frame containing snail measurements
#' @return A data.frame containing W and T estimates for every pair of thetas. 
#'         In each row, theta is the start theta and theta_end is the end theta.
#'         All other columns are preserved.
#' @export
#' @examples
#' estimateOverTime(snail)
estimateOverTime <- function(dt){
  if("data.table" %in% class(dt)){
    warning("May not work with a data.table")
  }
  allResults = list()
  for(i in 1:nrow(dt)){
    current = dt[i,]
    others = dplyr::filter(dt, theta < current$theta)
    others %<>% 
      dplyr::mutate(
             w = estimateW(current$theta - theta, current$r, r),
             t = estimateT(current$theta - theta, current$y - y, r, w),
             theta_end = rep(current$theta,n())
             )
    allResults = c(allResults,list(others))
  }
  return(data.table::rbindlist(allResults))
}
