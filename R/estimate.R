#' @importFrom magrittr "%<>%"

estimateW <- function(theta, r_theta, r_0) {
  (r_theta/r_0) ^ ( (2*pi) / theta )
}

estimateT <- function(theta, y_theta, r_0, W){
  y_theta / ( r_0 * ( (W ^ (theta / (2*pi))) - 1 ))
}

#' Estimate W and T based on every pair of points.
#' 
#' @param dt The data frame containing snail measurements
#' @return A list of data.frames containing W and T estimates for every pair of
#'         thetas.
#' @examples
#' estimateOverTime(snail)
estimateOverTime <- function(dt){
  allResults = list()
  for(i in 1:nrow(dt)){
    current = dt[i,]
    others = dplyr::filter(dt, theta < current$theta)
    others %<>% 
      dplyr::mutate(
             w = estimateW(current$theta - theta, current$r, r),
             t = estimateT(current$theta - theta, current$y - y, r, w)
             )
    thisOne = list(others)
    names(thisOne) = as.character(current$theta)
    allResults = c(allResults,thisOne)
  }
  return(allResults)
}
