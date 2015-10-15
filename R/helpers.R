#' Convert degrees to radians
#'
#' @param deg An angle in degrees
#' @return The same angle in radians
#' @export
degToRad <- function(deg){
    pi * deg / 180
}

#' Convert radians to degress
#'
#' @param deg An angle in radians
#' @return The same angle in degrees
#' @export
radToDeg <- function(rad){
    180 * rad / pi
}
