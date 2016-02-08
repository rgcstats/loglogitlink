#' The valideta item of the loglogit link object
#' 
#' For internal use only.
#' @param eta The linear predictors, a numeric vector of real values.
#' @return  a logical vector of the same length as eta, with all values equal to TRUE
loglogit.valideta <- function(eta){
  rep(T,length(eta))
}
