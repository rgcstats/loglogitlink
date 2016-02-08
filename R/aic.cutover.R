#' The AIC for a given generalized linear model with log-logit link.
#' 
#' The loglogit link function is a log function up to a specified cutover point, and a scaled logit function
#' above this point.
#' 
#' @param cutover The cutover.
#' @param formula The regression formula, see help(glm).
#' @param ... Other arguments to pass to glm2 (e.g. data).
#' @param stop Set to TRUE to open a browser window for debugging.
#' @return The Akaike Information Criterion (AIC) for the model (a single numeric value).
#' @examples
#' aic.cutover(0.6,y~x1+x2 , data=loglogit.example)
#' aic.cutover(0.7,y~x1+x2 , data=loglogit.example)
#' aic.cutover(0.8,y~x1+x2 , data=loglogit.example)
#' aic.cutover(0.9,y~x1+x2 , data=loglogit.example)
#' @export
aic.cutover <- function(cutover,formula,...,stop=F){
  if(stop) browser()
  out <- rep(NA,length(cutover))
  for(k in c(1:length(cutover))){
    out[k] <- glm2(family=binomial(link=loglogit.linkobject(cutover[k])),formula=formula,...)$aic
  }
  out
}


