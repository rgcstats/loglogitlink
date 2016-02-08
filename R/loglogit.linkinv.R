#' The inverse function of the loglogit link function.
#' 
#' The loglogit link function is a log function up to a specified cutover point, and a scaled logit function
#' above this point.
#' 
#' @param eta The linear predictors, a numeric vector of real values.
#' @param cutover The cutover.
#' @param stop Set to TRUE to open a browser window for debugging.
#' @return The expected values corresponding to the linear predictors.
#' @examples
#' loglogit.linkinv(eta=c(-2,-1,0,1,2),cutover=0.6)
#' @export
loglogit.linkinv <- function(eta,cutover,stop=F){
  if(stop) browser()
  if(cutover==0) out <- make.link("logit")$linkinv(eta)
  if(cutover==1) out <- exp(eta)
  if((cutover>0)&(cutover<1)){
    K <- log(cutover)
    a <- log(cutover/(1-cutover)) - K/(1-cutover)
    b <- 1 / (1-cutover)
    out <- exp(eta)
    if(sum(eta>K)>0) out[eta>K] <- make.link("logit")$linkinv(a+eta[eta>K]*b)
  }
  out
}

