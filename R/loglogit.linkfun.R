#' The loglogit link function.
#' 
#' The loglogit link function is a log function up to a specified cutover point, and a scaled logit function
#' above this point.
#' 
#' @param mu The expected value, lying between 0 and 1.
#' @param cutover The cutover.
#' @param stop Set to TRUE to open a browser window for debugging.
#' @return The linear predictor corresponding to mu.
#' @examples
#' loglogit.linkfun(mu=c(0,0.25,0.5,0.75,1),cutover=0.6)
#' @export
loglogit.linkfun <- function(mu,cutover,stop=F){
  if(stop) browser()
  if(cutover==0) out <- make.link("logit")$linkfun(mu)
  if(cutover==1) out <- log(mu)
  if((cutover>0)&(cutover<1)){
    out <- log(mu)
    K <- log(cutover)
    a <- log(cutover/(1-cutover)) - K/(1-cutover)
    b <- 1 / (1-cutover)
    if(sum(mu>cutover)>0) out[mu>cutover] <-( make.link("logit")$linkfun(mu[mu>cutover]) - a ) / b
  }
  out
}
