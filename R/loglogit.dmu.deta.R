#' The derivative of the inverse function of the loglogit link function.
#' 
#' The loglogit link function is a log function up to a specified cutover point, and a scaled logit function
#' above this point.
#' 
#' @param eta The linear predictors, a numeric vector of real values.
#' @param cutover The cutover.
#' @param stop Set to TRUE to open a browser window for debugging.
#' @return The derivatives of the link function calculated at the supplied linear predictor values.
#' @examples
#' loglogit.dmu.deta(eta=c(-2,-1,0,1,2),cutover=0.6)
#' @export
loglogit.dmu.deta <- function(eta,cutover,stop=F){
  if(stop) browser()
  if(cutover==0){
    mu.logistic <- make.link("logit")$linkinv(eta)
    out <- mu.logistic*(1-mu.logistic)
  }
  if(cutover==1) out <- exp(eta)
  if((cutover>0)&(cutover<1)){
    K <- log(cutover)
    a <- log(cutover/(1-cutover)) - K/(1-cutover)
    b <- 1 / (1-cutover)
    mu.logistic <- make.link("logit")$linkinv(a+eta*b)
    out <- exp(eta)
    if(sum(eta>K)>0) out[eta>K] <- b * make.link("logit")$mu.eta(a+eta[eta>K]*b)
  }
  out
}

