#' Link function which equals log function up to a cutover point then a scaled cloglog function
#'
#' This function produces a blended log-logist link object for a specified cutover
#' for use in binary regression using glm or glm2.
#'
#' @details
#' This function returns a link function which which equals log function for
#' values less than or equal to a specified cutover point and a scaled cloglog
#' function for values above the cutover. This means that the
#' exponentiated regression coefficients can be interpreted as relative risks
#' for individuals whose probability is below the cutover both with and without
#' the risk factor. In practice this is usually the great majority of cases.
#' The function is not normally called directly. Instead, glm or glm2 should be
#' used (preferably the latter to avoid fitting problems), as per the example below.
#' @param cutover The value at which the link function switches smoothly from log to scaled logit.
#' @return An object of class "link-glm", with link name "logcloglog".
#' @import glm2
#' 
#' @import stats
#' @examples
#' library(glm2)
#' example.cloglog.regression <- glm2(y~x1+x2 , data=loglogit.example,
#'     family = binomial(link=logcloglog.linkobject(0.8)))
#' summary(example.cloglog.regression)
#' @export
logcloglog.linkobject <- function(cutover){
  # calculate a and b first
  a.plus.b.logcutover <- make.link("cloglog")$linkfun(cutover)
  b <- cutover / make.link("cloglog")$mu.eta(a.plus.b.logcutover)
  a <- a.plus.b.logcutover - b*log(cutover)
  # now create and return link components
  linkfun <- function(mu){
    (mu<=cutover)*log(mu) + (mu>cutover)*(make.link("cloglog")$linkfun(mu)-a)/b
  }
  linkinv <- function(eta){
    (eta<=log(cutover))*exp(eta) + (eta>log(cutover))*make.link("cloglog")$linkinv(a+b*eta)
  }
  mu.eta <- function(eta){
    (eta<=log(cutover))*exp(eta) + (eta>log(cutover))*make.link("cloglog")$mu.eta(a+b*eta)*b
  }
  valideta <- function(eta){
    rep(T,length(eta))
  }
  out <- list(  linkfun=linkfun , linkinv=linkinv , mu.eta=mu.eta , valideta=valideta ,
                name="logcloglog" )
  class(out) <- "link-glm"
  out
}
