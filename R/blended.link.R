#' Link function which equals log function between user-specified cutoffs, and a scaled
#'   user-specified link function for linear predictors below the first cutoff and above
#'   the second cutoff.
#'
#' This function produces a blended object for a specified cutover and link function
#' for use in binary regression using glm or glm2.
#'
#' @details
#' This function returns a link function which which equals log function for
#' values of the fitted probability between cutover[1] and cutover[2].
#' For values less than cutover[1] or greater than cutover[2], a linearly scaled user-specified link
#' is used (typically logit is recommended). The linear scaling is such that the new link is smooth.
#' This means that the exponentiated regression coefficients can be interpreted as relative risks
#' for individuals whose probability is between cutover[1] and cutover[2] both with and without
#' the risk factor. In practice this is usually the great majority of cases.
#' The function is not normally called directly. Instead, glm or glm2 should be
#' used (preferably the latter to avoid fitting problems), as per the example below.
#' @param otherlinkname Character string indicating the link function to be used below cutover[1]
#'  or above cutover[2]
#' @param cutover The link function switches smoothly from otherlinkname to log at cutover[1] and back to
#' otherlinkname at cutover[2]
#' @return An object of class "link-glm", with link name "blended".
#' @import glm2
#' 
#' @import stats
#' @examples
#' library(glm2)
#' example.binary.regression <- glm2(y~x1+x2 , data=loglogit.example,
#'     family = binomial(link=blended.linkobject(cutover=c(0.2,0.8),otherlinkname="logit")))
#' summary(example.binary.regression)
#' @export
blended.linkobject <- function(otherlinkname,cutover){
  # calculate linear scalers a1, a2, b1 and b2 first
  a1.plus.b1.logcutover <- make.link(otherlinkname)$linkfun(cutover[1])
  b1 <- cutover[1] / make.link(otherlinkname)$mu.eta(a1.plus.b1.logcutover)
  a1 <- a1.plus.b1.logcutover - b1*log(cutover[1])
  a2.plus.b2.logcutover <- make.link(otherlinkname)$linkfun(cutover[2])
  b2 <- cutover[2] / make.link(otherlinkname)$mu.eta(a2.plus.b2.logcutover)
  a2 <- a2.plus.b2.logcutover - b2*log(cutover[2])
  # now create and return link components
  linkfun <- function(mu){
    (mu>=cutover[1])*(mu<=cutover[2])*log(mu) + (mu<cutover[1])*(make.link(otherlinkname)$linkfun(mu)-a1)/b1 +
      (mu>cutover[2])*(make.link(otherlinkname)$linkfun(mu)-a2)/b2
  }
  linkinv <- function(eta){
    (eta>=log(cutover[1]))*(eta<=log(cutover[2]))*exp(eta) +
      (eta<log(cutover[1]))*make.link(otherlinkname)$linkinv(a1+b1*eta) +
      (eta>log(cutover[2]))*make.link(otherlinkname)$linkinv(a2+b2*eta)
  }
  mu.eta <- function(eta){
    (eta>=log(cutover[1]))*(eta<=log(cutover[2]))*exp(eta) +
      (eta<log(cutover[1]))*make.link(otherlinkname)$mu.eta(a1+b1*eta)*b1 +
      (eta>log(cutover[2]))*make.link(otherlinkname)$mu.eta(a2+b2*eta)*b2
  }
  valideta <- function(eta){
    rep(T,length(eta))
  }
  out <- list(  linkfun=linkfun , linkinv=linkinv , mu.eta=mu.eta , valideta=valideta ,
                name="blended" )
  class(out) <- "link-glm"
  out
}
