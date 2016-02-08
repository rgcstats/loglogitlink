#' Link function which equals log function up to a cutover point then a scaled logit function
#'
#' This function produces a blended log-logist link object for a specified cutover
#' for use in binary regression using glm or glm2.
#'
#' @details
#' This function returns a link function which which equals log function for
#' values less than or equal to a specified cutover point and a scaled logit
#' function for values above the cutover. This means that the
#' exponentiated regression coefficients can be interpreted as relative risks
#' for individuals whose probability is below the cutover both with and without
#' the risk factor. In practice this is usually the great majority of cases.
#' The function is not normally called directly. Instead, glm or glm2 should be
#' used (preferably the latter to avoid fitting problems), as per the example below.
#' @param cutover The value at which the link function switches smoothly from log to scaled logit.
#' @param stop Set to TRUE to open a browser window for debugging.
#' @return An object of class "link-glm", with link name "loglogit".
#' @import glm2
#' @import stats
#' @examples
#' library(glm2)
#' example.loglogit.regression <- glm2(y~x1+x2 , data=loglogit.example,
#'     family = binomial(link=loglogit.linkobject(0.8)))
#' summary(example.loglogit.regression)
#' @export
loglogit.linkobject <- function(cutover,stop=F){
  if(stop) browser()
  out <- list(  linkfun=eval(parse(text=paste("function(mu){ loglogit.linkfun(mu,",cutover,") }",sep=""))),
                linkinv=eval(parse(text=paste("function(eta){ loglogit.linkinv(eta,",cutover,") }",sep=""))),
                mu.eta=eval(parse(text=paste("function(eta){ loglogit.dmu.deta(eta,",cutover,") }",sep=""))),
                valideta=loglogit.valideta ,
                name="loglogit" )
  class(out) <- "link-glm"
  out
}
