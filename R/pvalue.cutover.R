#' Test null hypothesis that cutover is equal to a particular value
#' 
#' Tests the null hypothesis that the cutover is equal to a supplied value for a given dataset
#' and regression formula
#' 
#' @details 
#' The p-value is based on the asymptotic chi-square distribution of 
#' the likelihood ratio test, and is not reliable for hypothesized cutovers
#' near the boundaries of the parameter space (i.e. close to 0 or 1)
#' 
#' @param cutover The hypothesised value of the cutover
#' @param formula The regression formula, see help(glm).
#' @param ... Arguments to pass to optim.cutover and glm2, would typically consist of data, 
#' a data frame containing the data to be analysed.
#' @param stop Set to TRUE to open a browser window for debugging.
#'  
#' @return The p-value for the hypothesis test.
#' @examples
#' pvalue.cutover(cutover=0.8,y~x1+x2 , data=loglogit.example )
#' @export
pvalue.cutover <- function(cutover,formula,...,stop=F){
  if(stop) browser()
  aic.Kopt <- optim.cutover(formula,...)$objective
  tempdata <- data
  glm.results.K0 <- glm2(formula=formula,...,
                         family=binomial(link=loglogit.linkobject(cutover)))
  observed.deviance.K0 <- glm.results.K0$aic - aic.Kopt
  pchisq(observed.deviance.K0,df=1,lower.tail=F)
}
