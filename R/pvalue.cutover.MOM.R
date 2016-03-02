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
#' @param B Number of boostraps to estimate the first and second moments of the
#' likelihood ratio test statistic.
#' @param data Data frame.
#' @param stop Set to TRUE to open a browser window for debugging.
#'  
#' @return The p-value for the hypothesis test.
#' @examples
#' pvalue.cutover(cutover=0.8,y~x1+x2 , data=loglogit.example )
#' @export
pvalue.cutover.MOM <- function(cutover,formula,B=30,data,stop=F){
  if(stop) browser()
  aic.Kopt <- optim.cutover(formula,data)$objective
  tempdata <- data
  glm.results.K0 <- glm2(formula=formula,data=data,
                         family=binomial(link=loglogit.linkobject(cutover)))
  observed.deviance.K0 <- glm.results.K0$aic - aic.Kopt
  # simulate from null model
  sim.deviances <- rep(NA,B)
  for(b in c(1:B)){
    tempdata$ysim <- simulate(glm.results.K0)$sim_1
    simformula <- as.formula(paste("ysim~",formula[3]))
    aic.Kopt.boot <- optim.cutover(simformula,data=tempdata)$objective
    glm.results.K0.boot <- glm2(formula=simformula,data=tempdata,
                                family=binomial(link=loglogit.linkobject(cutover)))
    sim.deviances[b] <- max( 0 , glm.results.K0.boot$aic - aic.Kopt.boot )
  }
  expected.value.deviance <- mean(sim.deviances)
  var.deviance <- var(sim.deviances)
  scale <- 0.5*var.deviance/expected.value.deviance
  df <- 2*expected.value.deviance^2/var.deviance
  pchisq(observed.deviance.K0/scale,df=df,lower.tail=F)
}
