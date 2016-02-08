#' Maximum likelihood estimator (MLE) of the cutover
#' 
#' The value of the cutover which maximises the likelihood of the GLM
#' with the log-logit link for a given regression formula and dataset
#' 
#' @details 
#' This function is primarily for internal use and would not normally be used directly.
#' In general the cutover should be a large value (e.g. 0.6-0.8) so that exponentiated coefficients
#' can be interpreted as odds ratios for the majority of subjects. The cutover value should be
#' consistent with the data but not the MLE.
#' 
#' @param formula The regression formula, see help(glm).
#' @param stop Set to TRUE to open a browser window for debugging.
#' @param ... additional named arguments to be passed to optimize or to aic.cutover, typically including data
#'            (which would be the data.frame containing the data to be analysed).
#' @return The MLE of the cutover.
#' @examples
#' optim.cutover(y~x1+x2 , data=loglogit.example)
#' @export
optim.cutover <- function(formula,...,stop=F){
  if(stop) browser()
  optresults <- optimise(f=aic.cutover,interval=c(0,0.999),formula=formula,...)
  optresults
}
