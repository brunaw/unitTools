#' quality
#'
#' Calculates the AIC and BIC for the chosen model.
#'
#' @param ll numeric. The loglikehood results. 
#' @param n_par numeric. The number of parameters. 
#'
#' @return AIC and BIC values
#' @examples{
#' \donttest{
#'   x <- seq(0.00001, 0.9999, l = 50)
#'   ll <- loglike(par = c(0.5, 0.5), model = "beta", x = x, sum = "yes")
#'   quality(ll, n_par = 2)
#'}
#'}
#' @export
quality <- function(ll, n_par){
  AIC <- 2*(n_par - sum(ll))
  BIC <- 2*(n_par/2 - sum(ll))
  return(c(AIC, BIC))
}
