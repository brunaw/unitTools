#' dLSL
#'
#' Logit Skewed Logistic distribution. 
#'
#' @param y numeric. Variable of interest.
#' @param mu numeric. LSL's first parameter.
#' @param phi numeric. LSL's second parameter.
#' @param lambda numeric. LSL's third parameter.
#'
#' @return Density values.
#' @examples{
#' \donttest{
#'   curve(dLSL(y = x, mu = 0.5, phi = 0.5, lambda = 0.5))
#'}
#'}
#' @export


dLSL <- function(y, mu, phi, lambda){
  (gamlss.dist::dLO(log(y/(1-y)),mu,1/sqrt(phi))*
     (2*sqrt(phi)/(y-y^2))*gamlss.dist::pLO(lambda*log(y/(1-y)),
                               mu, 1/sqrt(phi)))
}