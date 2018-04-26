#' dLST
#'
#' Logit Skewed t distribution. 
#'
#' @param y numeric. Variable of interest.
#' @param mu numeric. LST's first parameter.
#' @param phi numeric. LST's second parameter.
#' @param lambda numeric. LST's third parameter.
#' @param tau numeric. LST's fourth parameter.
#'
#' @return Density values.
#' @examples{
#' \donttest{
#'   curve(dLST(y = x, mu = 0, phi = 0.5, lambda = 0.5, tau = 0.5))
#'}
#'}
#' @export

invisible(capture.output(gamlss.dist::gen.Family("ST1", type = "logit")))

dLST <- function(y, mu, phi, lambda, tau){
  dlogitST1(x = y, mu = mu, sigma = phi, nu = lambda, tau = tau)
}
