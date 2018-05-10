#' dLSEP
#'
#' Logit Skewed Exponential Power distribution. 
#'
#' @param y numeric. Variable of interest.
#' @param mu numeric. LSEP's first parameter.
#' @param phi numeric. LSEP's second parameter.
#' @param lambda numeric. LSEP's third parameter.
#' @param tau numeric. LSEP's fourth parameter.
#'
#' @return Density values.
#' @examples{
#' \donttest{
#'   curve(dLSEP(y = x, mu = 0, phi = 0.5, lambda = 0.5, tau = 0.5))
#'}
#'}
#' @export

dLSEP <- function(y, mu, phi, lambda, tau){
  invisible(capture.output(gamlss.dist::gen.Family("SEP", type = "logit")))
  dlogitSEP(x = y, mu = mu, sigma = phi, nu = lambda, tau = tau)
}
