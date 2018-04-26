#' dLEP
#'
#' Logit Exponential Power distribution. 
#'
#' @param y numeric. Variable of interest.
#' @param mu numeric. LEP's first parameter.
#' @param phi numeric. LEP's second parameter.
#' @param nu numeric. LEP's third parameter.
#'
#' @return Density values.
#' @examples{
#' \donttest{
#'   curve(dLEP(y = x, mu = 0, phi = 0.5, nu = 0.5))
#'}
#'}
#' @export

invisible(capture.output(gamlss.dist::gen.Family("SEP", type = "logit")))

dLEP <- function(y, mu, phi, nu){
  dlogitSEP(x = y, mu = mu, sigma = phi, nu=0 , tau = nu)
}
