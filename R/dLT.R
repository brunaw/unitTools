#' dLT
#'
#' Logit t distribution. 
#'
#' @param y numeric. Variable of interest.
#' @param mu numeric. LST's first parameter.
#' @param phi numeric. LST's second parameter.
#' @param nu numeric. LST's third parameter.
#'
#' @return Density values.
#' @examples{
#' \donttest{
#'   curve(dLT(y = x, mu = 0, phi = 0.5, nu = 0.5))
#'}
#'}
#' @export

invisible(capture.output(gamlss.dist::gen.Family("TF", type = "logit")))

dLT <- function(y, mu, phi, nu){
  dlogitTF(x = y, mu = mu, sigma = phi, nu = nu)
}
