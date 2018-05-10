#' dLSN
#'
#' Logit Skewed Normal distribution. 
#'
#' @param y numeric. Variable of interest.
#' @param mu numeric. LSN's first parameter.
#' @param phi numeric. LSN's second parameter.
#' @param lambda numeric. LSN's third parameter.
#'
#' @return Density values.
#' @examples{
#' \donttest{
#'   curve(dLSN(y = x, mu = 0, phi = 0.5, lambda = 0.5))
#'}
#'}
#' @export

dLSN <- function(y, mu, phi, lambda){
  invisible(capture.output(gamlss.dist::gen.Family("SN1", type = "logit")))
  dlogitSN1(x = y, mu = mu, sigma = phi, nu = lambda)
}
