#' dLL
#'
#' Logit Logistic distribution. 
#'
#' @param y numeric. Variable of interest.
#' @param mu numeric. LL's first parameter.
#' @param phi numeric. LL's second parameter.
#'
#' @return Density values.
#' @examples{
#' \donttest{
#'   curve(dLL(y = x, mu = 0, phi = 0.5))
#'}
#'}
#' @export

dLL <- function(y, mu, phi){
  invisible(capture.output(gamlss.dist::gen.Family("LO", type="logit")))
  dlogitLO(x = y, mu = mu, sigma = phi)
}
