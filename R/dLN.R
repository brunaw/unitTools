#' dLN
#'
#' Logit Normal distribution. 
#'
#' @param y numeric. Variable of interest.
#' @param mu numeric. LN's first parameter.
#' @param phi numeric. LN's second parameter.
#'
#' @return Density values.
#' @examples{
#' \donttest{
#'   curve(dLN(y = x, mu = 0.5, phi = 0.5))
#'}
#'}
#' @export

dLN <- function(y, mu, phi){
  invisible(capture.output(gamlss.dist::gen.Family("NO", type="logit")))
  dlogitNO(x = y, mu = mu, sigma = phi)
}
