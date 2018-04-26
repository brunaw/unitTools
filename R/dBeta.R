#' dBeta
#'
#' Beta distribution. 
#'
#' @param y numeric. Variable of interest.
#' @param mu numeric. Beta's first parameter.
#' @param phi numeric. Beta's second parameter.
#'
#' @return Density values.
#' @examples{
#' \donttest{
#'   curve(dBeta(y = x, mu = 0.5, phi = 0.5))
#'}
#'}
#' @export

dBeta <-function(y, mu, phi){
  (1/beta(mu*phi, (1 - mu)*phi) * y^(mu*phi - 1) * 
     (1 - y)^((1 - mu)*phi - 1))
}
