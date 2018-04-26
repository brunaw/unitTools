#' dgUnit
#'
#' Unit Gamma distribution. 
#'
#' @param y numeric. Variable of interest.
#' @param mu numeric. Unit Gamma's first parameter.
#' @param phi numeric. Unit Gamma's second parameter.
#'
#' @return Density values.
#' @examples{
#' \donttest{
#'   curve(dgUnit(y = x, mu = 0.5, phi = 0.5))
#'}
#'}
#' @export

dgUnit <-function(y, mu, phi){
  (((((mu^(1/phi))/(1 - mu^(1/phi)))^phi)/gamma(phi)) 
   * y^(((mu^(1/phi))/(1 - mu^(1/phi)))-1)*
     (log(1/y))^(phi - 1))
}
