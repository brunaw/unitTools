#' dKum
#'
#' Kumaraswamy distribution. 
#'
#' @param y numeric. Variable of interest.
#' @param mu numeric. Kumaraswamy's first parameter.
#' @param phi numeric. Kumaraswamy's second parameter.
#' @param a numeric. 
#' @param b numeric. 
#'
#' @return Density values.
#' @examples{
#' \donttest{
#'   curve(dKum(y = x, mu = 0.5, phi = 0.5))
#'}
#'}
#' @export

dKum <-function(y, mu, phi, a = 0, b = 1){
  (((phi*log(0.5))/(log(1 - mu^phi))) * y^(phi - 1)*(1 - y^phi)^
     (((log(0.5))/(log(1 - mu^phi))) - 1))
}
