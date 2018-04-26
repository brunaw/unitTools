#' dSimplex
#'
#' Simplex distribution. 
#'
#' @param y numeric. Variable of interest.
#' @param mu numeric. Simplex's first parameter.
#' @param phi numeric. Simplex's second parameter.
#'
#' @return Density values.
#' @examples{
#' \donttest{
#'   curve(dSimplex(y = x, mu = 0.5, phi = 0.5))
#'}
#'}
#' @export

dSimplex <- function(y, mu, phi){
  simplexreg::dsimplex(x = y, mu = mu, sig = phi)
}