#' dBR
#'
#' Retangular Beta distribution. 
#'
#' @param y numeric. Variable of interest.
#' @param gamma numeric. RB's first parameter.
#' @param phi numeric. RB's second parameter.
#' @param alpha numeric. RB's third parameter.
#'
#' @return Density values.
#' @examples{
#' \donttest{
#'   curve(dBR(y = x, gamma = 0.5, phi = 0.5, alpha = 0.5))
#'}
#'}
#' @export

dBR <- function(y, gamma, phi, alpha){
  (alpha*(1 - abs(2*gamma - 1))+(1 - alpha*(1 - abs(2*gamma - 1))) * 
     dbeta(y,phi*(gamma - 0.5*alpha*(1 - abs(2*gamma - 1)))/
             (1 - alpha*(1 - abs(2*gamma - 1))),
           phi*(1 - (gamma - 0.5*alpha*(1-abs(2*gamma - 1)))/
                  (1 - alpha*(1 - abs(2*gamma - 1))))))
}
