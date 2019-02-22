#' loglike
#'
#' Loglikelihood for the Beta, Kumaraswamy, Unit Gamma, Simplex,
#'  Retangular Beta or LSL distributions. 
#'
#' @param par numeric. Parameters for the distribution.
#' @param model character. The chosen model.
#' @param x numeric. The variable values.
#' @param sum logic. Should we return the sum of the 
#' loglikelihood?
#'
#' @return loglikelihood values
#' @examples{
#' \donttest{
#'   x <- seq(0.00001, 0.9999, l = 50)
#'   loglike(par = c(0.5, 0.5), model = "beta", x = x, sum = "yes")
#'}
#'}
#' @export
loglike <- function(par, model, x, sum = "yes"){
  if(model == "beta"){
    m <- unitTools::dBeta(y = x, mu = par[1], phi = par[2])
  } else if(model == "kuma"){
    m <- dKum(y = x, mu = par[1], phi = par[2])
  } else if(model == "gu"){
    m <- dgUnit(y = x, mu = par[1], phi = par[2])
  } else if(model == "simplex"){
    m <- dSimplex(y = x, mu = par[1], phi = par[2])  
  } else if(model == "br"){
    m <- dBR(y = x, gamma = par[1], phi = par[2], alpha = par[3])
  }
  else if(model == "lsl"){
    m <- dLSL(y = x, mu = par[1], phi = par[2], lambda = par[3])
  }
  return(switch(sum, "yes" = sum(-log(m)),
                "no" = -log(m) 
  ))
}