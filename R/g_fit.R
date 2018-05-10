#' g_fit
#'
#' Fits `gamlss` distributions. The options are "ll", "ln", "lt",
#' "lep", "lsn", "lst", "lsep"
#'
#' @param model character. The chosen model.
#' @param y numeric. The variable values.
#'
#' @return the fitted parameters 
#' @examples{
#' \donttest{
#'   x <- seq(0.00001, 0.9999, l = 50)
#'   g_fit("ll", x)
#'}
#'}
#' @export


g_fit <- function(model, y){
  if(model == "ll"){
    invisible(capture.output(gamlss.dist::gen.Family("LO", 
                                                     type = "logit")))
    fit <- gamlss::gamlss(y ~ 1, family = logitLO,
                          trace = FALSE)
    fit_par <- c(fit$mu.coefficients, 
                 exp(fit$sigma.coefficients))
  } else if(model == "ln"){
    invisible(capture.output(gamlss.dist::gen.Family("NO", 
                                                     type = "logit")))
    fit <- gamlss::gamlss(y ~ 1, family = logitNO,
                          trace = FALSE)
    fit_par <- c(fit$mu.coefficients, 
                 exp(fit$sigma.coefficients))
  } else if(model == "lt"){
    invisible(capture.output(gamlss.dist::gen.Family("TF", type = "logit")))
    fit <- gamlss::gamlss(y ~ 1, family = logitTF,
                          trace = FALSE)
    fit_par <- c(fit$mu.coefficients, 
                 exp(fit$sigma.coefficients),
                 exp(fit$nu.coefficients))
  } else if(model == "lep"){
    PETest(0)
    invisible(capture.output(gamlss.dist::gen.Family("PET", type = "logit")))
    fit <- gamlss::gamlss(y ~ 1, family = logitPE,
                          trace = FALSE)
    
    fit_par <- c(fit$mu.coefficients,
                 exp(fit$sigma.coefficients),
                 fit$nu.coefficients)
    
  } else if(model == "lsn"){
    invisible(capture.output(gamlss.dist::gen.Family("SN1", type = "logit")))
    fit <- gamlss::gamlss(y ~ 1, family = logitSN1,
                          trace = FALSE)
    fit_par <- c(fit$mu.coefficients, 
                 exp(fit$sigma.coefficients),
                 fit$nu.coefficients)
  } else if(model == "lst"){
    invisible(capture.output(gamlss.dist::gen.Family("ST1", type = "logit")))
    fit <- gamlss::gamlss(y ~ 1, family = logitST1,
                          trace = FALSE)
    fit_par <- c(fit$mu.coefficients, 
                 exp(fit$sigma.coefficients),
                 fit$nu.coefficients,
                 exp(fit$tau.coefficients))
  } else if(model == "lsep"){
    invisible(capture.output(gamlss.dist::gen.Family("SEP", type = "logit")))
    fit <- gamlss::gamlss(y ~ 1, family = logitSEP,
                          trace = FALSE)
    fit_par <- c(fit$mu.coefficients, 
                 exp(fit$sigma.coefficients),
                 fit$nu.coefficients,
                 exp(fit$tau.coefficients))
  }
  return(fit_par)
}

