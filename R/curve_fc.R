#' curve_fc
#'
#' A function to make curves of the distributions. 
#'
#' @param model character. The model we want to curve.
#' @param par numeric. A vector of parameters.
#' @param col character. The curve color.
#' @param add logic. Should we allow more curves over?
#' @param leg logic. Show we show the legends?
#'
#' @return A curve.
#' @examples{
#' curve_fc("beta", par = c(0.5, 0.5))
#' }
#' @export

curve_fc <-   function(model, par, col = "red2", 
                       add = FALSE, leg = TRUE){
  mu <- par[1]
  phi <- par[2]
  
  if("beta" %in% model){
    m <- bquote(bold(mu == .(mu) ~ phi == .(phi)))

    curve(dBeta(y = x, mu = mu, phi = phi),
          xlim = c(0,1), 
          col = col,
          cex.lab = 1.5,
          lty = 1, 
          lwd = 5,
          ylab = "f(y)",
          xlab="y",
          add = add)
    if(leg){
      legend("topleft", 
             legend = m, 
             lty = 1, lwd = 3, col = 0)
    }
  }
  
  # Kumaraswamy distribution
  else if("kuma" %in% model ){
    m <- bquote(bold(mu == .(mu) ~ phi == .(phi)))
    
    curve(dKum(y = x, mu = mu, phi = phi),
          xlim = c(0,1), 
          col = col,
          lty = 1, 
          lwd = 5,
          cex.lab = 1.5,
          ylab = "f(y)",
          xlab = "y",
          add = add)
    if(leg){
      legend("topleft", 
             legend = m, 
             lty = 1, lwd = 3, col = 0)
    }
  }
  
  # Simplex distribution
  else if("simplex" %in% model){
    m <- bquote(bold(mu == .(mu) ~ phi == .(1/phi^2)))

    curve(dSimplex(y = x, mu = mu, phi = phi),
          xlim = c(0,1), 
          col = col,
          lty = 1, 
          lwd = 5,
          cex.lab = 1.5,
          ylab = "f(y)",
          xlab = "y",
          add = add)
    if(leg){
      legend("topleft", 
             legend = m, 
             lty = 1, lwd = 3, col = 0)
    }
  }
  
  # Unit gamma distribution
  else if("gu" %in% model){
    m <- bquote(bold(mu == .(mu) ~ phi == .(phi)))
    
    curve(dgUnit(y = x, mu = mu, phi = phi),
          xlim = c(0,1), 
          col = col,
          lty = 1, 
          lwd = 5,
          cex.lab = 1.5,
          ylab = "f(y)",
          xlab = "y",
          add = add)
    if(leg){
      legend("topleft", 
             legend = m, 
             lty = 1, lwd = 3, col = 0)
    }
  }
  
  # Retangular beta distribution
  else if("br" %in% model){
    alpha <- par[1]
    gamma <- par[2]
    phi <- par[3]
    
    m <- bquote(bold(alpha == .(alpha) ~ gamma == .(gamma) 
                     ~ phi == .(phi)))
    
    curve(dBR(y = x, alpha = alpha, gamma = gamma, phi = phi),
          xlim = c(0,1), 
          col = col,
          lty = 1, 
          lwd = 5,
          cex.lab = 1.5,
          ylab = "f(y)",
          xlab = "y",
          add = add)
    if(leg){
      legend("topleft", 
             legend = m, 
             lty = 1, lwd = 3, col = 0)
    }
  }
  
  # ll distribution
  else if("ll" %in% model){
    m <- bquote(bold(mu == .(mu) ~ phi == .(1/phi^2) ))
    
    curve(dLL(y = x, mu = mu, phi = phi),
          xlim = c(0,1), 
          col = col,
          lty = 1, 
          lwd = 5,
          cex.lab = 1.5,
          ylab = "f(y)",
          xlab = "y",
          add = add)
    if(leg){
      legend("topleft", 
             legend = m, 
             lty = 1, lwd = 3, col = 0)
    }
  }
  
  # ln distribution
  else if("ln" %in% model){
    m <- bquote(bold(mu == .(mu) ~ phi == .(1/phi^2) ))
    
    curve(dLN(y = x, mu = mu, phi = phi),
          xlim = c(0,1), 
          col = col,
          lty = 1, 
          lwd = 5,
          cex.lab = 1.5,
          ylab = "f(y)",
          xlab = "y",
          add = add)
    if(leg){
      legend("topleft", 
             legend = m, 
             lty = 1, lwd = 3, col = 0)
    }
  }
  
  # lt distribution
  else if("lt" %in% model){
    nu <- par[3]

    m <- bquote(bold(mu == .(mu) ~ phi == .(1/phi^2) ~ nu == .(nu)))
    
    curve(dLT(y = x, mu = mu, phi = phi, nu = nu),
          xlim = c(0,1), 
          col = col,
          lty = 1, 
          lwd = 5,
          cex.lab = 1.5,
          ylab = "f(y)",
          xlab = "y",
          add = add)
    if(leg){
      legend("topleft", 
             legend = m, 
             lty = 1, lwd = 3, col = 0)
    }
  }

  # lep
  else if("lep" %in% model){
    nu <- exp(par[3])
    m <- bquote(bold(mu == .(mu) ~ phi == .(1/phi^2) ~ nu == .(nu)))
    
    curve(dLEP(y = x, mu = mu, phi = phi, nu = nu),
          xlim = c(0,1), 
          col = col,
          lty = 1, 
          lwd = 5,
          cex.lab = 1.5,
          ylab = "f(y)",
          xlab = "y",
          add = add)
    if(leg){
      legend("topleft", 
             legend = m, 
             lty = 1, lwd = 3, col = 0)
    }
  }
  
  # lsl distribution
  else if("lsl" %in% model){
    lambda <- par[3]
    m <- bquote(bold(mu == .(mu) ~ phi == .(phi) ~ lambda == .(lambda)))
    
    curve(dLSL(y = x, mu = mu, phi = phi, lambda = lambda),
          xlim = c(0,1), 
          col = col,
          lty = 1, 
          lwd = 5,
          cex.lab = 1.5,
          ylab = "f(y)",
          xlab = "y",
          add = add)
    if(leg){
      legend("topleft", 
             legend = m, 
             lty = 1, lwd = 3, col = 0)
    }  
  }
  
  # lsn distribution
  else if("lsn" %in% model){
    lambda <- par[3]
    m <- bquote(bold(mu == .(mu) ~ phi == .(1/phi^2) ~ lambda == .(lambda)))
    
    curve(dLSN(y = x, mu = mu, phi = phi, lambda = lambda),
          xlim = c(0,1), 
          col = col,
          lty = 1, 
          lwd = 5,
          cex.lab = 1.5,
          ylab = "f(y)",
          xlab = "y",
          add = add)
    if(leg){
      legend("topleft", 
             legend = m, 
             lty = 1, lwd = 3, col = 0)
    }
  }
  
  # lst distribution
  else if("lst" %in% model){
    lambda <- par[3]
    tau <- par[4]
    m <- bquote(bold(mu == .(mu) ~ phi == .(1/phi^2) ~ 
                       lambda == .(lambda) ~ tau == .(tau)))
    
    curve(dLST(y = x, mu = mu, phi = phi,
                    lambda = lambda, tau = tau),
          xlim = c(0,1), 
          col = col,
          lty = 1, 
          lwd = 5,
          cex.lab = 1.5,
          ylab = "f(y)",
          xlab = "y",
          add = add)
    if(leg){
      legend("topleft", 
             legend = m, 
             lty = 1, lwd = 3, col = 0)
    }
  }
  
  # lsep distribution
  else if("lsep" %in% model){
    lambda <- par[3]
    tau <- par[4]

    m <- bquote(bold(mu == .(mu) ~ phi == .(1/phi^2)  
                     ~ lambda == .(lambda) ~ tau == .(tau)))
    
    curve(dLSEP(y = x, mu = mu, phi = phi,
                    lambda = lambda, tau = tau),
          xlim = c(0,1), 
          col = col,
          lty = 1, 
          lwd = 5,
          cex.lab = 1.5,
          ylab = "f(y)",
          xlab = "y",
          add = add)
    if(leg){
      legend("topleft", 
             legend = m, 
             lty = 1, lwd = 3, col = 0)
      
    }
  }
}

