#' Optimize mathematical function using the Adam algorithm
#'
#' This functions uses the Adam algorithm to find the minimum of a (multi-)
#' dimensional mathematical function. The combination considers both, the
#' average of the previous gradients (Momentum Optimizer) and the average
#' of the square gradients (RMS Prop), both under exponential decay.
#'
#' @import numDeriv
#' @importFrom magrittr "%>%"
#'
#' @param f a (multi-) dimensional function to be eptimized.
#' @param x0 the starting point of the optimization.
#' @param max.iter the maximum number of iterations performed in the optimization.
#' @param step.size the step size (sometimes referred to as 'learn-rate') of the optimization.
#' @param phi1 decay rate for RMS Prop term, i.e. the squared gradients.
#' @param phi2 decay rate for Momentum term, i.e. the previous gradients.
#' @param stop.grad the stop-criterion for the gradient change.
#'
#' @export
adam = function(f, x0, max.iter = 100, step.size = 0.01, phi1 = 0.5, phi2 = 0.8, stop.grad = .Machine$double.eps){

  if (!is.function(f)) stop("f is not a function")
  if (is.na(f(x0))) stop("Dimensions of function and starting point x0 do not match")

  #Initialize theta matrix and default parameter
  theta = matrix(0, nrow = (length(x0)+2), ncol = max.iter)
  theta[1:length(x0), 1] = x0
  theta[length(x0)+1, 1] = f(x0)

  s = matrix(0, nrow = length(x0), ncol = max.iter)
  mu = matrix(0, nrow = length(x0), ncol = max.iter)
  eps = 0.0001

  for (i in 2:max.iter) {

    #Calculate gradient
    nabla = grad(f, theta[1:length(x0), i-1])

    for (j in 1:length(x0)) {

      #Calculate mu  and s for j-th dimension
      mu[j, i] = phi1*mu[j, i-1] + (1-phi1)*nabla[j]
      s[j, i] = phi2*s[j, i-1] + (1-phi2)*nabla[j]*nabla[j]

      mu[j, i] = mu[j, i]/(1-phi1^i)
      s[j, i] = s[j, i]/(1-phi2^i)


      #Check if stop-criterion already reached
      if (all(abs(nabla) < stop.grad)) {
        i = i-1
        break
      }

      #Determine new point
      theta[j, i] = theta[j, i-1] - (step.size*mu[j, i])/((s[j, i] + eps)^(1/2))
    }

    theta[length(x0)+1, i] = f(theta[1:length(x0), i])
  }

  #Return results
  out = apply(matrix(seq(1:length(x0))), 1, function(x) return(theta[x, ])) %>%
    as.data.frame()

  names(out) = paste0("x", 1:(ncol(out)))
  out = cbind(out, y = theta[length(x0)+1, ])

  return(list(results = out, niter = i, optimfun = f))
}
