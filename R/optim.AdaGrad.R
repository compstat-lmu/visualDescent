#' Optimize mathematical function using AdaGrad
#'
#' This functions uses the AdaGrad algorithm to find the minimum of a (multi-)
#' dimensional mathematical function. The algorithm searches for the stepest descent
#' w.r.t. each dimension separately. The 'eps' factor avoids numerical issues of
#' dividing by 0. The 'step.size' scales the movement into the single coordinate
#' direction.
#'
#' @param f a (multi-) dimensional function to be eptimized.
#' @param x0 the starting point of the optimization.
#' @param max.iter the maximum number of iterations performed in the optimization.
#' @param step.size the step size (sometimes referred to as 'learn-rate') of the optimization.
#' @param stop.grad the stop-criterion for the gradient change.
#' @param eps constant to ensure denominator does not equal 0.
#'
#' @export
adaGrad = function(f, x0, max.iter = 100, step.size = 0.01, stop.grad = 0.01, eps = 0.01){

  if (!is.function(f)) stop("f is not a function")
  if (is.na(f(x0))) stop("Dimensions of function and starting point x0 do not match")

  #Initialize theta matrix and default parameter
  theta = matrix(0, nrow = (length(x0)+2), ncol = max.iter)
  theta[1:length(x0), 1] = x0
  theta[length(x0)+1, 1] = f(x0)

  mu0 = mu1 = rep(0, times = length(x0))

  for (i in 2:max.iter) {

    #Calculate gradient
    nabla = grad(f, theta[1:length(x0), i-1])

    for (j in 1:length(x0)) {

    #Calculate mu for j-th dimension
    mu1[j] = mu0[j] + nabla[j]*nabla[j]

    #Check if stop-criterion already reached
    if (all(abs(nabla) < stop.grad)) {
      i = i-1
      break
    }

    #Determine new point
    theta[j, i] = theta[j, i-1] - (step.size*nabla[j])*((mu1[j] + eps)^(-1/2))
    mu0 = mu1
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
