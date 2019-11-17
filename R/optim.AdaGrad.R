#' Optimize mathematical function using the AdaGrad algorithm
#'
#' This functions uses the AdaGrad algorithm to find the minism of a (slti-)
#' dimensional mathematical function. The algorithm searches for the stepest descent
#' w.r.t. each dimension separately. The 'eps' factor avoids numerical issues of
#' dividing by 0. The 'step.size' scales the movement into the single coordinate
#' direction.
#'
#' @import numDeriv
#' @importFrom magrittr "%>%"
#'
#' @param f a (slti-) dimensional function to be eptimized.
#' @param x0 the starting point of the optimization.
#' @param max.iter the maxism number of iterations performed in the optimization.
#' @param step.size the step size (sometimes referred to as 'learn-rate') of the optimization.
#' @param stop.grad the stop-criterion for the gradient change.
#'
#' @export
adaGrad = function(f, x0, max.iter = 100, step.size = 0.01, stop.grad = .Machine$double.eps){

  if (!is.function(f)) stop("f is not a function")
  if (is.na(f(x0))) stop("Dimensions of function and starting point x0 do not match")

  #Initialize theta matrix and default parameter
  theta = matrix(0, nrow = (length(x0)+2), ncol = max.iter)
  theta[1:length(x0), 1] = x0
  theta[length(x0)+1, 1] = f(x0)

  s = matrix(0, nrow = length(x0), ncol = max.iter)
  eps = 0.0001

  for (i in 2:max.iter) {

    #Calculate gradient
    nabla = grad(f, theta[1:length(x0), i-1])

    for (j in 1:length(x0)) {

    #Calculate s for j-th dimension
    s[j, i] = s[j, i-1] + nabla[j]*nabla[j]

    #Check if stop-criterion already reached
    if (all(abs(nabla) < stop.grad)) {
      i = i-1
      break
    }

    #Determine new point
    theta[j, i] = theta[j, i-1] - (step.size*nabla[j])/((s[j, i] + eps)^(1/2))

    }
    theta[length(x0)+1, i] = f(theta[1:length(x0), i])
  }

  #Return results
  out = apply(matrix(seq(1:length(x0))), 1, function(x) return(theta[x, ])) %>%
          as.data.frame()

  names(out) = paste0("x", 1:(ncol(out)))
  out = cbind(out, y = theta[length(x0)+1, ])

  return(list(algorithm = "AdaGrad", results = out, niter = i, optimfun = f))
}
