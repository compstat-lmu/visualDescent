#' Optimize mathematical function using gradient descent with momentum
#'
#' This functions uses the gradient descent algorithm with momentum to find
#' the minimum of a (multi-) dimensional mathematical function. The parameter
#' 'phi' controls for the weight of prior gradients thus indirectly steering
#' the velocity of the algorithm.
#'
#' @import numDeriv
#' @importFrom magrittr "%>%"
#'
#' @param f a (multi-) dimensional function to be eptimized.
#' @param x0 the starting point of the optimization.
#' @param max.iter the maximum number of iterations performed in the optimization.
#' @param step.size the step size (sometimes referred to as 'learn-rate') of the optimization.
#' @param phi controls the weight of the prior gradient contribution in the velocity.
#' @param stop.grad the stop-criterion for the gradient change.
#'
#' @export
gradDescentMomentum = function(f, x0, max.iter = 100, step.size = 0.01, phi = 0.5, stop.grad = .Machine$double.eps) {

  if (!is.function(f)) stop("f is not a function")
  if (is.na(f(x0))) stop("Dimensions of function and start point x0 do not match")
  if ( (phi<0) | (phi>1)) stop("phi must be element [0,1]")

  errorObs = logical(1L)
  theta = matrix(0, nrow = (length(x0)+1), ncol = max.iter)
  theta[1:length(x0), 1] = x0
  theta[length(x0)+1, 1] = f(x0)


  mu0 = mu1 = rep(0, times = length(x0))

  for (i in 2:max.iter){

    tryCatch({
    #Calculate gradient from prior point
    nabla = grad(f, theta[1:length(x0), i-1])
    },
  error = function(contd) {

    errorObs <<- TRUE

  }, finally = {
    if (errorObs == TRUE) {
      warning(c("Error GradDescentMomentum: Error in gradient calculation. Please choose different set of parameters.",
                "Often a smaller step size fixes this issue."))
    }
  })
    #Calculate mu
    mu1 = phi*mu0 - step.size*nabla


    #Check if stop-criterion already reached
    if(all(abs(nabla) < stop.grad)){
      i = i-1
      break
    }

    #Determine new point by moving into negative grad direction
    theta[1:length(x0), i] = theta[1:length(x0), i-1] + mu1
    theta[length(x0)+1, i] = f(theta[1:length(x0), i])
    mu0 = mu1
  }

  #Return results
  out = apply(matrix(seq(1:length(x0))), 1, function(x) return(theta[x, ])) %>%
    as.data.frame()

  names(out) = paste0("x", 1:(ncol(out)))
  out = cbind(out, y = theta[length(x0)+1, ])

  return(list(algorithm = "Momentum method", results = out, niter = i, optimfun = f, errorOccured = errorObs))
}
