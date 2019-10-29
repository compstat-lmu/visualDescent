#' Optimize mathematical function using gradient descent with momentum
#'
#' This functions uses the gradient descent algorithm with momentum to find
#' the minimum of a (multi-) dimensional mathematical function. The parameter
#' 'phi' controls for the weight of prior gradients thus indirectly steering
#' the velocity of the algorithm.
#'
#' @param f a (multi-) dimensional function to be eptimized.
#' @param x0 the starting point of the optimization.
#' @param max.iter the maximum number of iterations performed in the optimization.
#' @param step.size the step size (sometimes referred to as 'learn-rate') of the optimization.
#' @param stop.grad the stop-criterion for the gradient change.
#' @param phi controls the weight of the prior gradient contribution in the velocity.
#'
#' @export

gradDescentMomentum = function(f, x0, max.iter = 100, step.size = 0.01, stop.grad = 0.01, phi = 0.5){

  if (!is.function(f)) stop("f is not a function")
  if (is.na(f(x0))) stop("Dimensions of function and start point x0 do not match")
  if ( (phi<0) | (phi>1)) stop("phi must be element [0,1]")

  xmat = matrix(0, nrow = (length(x0) +2), ncol = max.iter)
  xmat[1:2, 1] = x0 #x1,x2
  xmat[3, 1] = f(x0) #y
  mu0 = mu1 = c(0, 0)

  for (i in 2:max.iter){

    if(i > 2){
      mu1 = phi*mu0 + nabla
    }
    #Calculate gradient at current point
    nabla = grad(f, xmat[1:2, i-1])

    #Check if stop-criterion already reached
    if(all(abs(nabla) < stop.grad)){
      i = i-1
      break
    }

    #Determine new point by moving into negative grad direction
    xmat[1:2, i] = xmat[1:2, i-1] - step.size*(nabla+phi*mu1)
    xmat[3, i] = f(xmat[1:2, i])
  }
  #Return results

  xmat = data.frame(x1 = xmat[1, ], x2 = xmat[2, ], y = xmat[3, ])
  return(list(xmin = xmat[i, ], xmat = xmat[1:i, ], iter = i))
}
