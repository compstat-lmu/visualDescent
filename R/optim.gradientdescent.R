#' Optimize mathematical function using gradient descent
#'
#' This functions uses the gradient descent algorithm to find the minimum of a
#' (multi-) dimensional mathematical function.
#'
#' @import numDeriv
#' @importFrom magrittr "%>%"
#'
#' @param f a (multi-) dimensional function to be eptimized.
#' @param x0 the starting point of the optimization.
#' @param max.iter the maximum number of iterations performed in the optimization.
#' @param step.size the step size (sometimes referred to as 'learn-rate') of the optimization.
#' @param stop.grad the stop-criterion for the gradient change.
#'
#' @export
gradDescent = function(f, x0, max.iter = 100, step.size = 0.001, stop.grad = .Machine$double.eps) {

    errorObs =logical(1L)
    theta = matrix(0, nrow = (length(x0)+1), ncol = max.iter)
    theta[1:length(x0), 1] = x0
    theta[length(x0)+1, 1] = f(x0)

    for (i in 2:max.iter) {

      try = tryCatch({

      nabla = grad(f, theta[1:length(x0), i-1])
      },
      error = function(contd) {

        errorObs <<- TRUE

      }, finally = {
        if (errorObs == TRUE) {
          warning(c("Error GradDescent: Error in gradient calculation. Please choose different set of parameters.",
                    "Often a smaller step size fixes this issue."))
        }
      })


      #Check if stop-criterion already reached
      if (all(abs(nabla) < stop.grad)) {
        i = i-1
        break
      }

      #Determine new point by moving into negative grad direction
      theta[1:length(x0), i] = theta[1:length(x0), i-1] - step.size*nabla
      theta[length(x0)+1, i] = f(theta[1:length(x0), i])

    }

    #Return results
    out = apply(matrix(seq(1:length(x0))), 1, function(x) return(theta[x, ])) %>%
      as.data.frame()

    names(out) = paste0("x", 1:(ncol(out)))
    out = cbind(out, y = theta[length(x0)+1, ])

    return(list(algorithm = "Gradient Descent", results = out, niter = i, optimfun = f, errorOccured = errorObs))
  }
