#' Optimize mathematical function using the RMSProp algorithm
#'
#' This functions uses the RMSProp algorithm to find the minimum of a (multi-)
#' dimensional mathematical function. The algorithm searches for the.
#' The 'eps' factor avoids numerical issues of dividing by 0. The 'step.size'
#' scales the movement into the single coordinate direction.
#'
#' @import numDeriv
#' @importFrom magrittr "%>%"
#'
#' @param f a (multi-) dimensional function to be eptimized.
#' @param x0 the starting point of the optimization.
#' @param max.iter the maximum number of iterations performed in the optimization.
#' @param stop.grad the stop-criterion for the gradient change.
#'
#' @export
adaDelta = function(f, x0, max.iter = 100, gamma = 0.95, stop.grad = .Machine$double.eps){
  
  if (!is.function(f)) stop("f is not a function")
  if (is.na(f(x0))) stop("Dimensions of function and starting point x0 do not match")
  
  #Initialize theta matrix and default parameter
  errorObs =logical(1L)
  theta = matrix(0, nrow = (length(x0) + 1), ncol = max.iter)
  theta[1:length(x0), 1] = x0
  theta[length(x0) + 1, 1] = f(x0)
  
  #Matrix with mean values of previous gradients
  s = matrix(0, nrow = length(x0), ncol = max.iter)
  s[,1] = grad(f, theta[1:length(x0), 1])^2
  delta = matrix(0, nrow = length(x0), ncol = max.iter)
  delta[, 1] = (1-gamma) * theta[1:length(x0), 1]^2
  eps = 0.0001
  
  for (i in 2:max.iter) {
    
    try = tryCatch({
      #Calculate gradient
      nabla = grad(f, theta[1:length(x0), i - 1])
      
    },
      error = function(contd) {
        
        errorObs <<- TRUE
        
      }, finally = {
        if (errorObs == TRUE) {
          warning(c("Error RMSprop: Error in gradient calculation. Please choose different set of parameters.",
            "Often a smaller step size fixes this issue."))
        }
      })
    #Check if stop-criterion already reached
    if (all(abs(nabla) < stop.grad)) {
      i = i - 1
      break
    }
    
    #Calculate expectation (here arithmetic mean) of previous gradients
    s[, i] = gamma * mean(s[, i - 1]) + (1 - gamma) * nabla^2
    
    #Determine new point and calculate function value
    if (i < 3){
      theta[1:length(x0), i] = theta[1:length(x0), i - 1] - ((c(0, 0) + eps)^(1/2)) /((s[, i] + eps)^(1/2))*nabla
    } else {
      theta[1:length(x0), i] = theta[1:length(x0), i - 1] - ((delta[, i - 2] + eps)^(1/2)) /((s[, i] + eps)^(1/2))*nabla
    }
    
    #calculate delta
    delta[, i] = gamma * delta[, i - 1] + (1-gamma) * (theta[1:length(x0), i] - theta[1:length(x0), i - 1])^2
    
    theta[length(x0) + 1, i] = f(theta[1:length(x0), i])
  }
  
  
  #Return results
  out = apply(matrix(seq(1:length(x0))), 1, function(x) return(theta[x, ])) %>%
    as.data.frame()
  
  names(out) = paste0("x", 1:(ncol(out)))
  out = cbind(out, y = theta[length(x0)+1, ])
  
  return(list(algorithm = "AdaDelta", results = out, niter = i, optimfun = f, errorOccured = errorObs))
}
