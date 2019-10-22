library(numDeriv) #Library used for gradient calculation

grad.descent = function(f, x0, max.iter = 100, step.size = 0.01, stop.grad = 0.01){

    xmat = matrix(0, nrow = (length(x0) +1), ncol = max.iter)
    xmat[1:2, 1] = x0
    xmat[3, 1] = f(x0)

    for (i in 2:max.iter){
      #Calculate gradient at current point
      nabla = grad(f, xmat[1:2, i-1])

      #Check if stop-criterion already reached
      if(all(abs(nabla) < stop.grad)){
        i = i-1
        break
      }

      #Determine new point by moving into negative grad direction
      xmat[1:2, i] = xmat[1:2, i-1] - step.size*nabla
      xmat[3, i] = f(xmat[1:2, i])
    }
    #Return results

    xmat = data.frame(x1 = xmat[1, ], x2 = xmat[2, ], y = xmat[3, ])
    return(list(xmin = xmat[i, ], xmat = xmat[1:i, ], iter = i))
  }
