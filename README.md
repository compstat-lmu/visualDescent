# visualDescend
Visualization of different descent methods including GD, momentum method, AdaGrad

# Installation
```
devtools::install_github("PhilippScheller/visualDescend")
```

# Example - First Steps

## Step 1 - Define mathematical function (to be optimized in step 2)
```{generate-testfun, results = "hide"}
  testfun1 = function(x) { return(x[1]^2 + 1/3*x[2]^2)} # arbitrarily chosen
  testfun2 = function(x) { return((x[1]^2 + x[2] -11)^2 + (x[1] + x[2]^2 - 7)^2)} # himmelblau's
  ```
## Step 2 - Optimization

### Gradient descent
```{optim-gd, results = "hide"}
optimGDfun1 = gradDescent(f = testfun1, x0 = c(-4, -4), step.size = 0.2)
optimGDfun2 = gradDescent(f = testfun2, x0 = c(0, 0), step.size = 0.01)
```

### Gradient descent momentum
```{optim-momentum, results = "hide"}
optimMomfun1 = gradDescentMomentum(f = testfun1, x0 = c(-4, -4), step.size = 0.2, phi = 0.3)
optimMomfun2 = gradDescentMomentum(f = testfun2, x0 = c(0, 0), step.size = 0.01, phi = 0.3)
```
Note: setting 'phi=0' (all else equal) leads to same results than optimization with 'gradDescent()' function

### AdaGrad
```{optim-adaGrad, results = "hide"}
optimAdafun1 = adaGrad(f = testfun1, x0 = c(-4, -4), step.size = 0.1, max.iter = 10000, eps = 0.01)
optimAdafun2 = adaGrad(f = testfun2, x0 = c(0, -2), step.size = 0.05, max.iter = 1000, eps = 0.01)
```

## Step 3 - Plot functions and iterations in optimization procedure

### Plot gradient descent optim procedure
```{plot-optim-gd, results = "hide"}
plotGDfun1 = plot2d(f = optimGDfun1$optimfun, x1.lower = -4, x1.upper = 4, x2.lower = -4,
                      x2.upper = 4, n.x = 30, xmat = optimGDfun1$results)
plotGDfun1

plotGDfun2 = plot2d(f = optimGDfun2$optimfun, x1.lower = -4, x1.upper = 4, x2.lower = -4,
                        x2.upper = 4, n.x = 30, xmat = optimGDfun2$results)
plotGDfun2
```

### Plot gradient descent momentum procedure
```{plot-momentum, results = "hide"}
plotMomfun1 = plot2d(f = optimMomfun1$optimfun, x1.lower = -4, x1.upper = 4, x2.lower = -4,
                     x2.upper = 4, n.x = 30, xmat = optimMomfun1$results)
plotMomfun1

plotMomfun2 = plot2d(f = optimMomfun2$optimfun, x1.lower = -4, x1.upper = 4, x2.lower = -4,
                        x2.upper = 4, n.x = 30, xmat = optimMomfun2$results)
plotMomfun2
```

### Plot AdaGrad procedure
```{plot-adaGrad, results = "hide"}
plotAdatestfun1 = plot2d(f = optimAdafun1$optimfun, x1.lower = -4, x1.upper = 4, x2.lower = -4,
                x2.upper = 4, n.x = 30, xmat = optimAdafun1$results)
plotAdatestfun1

plotAdatestfun2 = plot2d(f = optimAdafun2$optimfun, x1.lower = -4, x1.upper = 4, x2.lower = -4,
                x2.upper = 4, n.x = 30, xmat = optimAdafun2$results)
plotAdatestfun2
```






