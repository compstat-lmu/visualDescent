# visualDescend
Visualization of different descent methods including GD, momentum method, AdaGrad.

# Visualization via Shiny App
Under the following link you can find a demo of the optimization algorithms with pre-defined optimization functions. All important parameters of the optimization can be controlled and updated in realtime.

[R Shiny App](https://philippscheller.shinyapps.io/shinyapp/)

As an alternative you can deploy the app on your local machine via the following function. Uncommented lines refer to packages that may be necessary to load beforehand. 

```r
# library(devtools)
# library(shiny)

devtools::install_github("compstat-lmu/visualDescent")
library(visualDescent)

runAppLocal()
```


# Example - First Steps

## Step 1 - Define mathematical function (to be optimized in step 2)
```r
  testfun1 = function(x) { return(x[1]^2 + 1/3*x[2]^2)} # arbitrarily chosen
  testfun2 = function(x) { return((x[1]^2 + x[2] -11)^2 + (x[1] + x[2]^2 - 7)^2)} # himmelblau's
  ```
## Step 2 - Optimization

### Gradient descent
```r
optimGDfun1 = gradDescent(f = testfun1, x0 = c(-4, -4), step.size = 0.2)
optimGDfun2 = gradDescent(f = testfun2, x0 = c(0, 0), step.size = 0.01)
```

### Gradient descent momentum
```r
optimMomfun1 = gradDescentMomentum(f = testfun1, x0 = c(-4, -4), step.size = 0.2, phi = 0.3)
optimMomfun2 = gradDescentMomentum(f = testfun2, x0 = c(0, 0), step.size = 0.01, phi = 0.3)
```
Note: setting 'phi=0' (all else equal) leads to same results than optimization with 'gradDescent()' function

### AdaGrad
```r
optimAdafun1 = adaGrad(f = testfun1, x0 = c(-4, -4), step.size = 0.1, max.iter = 1000)
optimAdafun2 = adaGrad(f = testfun2, x0 = c(0, -2), step.size = 0.05, max.iter = 1000)
```

## Step 3 - Plot functions and iterations in optimization procedure

### Plot gradient descent optim procedure
```r
plotGDfun1 = plot2d(f = optimGDfun1$optimfun, x1.lower = -4, x1.upper = 4, x2.lower = -4,
                      x2.upper = 4, n.x = 30, xmat = optimGDfun1$results)
plotGDfun1

plotGDfun2 = plot2d(f = optimGDfun2$optimfun, x1.lower = -4, x1.upper = 4, x2.lower = -4,
                        x2.upper = 4, n.x = 30, xmat = optimGDfun2$results)
plotGDfun2
```

### Plot gradient descent momentum procedure
```r
plotMomfun1 = plot2d(f = optimMomfun1$optimfun, x1.lower = -4, x1.upper = 4, x2.lower = -4,
                     x2.upper = 4, n.x = 30, xmat = optimMomfun1$results)
plotMomfun1

plotMomfun2 = plot2d(f = optimMomfun2$optimfun, x1.lower = -4, x1.upper = 4, x2.lower = -4,
                        x2.upper = 4, n.x = 30, xmat = optimMomfun2$results)
plotMomfun2
```

### Plot AdaGrad procedure
```r
plotAdatestfun1 = plot2d(f = optimAdafun1$optimfun, x1.lower = -4, x1.upper = 4, x2.lower = -4,
                x2.upper = 4, n.x = 30, xmat = optimAdafun1$results)
plotAdatestfun1

plotAdatestfun2 = plot2d(f = optimAdafun2$optimfun, x1.lower = -4, x1.upper = 4, x2.lower = -4,
                x2.upper = 4, n.x = 30, xmat = optimAdafun2$results)
plotAdatestfun2
```


# Adding customized functions to existing 'funData'
To add a new function we use 'makeSingleObjectiveFunction()' from the package 'smoof'.
Suppose we would like to add the paraboloid function known as f(x) = a*x^2+b*x^2 where a,b are
chosen fixed constants we can do so by defining

```r
paraboloid = makeSingleObjectiveFunction(name = "Paraboloid", fn = function(x) 2*x[1]^2 + 50*x[2]^2, par.set = makeParamSet(
    makeNumericParam("x1", lower = -5, upper = 5),
    makeNumericParam("x2", lower = -5, upper = 5)), global.opt.params = list(x1 = 0, x2 = 0), global.opt.value = 0)
```
In this example we define a name ("Paraboloid"), the function ("fn") with constant a = 2, b = 50 in our example and the lower and upper bounds -5, 5. Besides that we can define the global optimum parameters (global.opt.params) which is needed to be able to display the loss plot in the optimization procedure. 

For any further parameters use the help page (i.e. '?makeSingleObjectiveFunktion') or the package documentation under https://cran.r-project.org/web/packages/smoof/smoof.pdf 
