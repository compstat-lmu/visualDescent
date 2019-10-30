# Introduction to VisualDescent
# Examples how to use. For further information on paramters see help files
# (I hope that works already, e.g. try ?gradDescent)
# required packages should already be in 'DESCRIPTION' file

# Step 1: define mathematical function (which is later optimized)
testfun1 = function(x) { return(x[1]^2 + 1/3*x[2]^2)} # arbitrarily chosen
testfun2 = function(x) { return((x[1]^2 + x[2] -11)^2 + (x[1] + x[2]^2 - 7)^2)} # himmelblau's

# Step 2: optimization (can also be performed for hihger dimensional functions)
# but visualization onyl possible for 3 dimensional functions.

# (a) gradient descent
optimGDfun1 = gradDescent(testfun1, c(-4, -4), step.size = 0.2)
optimGDfun2 = gradDescent(testfun2, c(0,0), step.size = 0.01)

# (b) gradient descent momentum (is same as grad descent if phi = 0) but I think this is
# clearer if students can compare both and set phi to 0 and see that its the same.
optimMomfun1 = gradDescentMomentum(testfun1, c(-4, -4), step.size = 0.2, phi = 0.3)
optimMomfun2 = gradDescentMomentum(testfun2, c(0, 0), step.size = 0.01, phi = 0.3)

# (c) AdaGrad (maybe we take out eps in params since it is only constant due to root
# in denominator in optimization step not to be 0)
optimAdafun1 = adaGrad(testfun1, c(-4,-4), step.size = 0.1, max.iter = 10000, eps = 0.01)
optimAdafun2 = adaGrad(testfun2, c(0,-2), step.size = 0.05, max.iter = 1000, eps = 0.01)


# Step 3: plot functions and iterations in optimization procedure

# (a) plot for gradient descent
plotGDfun1 = plot2d(f = optimGDfun1$optimfun, x1.lower = -4, x1.upper = 4, x2.lower = -4,
                      x2.upper = 4, n.x = 30, xmat = optimGDfun1$results)
plotGDfun1

plotGDfun2 = plot2d(f = optimGDfun2$optimfun, x1.lower = -4, x1.upper = 4, x2.lower = -4,
                        x2.upper = 4, n.x = 30, xmat = optimGDfun2$results)
plotGDfun2

# (b) plot for gradient descent momentum
plotMomfun1 = plot2d(f = optimMomfun1$optimfun, x1.lower = -4, x1.upper = 4, x2.lower = -4,
                     x2.upper = 4, n.x = 30, xmat = optimMomfun1$results)
plotMomfun1

plotMomfun2 = plot2d(f = optimMomfun2$optimfun, x1.lower = -4, x1.upper = 4, x2.lower = -4,
                        x2.upper = 4, n.x = 30, xmat = optimMomfun2$results)
plotMomfun2

# (c) plot for AdaGrad
plotAdatestfun1 = plot2d(f = optimAdafun1$optimfun, x1.lower = -4, x1.upper = 4, x2.lower = -4,
                x2.upper = 4, n.x = 30, xmat = optimAdafun1$results)
plotAdatestfun1

plotAdatestfun2 = plot2d(f = optimAdafun2$optimfun, x1.lower = -4, x1.upper = 4, x2.lower = -4,
                x2.upper = 4, n.x = 30, xmat = optimAdafun2$results)
plotAdatestfun2
