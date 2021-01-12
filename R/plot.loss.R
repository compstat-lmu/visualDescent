#' Visualize loss of optimization procedure
#'
#' This functions visualizes the difference between the true optimum and the optimum in the
#' current iteration (loss) during the optimization procedure.
#' The default upper and lower bounds for the plot are set by the box constraints of the
#' respective optimization function.
#'
#' @import checkmate
#' @import ggplot2
#' @import ggnewscale
#' @import colorRamps
#'
#' @param f a (multi-) dimensional function to be optimized.
#' @param x1.lower the lower boundary for the range of the x1 coordinate displayed.
#' @param x1.upper the upper boundary for the range of the x1 coordinate displayed.
#' @param x2.lower the lower boundary for the range of the x2 coordinate displayed.
#' @param x2.upper the upper boundary for the range of the x2 coordinate displayed.
#' @param n.x the number (equidistant) points at which f is evaluated w.r.t x1 and x2 coordinate.
#' @param xmat an object of class list containing the results of the algorithm.
#' @param trueOptZ is a numeric of the z coordinate of the true optimum.
#' @param algoName list of strings with names of algorithms. Length must equal length of 'xmat'.
#' @param optimError passed argument from optimization. Specifies if error in gradient occured.
#'
#' @export
plotLoss = function(f, x1.lower, x1.upper, x2.lower, x2.upper, xmat, trueOptZ = NULL, algoName = NULL,  optimError = FALSE) {

  # Check several input parameter
  if (checkList(xmat) == FALSE) stop("'xmat' must be a list object")
  if (checkList(algoName) == FALSE) stop("'algoName' must be a list object")
  if(optimError == TRUE) stop("Error in plotLoss: error occured in optimization. Please specify different set of parameters.")

  # Check if lower and upper bounds customized, else set to box constraints
  if (missing(x1.lower)) x1.lower = getLowerBoxConstraints(f)[1]
  if (missing(x2.lower)) x2.lower = getLowerBoxConstraints(f)[2]
  if (missing(x1.upper)) x1.upper = getUpperBoxConstraints(f)[1]
  if (missing(x2.upper)) x2.upper = getUpperBoxConstraints(f)[2]

  if (is.null(algoName)) {
    algoName = as.list(paste0("AlgoAutoFill", 1:length(xmat)))
  }


  if (!is.null(trueOptZ)) {
    print(trueOptZ)
    plot.dfOpt = data.frame(z = trueOptZ)
  } else {
    if (!is.null(getGlobalOptimum(f)$value)) {
      plot.dfOpt = data.frame(z = getGlobalOptimum(f)$value)
    } else {
      stop("To plot Loss minimum needs to be specified. Make sure the provided function has a specified global minimum")
    }
  }


  nresults = length(xmat)
  col = colorRamps::matlab.like2(nresults)
  col = col[1:nresults]
  niter = lapply(xmat, function(x) seq(1, nrow(x), by = 1))

  delta = lapply(xmat, function(x) x$y - plot.dfOpt$z)

  plot.df = mapply(function(x, u, v, w) cbind.data.frame(data.frame(iter = x, loss = u, algo = v, col = w, stringsAsFactors = FALSE)),
                   x = niter, u = delta, v = algoName, w = col, SIMPLIFY = FALSE)

  plot.df = do.call(rbind, plot.df)

  plot.loss = ggplot(plot.df, aes(x = iter, y = loss, color = algo)) +
    geom_path(size = 1.3) +
    scale_color_manual(values = col) +
    ggtitle("Objective value") +#("Loss to optimum") +
    xlab("Iteration") +
    ylab("Loss") +
    theme(legend.title = element_blank()) +
    theme(plot.title = element_text(hjust = .5))
  #theme(panel.background = element_rect(fill = 'white', colour = 'black'))

  return(plot.loss)

}
