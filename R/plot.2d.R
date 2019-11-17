#' Visualize optimization procedure in 2d contour plot
#'
#' This functions visualizes the steps of the optimization in a 2 dimensional contour plot.
#' The default upper and lower bounds for the plot are set by the box constraints of the
#' respective optimization function.
#'
#' @import checkmate
#' @import ggplot2
#' @import smoof
#' @importFrom stringr "str_extract_all"
#'
#' @param f a (multi-) dimensional function to be eptimized.
#' @param x1.lower the lower boundary for the range of the x1 coordinate displayed.
#' @param x1.upper the upper boundary for the range of the x1 coordinate displayed.
#' @param x2.lower the lower boundary for the range of the x2 coordinate displayed.
#' @param x2.upper the upper boundary for the range of the x2 coordinate displayed.
#' @param n.x the number (equidistant) points at which f is evaluated w.r.t x1 and x2 coordinate.
#' @param xmat an object of class list containing the results of the algorithm.
#' @param algoNamw list of strings with names of algorithms. Length must equal length of 'xmat'.
#'
#' @export
plot2d = function(f, x1.lower, x1.upper, x2.lower, x2.upper, n.x = 30L, xmat, algoName = NULL) {

  # Check several input parameter
    if (checkList(xmat) == FALSE) stop("'xmat' must be a list object")
    if (checkList(algoName) == FALSE) stop("'algoName' must be a list object")
    if (length(xmat) > length(algoName) && !is.null(algoName)) {
      warning("unsufficient names provides: outstanding instances autofilled")
      algoName = as.list(unlist(algoName), paste0("AlgoAutoFill", (length(xmat) - length(algoName)):length(xmat)))
    }
    if (is.null(algoName)) {
      algoName = as.list(paste0("AlgoAutoFill", 1:length(xmat)))
    }

  # Check if lower and upper bounds customized, else set to box constraints
    if (missing(x1.lower)) x1.lower = getLowerBoxConstraints(f)[1]
    if (missing(x2.lower)) x2.lower = getLowerBoxConstraints(f)[2]
    if (missing(x1.upper)) x1.upper = getUpperBoxConstraints(f)[1]
    if (missing(x2.upper)) x2.upper = getUpperBoxConstraints(f)[2]

  # Modify 'xmat' to match 3rd coordinate (set to 0) and identify number of procedures to be plotted
  xmat$y = 0
  nresults = length(xmat)
  col = seq(1:nresults)

  # Generate grid with specfied dimensions and data.frame for plot
  x1 = seq(x1.lower, x1.upper, length.out = n.x)
  x2 = seq(x2.lower, x2.upper, length.out = n.x)

  df = expand.grid(x1, x2)
  y = apply(df, 1, f)

  plot.df = data.frame(df, y)
  # plot.title = capture.output(f)[1] %>%
  #           str_extract_all("(?<=\\{).+?(?=\\})")
  plot.title = getName(f)

  names(plot.df) = c("x1", "x2", "y")

  # Generate contour plot and add steps of descent procedure as points
  plot = ggplot(plot.df, aes(x1, x2, z = y)) +
    stat_contour(bins = n.x, aes(colour = stat(level))) +
    ggtitle(paste(plot.title)) +
    theme(plot.title = element_text(hjust = .5))

  for (i in 1:(nresults-1)) {
    plot = plot + geom_point(data = xmat[[i]], color = i, size = .8)
  }

  # Return ggplot object
  return(plot)
}
