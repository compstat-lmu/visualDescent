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
#' @param trueOpt is a numeric vector of the true optimum of the function.
#' @param algoNamw list of strings with names of algorithms. Length must equal length of 'xmat'.
#'
#' @export
plot2d = function(f, x1.lower, x1.upper, x2.lower, x2.upper, n.x = 30L, xmat, trueOpt = NULL, algoName = NULL, plotSegments = FALSE) {

  # Check several input parameter
    if (checkList(xmat) == FALSE) stop("'xmat' must be a list object")
    if (checkList(algoName) == FALSE) stop("'algoName' must be a list object")
    if (!is.null(trueOpt) & !is.numeric(trueOpt)) stop("provided true optimum must be a numeric vector")
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
    nresults = length(xmat)
    xmat$y = 0
    xmat = as.list(xmat)
    col = c("black", "red", "blue", "green")
    col = col[1:nresults]

    if (!is.null(trueOpt)) plot.dfOpt = data.frame(x1 = trueOpt[1], x2 = trueOpt[2])

  # Generate grid with specfied dimensions and data.frame for plot
  x1 = seq(x1.lower, x1.upper, length.out = n.x)
  x2 = seq(x2.lower, x2.upper, length.out = n.x)

  df = expand.grid(x1, x2)
  y = apply(df, 1, f)

  plot.df = data.frame(df, y)
  plot.title = getName(f)

  names(plot.df) = c("x1", "x2", "y")

  plot.points.df = mapply(function(u, v, w) cbind.data.frame(u, data.frame(name = v, col = w, stringsAsFactors = FALSE)),
                          u = xmat[-(nresults+1)], v = algoName, w = col, SIMPLIFY = F)

  plot.points.df = do.call(rbind, plot.points.df)

  # Generate contour plot and add steps of descent procedure as points
  plot = ggplot(plot.df, aes(x1, x2)) +
    stat_contour(bins = n.x, aes(z = y, color = stat(level))) +
    ggtitle(paste(plot.title)) +
    new_scale_color() +
    geom_point(data = plot.points.df, aes(x = x1, y = x2, color = name),
               alpha = .5) +
    geom_path(data = plot.points.df, aes(x = x1, y = x2, color = name)) + 
    # scale_fill_manual(name = "Test", values = plot.points.df$col) +    # geom_path(data = plot.points.df, aes(x = x1, y = x2, col = name)) + 
    xlab(expression(theta[1])) + ylab(expression(theta[2])) + 
    # guides(colour = guide_legend(override.aes = list(color = c("black", "blue", "green", "grey")))) +
    # scale_colour_manual(breaks = plot.points.df$name, values = unique(plot.points.df$col)) +
    theme(plot.title = element_text(hjust = .5)) +
    theme(panel.background = element_rect(fill = 'white', colour = 'black')) 


    if(!is.null(trueOpt)) {
      plot = plot + geom_point(aes(x = plot.dfOpt$x1, y = plot.dfOpt$x2, fill = "Global Optimum"))
    }
  # for (i in 1:nresults) {
  #
  #   data = data.frame(x = xmat[[i]]$x1, y = xmat[[i]]$x2)
  #   print(algoName[[i]])
  #   plot = plot + geom_point(data = data, aes(x = x, y = y, fill = algoName[[i]]), color = col[i], size = .8)
  # }

  plot = plot + theme(legend.title = element_blank())


  # Return ggplot object
  return(plot)
}
