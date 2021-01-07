#' Visualize optimization procedure in 3d plot
#'
#' This functions visualizes the steps of the optimization in a 3 dimensional contour plot.
#' The default upper and lower bounds for the plot are set by the box constraints of the
#' respective optimization function.
#'
#' @import checkmate
#' @import plotly
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
plot3d = function(f, x1.lower, x1.upper, x2.lower, x2.upper, n.x = 100L, xmat, trueOpt = NULL, algoName = NULL,
  optimError = FALSE) {

  # Check several input parameter
  if (checkList(xmat) == FALSE) stop("'xmat' must be a list object")
  if (checkList(algoName) == FALSE) stop("'algoName' must be a list object")
  if (!is.null(trueOpt) & !is.numeric(trueOpt)) stop("provided true optimum must be a numeric vector")
  if(optimError == TRUE) stop("Error in plot3d: error occured in optimization. Please specify different set of parameters.")
  if (length(xmat) > length(algoName) && !is.null(algoName)) {
    print("unsufficient names provides: outstanding instances autofilled")
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
  col = c("black", "red", "blue", "green")
  col = col[1:nresults]

  x1 = seq(x1.lower, x1.upper, length.out = n.x)
  x2 = seq(x2.lower, x2.upper, length.out = n.x)

  plot.points.df = data.frame(mapply(function(u, v, w) cbind.data.frame(u, data.frame(name = v, col = w, stringsAsFactors = FALSE)),
    u = xmat, v = algoName, w = col)[[1]])

  # plot.points.z = lapply(xmat, function(x) matrix(apply(expand.grid(x$x1, x$x2), 1, f), ncol = length(x$x1)))


  plot.points = mapply(function(x, y, z) data.frame(x, algo = y, col = z, stringsAsFactors = FALSE), x = xmat, y = algoName,
    z = col, SIMPLIFY = FALSE)

  plot.points = do.call(rbind, plot.points)

  z = matrix(apply(expand.grid(x1, x2), 1, f), ncol = length(x1))

  plot.title = getName(f)


  plot.3d = plot_ly(opacity = 0.75) %>%
    add_surface(x = x1, y = x2, z = z, #colorscale = list(c(0,1), c("lightgrey", "lightblue")),
      contours = list(
        z = list(
          show=TRUE,
          usecolormap=TRUE,
          highlightcolor="#ff0000",
          project=list(z=TRUE)
        )
      ), showlegend = FALSE) #%>%
    # add_annotations(text = "", xref="paper", yref="paper", x=1.05, xanchor="left",
    #   y=1, yanchor="bottom", legendtitle=FALSE, showarrow=FALSE) %>%
    # layout(legend = TRUE)

    # layout(
    #   title = plot.title,
    #   scene = list(
    #     xaxis = list(title = "x1"),
    #     yaxis = list(title = "x2"),
    #     zaxis = list(title = "y"),
    #     color = c("grey")
    #   )) %>%
  #
  for(i in 1:length(xmat)){
   plot.3d <- plot.3d %>% add_trace(data = data.frame(xmat[[i]]), type = "scatter3d", x = ~x1, y = ~x2, z = ~y,
      mode = "markers", marker = list(size = 4, color = col[i], line = list(
        color = 'rgb(220, 220, 220)',
        width = 2
      )
      ), name = paste(names(xmat)[i]))#, showlegend = FALSE
   }

  #
  # add_trace(data = plot.points, type = "scatter3d", x = ~x2, y = ~x1, z = ~y,
  #   mode = "markers", symbol = ~algo, marker = list(size = 5)) %>%
  #
    # add_trace(data = plot.points, type = "scatter3d", x = ~x2, y = ~x1, z = ~y,
    #   mode = "markers", symbol = ~algo, marker = list(size = 5)) %>%

  return(plot.3d)
}
