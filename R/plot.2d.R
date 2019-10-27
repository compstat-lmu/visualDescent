#' Visualize optimization procedure in 2d contour plot
#'
#' This functions visualizes the steps of the optimization in a 2 dimensional contour plot.
#'
#' @param f a (multi-) dimensional function to be eptimized.
#' @param x1.lower the lower boundary for the range of the x1 coordinate displayed.
#' @param x1.upper the upper boundary for the range of the x1 coordinate displayed.
#' @param x2.lower the lower boundary for the range of the x2 coordinate displayed.
#' @param x2.upper the upper boundary for the range of the x2 coordinate displayed.
#' @param n.x the number (equidistant) points at which f is evaluated w.r.t x1 and x2 coordinate.
#' @param xmat an object of class
#'
#' @export
plot.2d = function(f, x1.lower, x1.upper, x2.lower, x2.upper, n.x, xmat){

  # Modify 'xmat' to match 3rd coordinate (set to 0)
  xmat$y = 0

  # Generate grid with specfied dimensions and data.frame for plot
  x1 = seq(x1.lower, x1.upper, length.out = n.x)
  x2 = seq(x2.lower, x2.upper, length.out = n.x)

  df = expand.grid(x1, x2)
  y = apply(df, 1, f)

  plot.df = data.frame(df, y)
  plot.title = capture.output(f)[1] %>%
            str_extract_all("(?<=\\{).+?(?=\\})")

  names(plot.df) = c("x1", "x2", "y")

  # Generate contour plot and add steps of descent procedure as points
  plot = ggplot(plot.df, aes(x1, x2, z = y)) +
    stat_contour(bins = n.x, aes(colour = stat(level))) +
    geom_point(data = xmat, colour = "red") +
    ggtitle(paste(plot.title)) +
    theme(plot.title = element_text(hjust = .5))

  # Return ggplot object
  return(plot)
}
