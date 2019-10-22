library(ggplot2)
library(RColorBrewer)
library(reshape2)
library(dplyr)

plot.2d = function(f, x1.lower, x1.upper, x2.lower, x2.upper, n.x, xmat){

  # Modify 'xmat' to match 3rd coordinate (set to 0)
  xmat$y = 0

  # Generate grid with specfied dimensions and data.frame for plot
  x1 = seq(x1.lower, x1.upper, length.out = n.x)
  x2 = seq(x2.lower, x2.upper, length.out = n.x)
  y = outer(x1, x2, f)

  colnames(y) = x1
  rownames(y) = x2

  plot.title = capture.output(f)[1] %>%
            str_extract_all("(?<=\\{).+?(?=\\})")

  plot.df = melt(y)
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
