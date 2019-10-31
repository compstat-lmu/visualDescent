library(plotly)

plot3d = function(f, x1.lower, x1.upper, x2.lower, x2.upper, n.x, xmat) {

  x1 = seq(x1.lower, x1.upper, length.out = n.x)
  x2 = seq(x2.lower, x2.upper, length.out = n.x)

  df.x = mesh(x1, x2)

  y = expand.grid(x1, x2)
  y = apply(df, 1, f) %>%
    cbind(y)
  names(y) = c("y", "x1", "x2")

  df.y = reshape(y, idvar = "x1", timevar = "x2", direction = "wide") %>%
    as.matrix()
  df.y = df.y[,-1]

  colnames(df.y) = round(x2, 3)
  rownames(df.y) = round(x1, 3)


  plot.title = capture.output(f)[1] %>%
    str_extract_all("(?<=\\{).+?(?=\\})")

  plot = surf3D(x = df.x$x, y = df.x$y , z = df.y, box )
    scatter3D(xmat$x1, xmat$x2, xmat$y)

  return(plot)

}
