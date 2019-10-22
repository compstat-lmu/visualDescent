library(plotly)

plot.3d = function(f, x1.lower, x1.upper, x2.lower, x2.upper, n.x, xmat) {


  x1 = seq(x1.lower, x1.upper, length.out = n.x)
  x2 = seq(x2.lower, x2.upper, length.out = n.x)
  y = outer(x1, x2, f)

  plot.title = capture.output(f)[1] %>%
    str_extract_all("(?<=\\{).+?(?=\\})")

  plot = plot_ly() %>%
    add_surface(x = x1, y = x2, z = y, colorscale = list(c(0,1), c("lightgrey", "gray"))) %>%
    layout(
      title = plot.title,
      scene = list(
        xaxis = list(title = "x1"),
        yaxis = list(title = "x2"),
        zaxis = list(title = "y"),
        color = c("grey")
    )) %>%
    add_trace(type = "scatter3d", x = xmat$x1, y = xmat$x2, z = xmat$y,
              mode = "markers", marker = list(color = "red")) %>%
    add_annotations(text = "y", xref="paper", yref="paper", x=1.05, xanchor="left",
                    y=1, yanchor="bottom", legendtitle=FALSE, showarrow=FALSE)

  return(plot)

}
