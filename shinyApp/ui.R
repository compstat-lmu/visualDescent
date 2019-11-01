#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

fluidPage(

  titlePanel(title=h4("Steps optimization procedure", align="center")),

  sidebarLayout(position = "left",
                sidebarPanel(
                  selectInput("method", "Choose optimization method", choices = c("Gradien Descent", "Momentum",
                                                                                  "AdaGrad", "Adam"), selected = "AdaGrad"),
                  selectInput("startx1", "Choose start point x1", choices = round(seq(-4, 4, by = 0.1), 2),
                              selected = 0),
                  selectInput("startx2", "Choose start point x2", choices = round(seq(-4, 4, by = 0.1), 2),
                              selected = -4),
                  selectInput("functions", "Choose predefined function", choices = c("Function 1", "Function 2"),
                              selected = "Function 2"),
                  sliderInput("step.size", "Step size:", min = 0, max = 1, step = 0.05, value= 0.1),
                  sliderInput("max.iter", "Maximum number of iterations:",min = 0, max = 200, step=1, value= 10),
                  conditionalPanel(condition = "input.method == 'Momentum'",
                                   sliderInput("phi", HTML("Velocity parameter &Phi;:"),min = 0, max = 1,
                                               step=0.05, value= 0.1)),
                  conditionalPanel(condition = "input.method == 'Adam'",
                                   sliderInput("phi1", HTML("Decay rate &Phi; 1:"),min = 0, max = 1,
                                               step=0.05, value= 0.5)),
                  conditionalPanel(condition = "input.method == 'Adam'",
                                   sliderInput("phi2", HTML("Decay rate &Phi; 2:"),min = 0, max = 1,
                                               step=0.05, value= 0.8)),
                  uiOutput("Params")),

                mainPanel(column(2, plotOutput("plot", width = "700px", height = "500px"))))
)
