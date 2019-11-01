#
library(shiny)

# Define UI for application ggplot
ui <- fluidPage(

        titlePanel(title=h4("Steps optimization procedure", align="center")),

        sidebarLayout(position = "left", sidebarPanel(
        sliderInput("step.size", "Step size:", min = 0, max = 1, step = 0.05, value= 0.1),
        sliderInput("max.iter", "Maximum number of iterations:",min = 0, max = 200, step=1, value= 10),
        sliderInput("phi", HTML("Velocity parameter &Phi;:"),min = 0, max = 1, step=0.05, value= 0.1),

        selectInput("functions", "Choose predefined function", choices = c("Function 1", "Function 2"))),

        mainPanel(column(2, plotOutput("plot", width = "700px", height = "500px"))))
      )

# Define server logic required to draw a histogram
server <- function(input, output, session){

  funData = data.frame(fun1 = testfun, fun2 = testfun1)

  observe({
    functionData = input$functions
  })

  functionsStorage =

  output$plot = renderPlot({
                    plot2d(input$functions, -4, 4, -4, 4, 30,
                      gradDescentMomentum(input$functions, c(-4, -4), step.size = input$step.size,
                                          max.iter = input$max.iter, phi = input$phi)$results)})

}

# Run the application
shinyApp(ui = ui, server = server)

