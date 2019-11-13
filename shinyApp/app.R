library(smoof)
library(shiny)
library(visualDescent)


ui <- fluidPage(

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


readRDS("funData.rds")

server <- function(input, output, session){

  Reactives = reactiveValues()

  observe({
    if(input$functions == "Function 1"){
      Reactives$plot <<- funData$Ackley2d
    }
    if(input$functions == "Function 2"){
      Reactives$plot <<- funData$Beale
    }})

  observe({
    Reactives$x1 <<- as.numeric(input$startx1)
    Reactives$x2 <<- as.numeric(input$startx2)
  })

  observe({
    Reactives$phi1 <<- input$phi1
    Reactives$phi2 <<- input$phi2
  })

  observe({
    if(input$method == "Gradien Descent"){
            output$plot = renderPlot({
                            plot2d(Reactives$plot, -4, 4, -4, 4, 30,
                                    gradDescent(Reactives$plot, c(Reactives$x1, Reactives$x2), step.size = input$step.size,
                                                          max.iter = input$max.iter)$results)})
    }
    if(input$method == "Momentum"){
      output$plot = renderPlot({
        plot2d(Reactives$plot, -4, 4, -4, 4, 30,
               gradDescentMomentum(Reactives$plot, c(Reactives$x1, Reactives$x2), step.size = input$step.size,
                           max.iter = input$max.iter, phi = input$phi)$results)})
    }
    if(input$method == "AdaGrad"){
      output$plot = renderPlot({
        plot2d(Reactives$plot, -4, 4, -4, 4, 30,
               adaGrad(Reactives$plot, c(Reactives$x1, Reactives$x2), step.size = input$step.size,
                                   max.iter = input$max.iter)$results)})
    }
    if(input$method == "Adam"){
      output$plot = renderPlot({
        plot2d(Reactives$plot, -4, 4, -4, 4, 30,
               adam(Reactives$plot, c(Reactives$x1, Reactives$x2), step.size = input$step.size,
                       max.iter = input$max.iter, phi1 = Reactives$phi1, phi2 = Reactives$phi2)$results)})
    }})
}

# Run the application
shinyApp(ui = ui, server = server)

