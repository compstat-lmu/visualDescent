library(shiny)
library(visualDescent)

# Define general parameters of functions
funData = readRDS("funData.rda")
funNames = as.list(names(funData))
xValues = lapply(funData, function(x) {lower = getLowerBoxConstraints(x)
                                        upper = getUpperBoxConstraints(x)
                                        return(c(lower = lower, upper = upper))})
opt = lapply(funData, function(x) {x1 = getGlobalOptimum(x)$param[1]
                                    x2 = getGlobalOptimum(x)$param[2]
                                    return(c(x1, x2))})


ui <- fluidPage(


        fluidRow(
        column(3,
                        checkboxGroupInput("method", "Choose optimization methods", choices = c("GradientDescent", "Momentum",
                                                                          "AdaGrad", "Adam"), selected = "AdaGrad"),
                        uiOutput("x1coords"),
                        uiOutput("x2coords"),
                        selectInput("functions", "Choose predefined function", choices = funNames, selected = "Ackley2d"),

                        sliderInput("step.size", "Step size (all):", min = 0, max = 0.1, step = 0.001, value= 0.001),
                        sliderInput("max.iter", "Maximum number of iterations (all):",min = 0, max = 10000, step=1, value= 10),
                        sliderInput("phi", HTML("Velocity parameter &Phi; (Momentum):"),min = 0, max = 1, step=0.05, value= 0.1),
                        sliderInput("phi1", HTML("Decay rate &Phi; 1 (Adam):"),min = 0, max = 1, step=0.05, value= 0.9),
                        sliderInput("phi2", HTML("Decay rate &Phi; 2 (Adam):"),min = 0, max = 1, step=0.05, value= 0.95)),

        column(6,
                mainPanel(
                  # h3("Steps optimization procedure", align= "center"),
                  plotOutput("plot", width = "800px", height = "500px"))),

        column(9,
               mainPanel(
                 # h3("Algorithms", align = "center"),
                 img(src = "Algorithms.png", height = 400, width = 700)
               ))
        ))








server <- function(input, output, session){

  Reactives = reactiveValues()

  observe({
  Reactives$plot <<- get(input$functions, funData)
  })

  observe({
    req(input$functions)
  output$x1coords = renderUI({ sliderInput("startx1", "Choose start point x1",
                                          min = get(input$functions, xValues)[1],
                                          max = get(input$functions, xValues)[3],
                                          step = 0.01,
                                          value = get(input$functions, xValues)[1])})

  output$x2coords = renderUI({ sliderInput("startx2", "Choose start point x2",
                                           min = get(input$functions, xValues)[2],
                                           max = get(input$functions, xValues)[4],
                                           step = 0.01,
                                           value = get(input$functions, xValues)[2])})
  }, priority = 2)
  observe({
    Reactives$x1 <<- as.numeric(input$startx1)
    Reactives$x2 <<- as.numeric(input$startx2)
  }, priority = 3)

  observe({
    Reactives$phi1 <<- input$phi1
    Reactives$phi2 <<- input$phi2

  })

  observe({


    req(Reactives$plot, Reactives$x1, Reactives$x2, input$step.size, input$max.iter, input$method, Reactives$phi1, Reactives$phi2, input$functions)

    if ("GradientDescent" %in% input$method) {
      resultsGD = gradDescent(Reactives$plot, c(Reactives$x1, Reactives$x2), step.size = input$step.size,
                                      max.iter = input$max.iter)$results
    } else {
      resultsGD = c(0,0,0)
    }

    if ("Momentum" %in% input$method) {
      resultsMomentum = gradDescentMomentum(Reactives$plot, c(Reactives$x1, Reactives$x2), step.size = input$step.size,
                                                    max.iter = input$max.iter, phi = input$phi)$results
    } else {
      resultsMomentum = c(0,0,0)
    }

    if ("AdaGrad" %in% input$method) {
    resultsAdaGrad = adaGrad(Reactives$plot, c(Reactives$x1, Reactives$x2), step.size = input$step.size,
                             max.iter = input$max.iter)$results
    } else {
      resultsAdaGrad = c(0,0,0)
    }

    if ("Adam" %in% input$method) {
      resultsAdam = adam(Reactives$plot, c(Reactives$x1, Reactives$x2), step.size = input$step.size,
                       max.iter = input$max.iter, phi1 = Reactives$phi1, phi2 = Reactives$phi2)$results
    } else {
      resultsAdam = c(0,0,0)
    }
    results = list(GradientDescent = resultsGD, Momentum = resultsMomentum, AdaGrad = resultsAdaGrad, Adam = resultsAdam)
    Reactives$results = results[input$method]

  }, priority = 1)

  observe({
            req(Reactives$results, input$functions)

            output$plot = renderPlot({
                            plot2d(Reactives$plot, get(input$functions, xValues)[1], get(input$functions, xValues)[3],
                                   get(input$functions, xValues)[2], get(input$functions, xValues)[4],
                                   trueOpt = c(as.numeric(get(input$functions, opt)[1]), as.numeric(get(input$functions, opt)[2])),
                                    xmat = Reactives$results)
                            })
        }, priority = 0)
}

# Run the application
shinyApp(ui = ui, server = server)

