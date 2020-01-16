library(shiny)
library(visualDescent)

# Define general parameters of functions
funData = readRDS("funData.rda")
funNames = as.list(names(funData))
xValues = lapply(funData, function(x) {lower = getLowerBoxConstraints(x)
                                        upper = getUpperBoxConstraints(x)
                                        return(c(lower = lower, upper = upper))})
opt = lapply(funData, function(x) {y = list(x1 = getGlobalOptimum(x)$param[1],
                                    x2 = getGlobalOptimum(x)$param[2],
                                    z = getGlobalOptimum(x)$value)
                                    return(y)})

examples = list(Scenario1 = list(method = c("GradientDescent", "Momentum"), step.size = 0.002, max.iter = 200,
                                 phi = 0.9, phi1 = 0, phi2 = 0, fun = c("StyblinkskiTang"), startx1 = 5, startx2 = -5),
                Scenario2 = list(method = c("GradientDescent", "Momentum"), step.size = 0.018, max.iter = 200,
                                 phi = 0.15, phi1 = 0, phi2 = 0, fun = c("Paraboloid"), startx1 = - 5, startx2 = - 5),
                Scenario3 = list(method = c("Adam", "AdaGrad", "GradientDescent", "Momentum"), step.size = 0.018, max.iter = 200,
                                 phi = 0.2, phi1 = 0.75, phi2 = 0.65, fun = c("Paraboloid"), startx1 = - 5, startx2 = -5))


ui <- fluidPage(
        actionButton("run", "Update Window"),
        fluidRow(
        column(3,
                        h4("Option A: See example optimization runs:", align= "left"),
                        h6("Example 1: Convergence of GD with and without momentum for an ill-conditioned function", align= "left"),
                        actionButton("example1", "Example 1"),
                        h6("Example 2: Convergence of GD, Momentum, Adagrad and Adam for an ill-conditioned function", align= "left"),
                        actionButton("example2", "Example 2"),
                        h6("Example 3: GD with and without momentum: Capability of escaping local minima", align= "left"),
                        actionButton("example3", "Example 3")),

                        # checkboxGroupInput("examples", "Choose Example Configuration (Optional)", choices = c("Scenario1", "Scenario2",
        column(3,
                        h4("Option B: Play around", align= "left"),
                        uiOutput("fun"),
                        uiOutput("method"),
                        uiOutput("x1coords"),
                        uiOutput("x2coords"),
                        uiOutput("step.size"),
                        uiOutput("max.iter"),
                        uiOutput("phi"),
                        uiOutput("phi1"),
                        uiOutput("phi2")),

        column(5,
                mainPanel(
                  # h3("Steps optimization procedure", align= "center"),
                  plotOutput("plotLoss", width = "650px", height = "350px"),
                  plotOutput("plot2d", width = "650px", height = "350px"),
                  plotlyOutput("plot3d", width = "650px", height = "350px")))

        ))



server <- function(input, output, session){


  Reactives = reactiveValues()

  observeEvent(
    input$example1, {
      updateCheckboxInput(session, "method", value = examples$Scenario2$method)
      updateSliderInput(session, "step.size", value = examples$Scenario2$step.size)
      updateSliderInput(session, "max.iter", value = examples$Scenario2$max.iter)
      updateSliderInput(session, "phi", value = examples$Scenario2$phi)
      updateSliderInput(session, "phi1", value = examples$Scenario2$phi1)
      updateSliderInput(session, "phi2", value = examples$Scenario2$phi2)
      updateSliderInput(session, "startx1", value = examples$Scenario2$startx1)
      updateSliderInput(session, "startx2", value = examples$Scenario2$startx2)
      updateSelectInput(session, "fun", selected = examples$Scenario2$fun)
    })

  observeEvent(
    input$example2, {
      updateSelectInput(session, "fun", selected = examples$Scenario3$fun)
      updateCheckboxInput(session, "method", value = examples$Scenario3$method)
      updateSliderInput(session, "step.size", value = examples$Scenario3$step.size)
      updateSliderInput(session, "max.iter", value = examples$Scenario3$max.iter)
      updateSliderInput(session, "phi", value = examples$Scenario3$phi)
      updateSliderInput(session, "phi1", value = examples$Scenario3$phi1)
      updateSliderInput(session, "phi2", value = examples$Scenario3$phi2)
      updateSliderInput(session, "startx1", value = examples$Scenario3$startx1)
      updateSliderInput(session, "startx2", value = examples$Scenario3$startx2)
    })

  observeEvent(
    input$example3, {
      updateCheckboxInput(session, "method", value = examples$Scenario1$method)
      updateSliderInput(session, "step.size", value = examples$Scenario1$step.size)
      updateSliderInput(session, "max.iter", value = examples$Scenario1$max.iter)
      updateSliderInput(session, "phi", value = examples$Scenario1$phi)
      updateSliderInput(session, "phi1", value = examples$Scenario1$phi1)
      updateSliderInput(session, "phi2", value = examples$Scenario1$phi2)
      updateSliderInput(session, "startx1", value = examples$Scenario1$startx1)
      updateSliderInput(session, "startx2", value = examples$Scenario1$startx2)
      updateSelectInput(session, "fun", selected = examples$Scenario1$fun)
    })

  observe({

    req(input$fun, input$method, input$step.size, input$max.iter, input$phi, input$phi1, input$phi2)

      Reactives$plot = get(input$fun, funData)
      Reactives$fun = input$fun
      Reactives$method = input$method
      Reactives$step.size = input$step.size
      Reactives$max.iter = input$max.iter
      Reactives$phi = input$phi
      Reactives$phi1 = input$phi1
      Reactives$phi2 = input$phi2
      Reactives$x1 = as.numeric(input$startx1)
      Reactives$x2 = as.numeric(input$startx2)

  })

  observe({
  output$fun = renderUI({selectInput("fun", "Choose Objective Function",
                                      choices = funNames, selected = if (is.null(input$examples)) {
                                                            "Ackley2d"
                                                            } else {
                                                              Reactives$fun
                                                            }
                                      )})

  output$x1coords = renderUI({ sliderInput("startx1", HTML("Choose Start Point &theta;<sub>1</sub>"),
                                          min = get(Reactives$fun, xValues)[1],
                                          max = get(Reactives$fun, xValues)[3],
                                          step = 0.01,
                                          value = if (is.null(input$startx1)) {
                                                            get(Reactives$fun, xValues)[1]
                                                            } else {
                                                              Reactives$x1
                                                            }
                                        )})

  output$x2coords = renderUI({ sliderInput("startx2", HTML("Choose Start Point &theta;<sub>2</sub>"),
                                           min = get(Reactives$fun, xValues)[2],
                                           max = get(Reactives$fun, xValues)[4],
                                           step = 0.01,
                                           value = if (is.null(input$startx2)) {
                                                            get(Reactives$fun, xValues)[2]
                                                            } else {
                                                              Reactives$x2
                                                            }
                                          )})


  output$method = renderUI({checkboxGroupInput("method", "Choose Optimizer(s)",
                                               choices = c("GradientDescent", "Momentum",
                                                            "AdaGrad", "Adam"),
                                               selected = if (is.null(input$method)) {
                                                            "GradientDescent"
                                                            } else {
                                                              Reactives$method
                                                            }
                                              )})


  output$step.size = renderUI({sliderInput("step.size", HTML("Step size &alpha; (all optimizers):"),
                                           min = 0, max = 0.5, step = 0.001,
                                           value =  if (is.null(input$step.size)) {
                                                      0.01
                                           } else {
                                             Reactives$step.size
                                           }
                                          )})

  output$max.iter = renderUI({sliderInput("max.iter", "Max. iterations T (all optimizers):",
                                          min = 0, max = 2000, step = 1,
                                          value = if (is.null(input$max.iter)) {
                                                      10
                                            } else {
                                              Reactives$max.iter
                                            }
                                          )})

  output$phi = renderUI({sliderInput("phi", HTML("Momentum &phi; :"),
                                     min = 0, max = 1, step = 0.05,
                                     value = if (is.null(input$phi)) {
                                       0.1
                                     } else {
                                       Reactives$phi
                                     }
                                       )})

  output$phi1 = renderUI({sliderInput("phi1", HTML("Decay Rate &rho;<sub>1</sub> (Adam):"),
                                      min = 0, max = 1, step=0.05,
                                      value= if (is.null(input$phi1)) {
                                        0.9
                                      } else {
                                        Reactives$phi1
                                      }
                                        )})

  output$phi2 = renderUI({sliderInput("phi2", HTML("Decay Rate &rho;<sub>2</sub> (Adam):"),
                                      min = 0, max = 1, step=0.05,
                                      value = if (is.null(input$phi2)) {
                                        0.95
                                      } else {
                                        Reactives$phi2
                                      }
  )})

  }, priority = 2)


  observeEvent(input$run, {
    # req(Reactives$plot, Reactives$x1, Reactives$x2, input$step.size, input$max.iter, input$method, Reactives$phi1, Reactives$phi2, input$fun)

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

  observeEvent(input$run, {

            req(Reactives$results, Reactives$fun)

            output$plot2d = renderPlot({

                            plot2d(Reactives$plot, get(Reactives$fun, xValues)[1], get(Reactives$fun, xValues)[3],
                                   get(Reactives$fun, xValues)[2], get(Reactives$fun, xValues)[4],
                                   trueOpt = c(as.numeric(get(Reactives$fun, opt)$x1), as.numeric(get(Reactives$fun, opt)$x2)),
                                    xmat = Reactives$results, algoName = Reactives$method)
                            })
            output$plotLoss = renderPlot({
                            plotLoss(Reactives$plot, get(Reactives$fun, xValues)[1], get(Reactives$fun, xValues)[3],
                                     get(Reactives$fun, xValues)[2], get(Reactives$fun, xValues)[4],
                                     xmat = as.list(Reactives$results), trueOptZ = c(as.numeric(get(Reactives$fun, opt)$z)),
                                     algoName = as.list(Reactives$method))

                            })
            output$plot3d = renderPlotly({
              plot3d(Reactives$plot, get(Reactives$fun, xValues)[1], get(Reactives$fun, xValues)[3],
                              get(Reactives$fun, xValues)[2], get(Reactives$fun, xValues)[4],
                              trueOpt = c(as.numeric(get(Reactives$fun, opt)$x1), as.numeric(get(Reactives$fun, opt)$x2)),
                              xmat = Reactives$results, algoName = Reactives$method)
                            })

        })
}

# Run the application
shinyApp(ui = ui, server = server)

