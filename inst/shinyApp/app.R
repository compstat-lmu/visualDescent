require(shiny)
require(shinydashboard)
require(shinyjs)
require(shinybusy)
library(visualDescent)

# Define default settings
# shiny app on server throws same errors as local application. By default this is not true, so the app deployed on server
# throws a unified error message, i.e. 'error has occured, please contact package author'. To ensure that the errors are also
# thrown on the server set option to 'FALSE'.
options(shiny.sanitize.errors = FALSE)


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

# Specify examples: this list can be extended. It is important to specify all parameters and set the parameters which
# are not needed for the optimization to 0 (do not simply drop them).
examples = list(Scenario1 = list(method = c("GradientDescent", "Momentum"), step.size = 0.002, max.iter = 200,
  phi = 0.9, phi1 = 0, phi2 = 0, fun = c("StyblinkskiTang"), startx1 = 5, startx2 = -5),
  Scenario2 = list(method = c("GradientDescent", "Momentum"), step.size = 0.018, max.iter = 200,
    phi = 0.15, phi1 = 0, phi2 = 0, fun = c("Paraboloid"), startx1 = - 5, startx2 = - 5),
  Scenario3 = list(method = c("Adam", "AdaGrad", "GradientDescent", "Momentum"), step.size = 0.018, max.iter = 200,
    phi = 0.2, phi1 = 0.75, phi2 = 0.65, fun = c("Paraboloid"), startx1 = - 5, startx2 = -5),
  Scenario4 = list(method = c("Momentum"), step.size = 0.002, max.iter = 100,
    phi = 0.5, phi1 = 0, phi2 = 0, fun = c("HyperbolicParaboloid"), startx1 = -5, startx2 = 0))



ui <- dashboardPage(dashboardHeader(),
  dashboardSidebar(

    actionButton("run", "Update Window", icon("paper-plane"),
      style="color: #fff; background-color: #0aa321; border-color: #2e6da4"),
    #fluidRow(

    # Specify action buttons for the examples. If list of examples (i.e. 'examples') is extended, add another action button here.
    # column(3,

    # h4("Option A: See example optimization runs:", align= "left"),
    # h6("Example 1: Convergence of GD with and without momentum for an ill-conditioned function", align= "left"),
    # actionButton("example1", "Example 1"),
    # h6("Example 2: Convergence of GD, Momentum, Adagrad and Adam for an ill-conditioned function", align= "left"),
    # actionButton("example2", "Example 2"),
    # h6("Example 3: GD with and without momentum: Capability of escaping local minima", align= "left"),
    # actionButton("example3", "Example 3"),
    # h6("Example 4: GD with momentum: Terminating in saddle point", align= "left"),
    # actionButton("example4", "Example 4")
    hr(),
    h4("Opt. A: Predefined Examples", align= "left"),
    selectInput("example", "Choose predefined example",
      choices = list(" " = "null", "Example 1" = "example1", "Example 2" = "example2", "Example 3" = "example3",
        "Example 4" = "example4"), selected = "null", selectize = FALSE),
    conditionalPanel(condition = "input.example == 'example1'", textOutput("text_example1")),
    conditionalPanel(condition = "input.example == 'example2'", textOutput("text_example2")),
    conditionalPanel(condition = "input.example == 'example3'", textOutput("text_example3")),
    conditionalPanel(condition = "input.example == 'example4'", textOutput("text_example4")),

    hr(),
    # Specify slider inputs for playing around. Theses are defined as 'uiOutput' since they need to take on values when the parameters
    # are set by e.g. pre-defined examples. They react to changes in the valiables and are permanently updated.
    #  column(3,
    h4("Opt. B: Play around", align= "left"),
    uiOutput("fun"),
    uiOutput("method"),
    uiOutput("x1coords"),
    uiOutput("x2coords"),
    uiOutput("step.size"),
    uiOutput("max.iter"),
    uiOutput("phi"),
    uiOutput("phi1"),
    uiOutput("phi2"),#))

    hr(),
    actionButton("exportPlot", "Export Plot as png", icon("paper-plane")),
    textInput("filePath", label = "File path for plot export", value = "Enter file path for saving plots here"),
    uiOutput("message")
  ),
  dashboardBody(
    # for loading information (or add_busy_bar(color = "#FF0000"))
    add_busy_gif(
      src = "https://jeroen.github.io/images/banana.gif",
      height = 70, width = 70
    ),

    conditionalPanel(condition = "input.run == 0", textOutput("text_update")),
    # Specify the order of the plots of the application and their repsective size.
    textOutput("text"),
    plotOutput("plotLoss", width = "650px", height = "350px"),
    plotOutput("plot2d", width = "650px", height = "350px"),
    plotlyOutput("plot3d", width = "650px", height = "350px")))




server <- function(input, output, session){

  output$text_update <- renderText({
    "Set (new) selections and commit settings to plot by pressing [Update Window]. The button has to be pushed again in case
    of any changes in the settings."
  })

  output$text_example1 <- renderText({
    "Convergence of GD with and without momentum for an ill-conditioned function."
  })

  output$text_example2 <- renderText({
    "Convergence of GD, Momentum, Adagrad and Adam for an ill-conditioned function."
  })

  output$text_example3 <- renderText({
    "GD with and without momentum: Capability of escaping local minima."
  })

  output$text_example4 <- renderText({
    "GD with momentum: Terminating in saddle point."
  })

  Reactives = reactiveValues()

  observeEvent(
    input$example, {
      if(input$example != "null"){
        ex <- input$example
        if(ex == "example1"){
          updateCheckboxInput(session, "method", value = examples$Scenario2$method)
          updateSliderInput(session, "step.size", value = examples$Scenario2$step.size)
          updateSliderInput(session, "max.iter", value = examples$Scenario2$max.iter)
          updateSliderInput(session, "phi", value = examples$Scenario2$phi)
          updateSliderInput(session, "phi1", value = examples$Scenario2$phi1)
          updateSliderInput(session, "phi2", value = examples$Scenario2$phi2)
          updateSliderInput(session, "startx1", value = examples$Scenario2$startx1)
          updateSliderInput(session, "startx2", value = examples$Scenario2$startx2)
          updateSelectInput(session, "fun", selected = examples$Scenario2$fun)
        }
        else if(ex == "example2"){
          updateSelectInput(session, "fun", selected = examples$Scenario3$fun)
          updateCheckboxInput(session, "method", value = examples$Scenario3$method)
          updateSliderInput(session, "step.size", value = examples$Scenario3$step.size)
          updateSliderInput(session, "max.iter", value = examples$Scenario3$max.iter)
          updateSliderInput(session, "phi", value = examples$Scenario3$phi)
          updateSliderInput(session, "phi1", value = examples$Scenario3$phi1)
          updateSliderInput(session, "phi2", value = examples$Scenario3$phi2)
          updateSliderInput(session, "startx1", value = examples$Scenario3$startx1)
          updateSliderInput(session, "startx2", value = examples$Scenario3$startx2)
        }
        else if(ex == "example3"){
          updateCheckboxInput(session, "method", value = examples$Scenario1$method)
          updateSliderInput(session, "step.size", value = examples$Scenario1$step.size)
          updateSliderInput(session, "max.iter", value = examples$Scenario1$max.iter)
          updateSliderInput(session, "phi", value = examples$Scenario1$phi)
          updateSliderInput(session, "phi1", value = examples$Scenario1$phi1)
          updateSliderInput(session, "phi2", value = examples$Scenario1$phi2)
          updateSliderInput(session, "startx1", value = examples$Scenario1$startx1)
          updateSliderInput(session, "startx2", value = examples$Scenario1$startx2)
          updateSelectInput(session, "fun", selected = examples$Scenario1$fun)
        }
        else if(ex == "example4"){
          updateCheckboxInput(session, "method", value = examples$Scenario4$method)
          updateSliderInput(session, "step.size", value = examples$Scenario4$step.size)
          updateSliderInput(session, "max.iter", value = examples$Scenario4$max.iter)
          updateSliderInput(session, "phi", value = examples$Scenario4$phi)
          updateSliderInput(session, "phi1", value = examples$Scenario4$phi1)
          updateSliderInput(session, "phi2", value = examples$Scenario4$phi2)
          updateSliderInput(session, "startx1", value = examples$Scenario4$startx1)
          updateSliderInput(session, "startx2", value = examples$Scenario4$startx2)
          updateSelectInput(session, "fun", selected = examples$Scenario4$fun)
        }
      }
      else if(input$example == "null"){
        NULL
      }
    })

  observe({

    req(input$fun, input$method, input$step.size, input$max.iter, input$phi, input$phi1, input$phi2)

    Reactives$plot = isolate(get(input$fun, funData))
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

  # observeEvent(input$run, {
  #   if(input$run == 0){
  #    # shinyjs::show("text_update", anim = TRUE)
  #     NULL
  #   }
  #   else{
  #     shinyjs::hide("text_update", animType = "fade")
  #   }
  # })

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
        "AdaGrad", "Adam", "RMS", "NAG"),
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

  })




  observeEvent(input$run, {

    plot = isolate(get(input$fun, funData))
    x1 = isolate(input$startx1)
    x2 = isolate(input$startx2)
    step.size = isolate(input$step.size)
    max.iter = isolate(input$max.iter)
    method = isolate(input$method)
    phi = isolate(input$phi)
    phi1 = isolate(input$phi1)
    phi2 = isolate(input$phi2)
    fun = isolate(input$fun)

    # output$text = renderText({
    #   c(x1, x2)
    # })

    if ("GradientDescent" %in% method) {
      res = gradDescent(plot, c(x1, x2), step.size = step.size,
        max.iter = max.iter)
      resultsGD = res$results
      errorGD = res$errorOccured

    } else {
      resultsGD = c(0,0,0)
      errorGD = FALSE
    }

    if ("Momentum" %in% method) {
      res = gradDescentMomentum(plot, c(x1, x2), step.size = step.size,
        max.iter = max.iter, phi = phi)
      resultsMomentum = res$results
      errorMomentum = res$errorOccured

    } else {
      resultsMomentum = c(0,0,0)
      errorMomentum = FALSE
    }

    if ("AdaGrad" %in% method) {
      res = adaGrad(plot, c(x1, x2), step.size = step.size,
        max.iter = max.iter)
      resultsAdaGrad = res$results
      errorAdaGrad = res$errorOccured

    } else {
      resultsAdaGrad = c(0,0,0)
      errorAdaGrad = FALSE

    }

    if ("Adam" %in% method) {
      res = adam(plot, c(x1, x2), step.size = step.size,
        max.iter = max.iter, phi1 = phi1, phi2 = phi2)
      resultsAdam = res$results
      errorAdam = res$errorOccured

    } else {
      resultsAdam = c(0,0,0)
      errorAdam = FALSE
    }
    if ("RMS" %in% method) {
      res = RMSprop(plot, c(x1, x2), step.size = step.size,
        max.iter = max.iter)
      resultsRMS = res$results
      errorRMS = res$errorOccured

    } else {
      resultsRMS = c(0,0,0)
      errorRMS = res$errorOccured
    }
    if ("NAG" %in% method) {
      res = NAG(plot, c(x1, x2), step.size = step.size,
        max.iter = max.iter, phi = phi)
      resultsNAG = res$results

    } else {
      resultsNAG = c(0,0,0)
      errorNAG = res$errorOccured
    }
    results = list(GradientDescent = resultsGD, Momentum = resultsMomentum, AdaGrad = resultsAdaGrad, Adam = resultsAdam,
      RMS = resultsRMS, NAG = resultsNAG)
    errors = list(GradientDescent = errorGD, Momentum = errorMomentum, AdaGrad = errorAdaGrad, Adam = errorAdam,
      RMS = errorRMS, NAG = errorNAG)
    results = results[method]
    errors = get(method, errors)



    # # })
    # #
    # # observeEvent(input$run, {
    # #
    # #           req(Reactives$results, input$fun)
    # #           results = isolate(Reactive$results)
    # #           fun = isolate(input$fun)
    #

    Reactives$plot2d = plot2d(plot, get(fun, xValues)[1], get(fun, xValues)[3],
      get(fun, xValues)[2], get(fun, xValues)[4],
      trueOpt = c(as.numeric(get(fun, opt)$x1), as.numeric(get(fun, opt)$x2)),
      xmat = results, algoName = method, optimError = errors)

    Reactives$plotLoss = plotLoss(plot, get(fun, xValues)[1], get(fun, xValues)[3],
      get(fun, xValues)[2], get(fun, xValues)[4],
      xmat = as.list(results), algoName = as.list(method),
      optimError = errors)

    Reactives$plot3d =  plot3d(plot, get(fun, xValues)[1], get(fun, xValues)[3],
      get(fun, xValues)[2], get(fun, xValues)[4],
      trueOpt = c(as.numeric(get(fun, opt)$x1), as.numeric(get(fun, opt)$x2)),
      xmat = results, algoName = method, optimError = errors)

    Reactives$plotExport = grid.arrange(Reactives$plotLoss, Reactives$plot2d, ncol = 2)

    output$plot2d = renderPlot({
      Reactives$plot2d
    })
    output$plotLoss = renderPlot({
      #   input$run
      # Sys.sleep(3)
      Reactives$plotLoss
    })
    output$plot3d = renderPlotly({
      Reactives$plot3d
    })

  })

  observeEvent(input$exportPlot, {

    errorPath = FALSE
    tryCatch({

      ggsave(path = input$filePath, filename = "plot.png", plot = Reactives$plotExport, width = 30, height = 12,
        units = "cm", dpi = 600)

    },
      error = function(contd) {
        errorPath <<- TRUE
      }, finally = {
        if (errorPath == TRUE) {
          output$message = renderText({
            warning(c("File path does not exist. Please make sure path is correctly specified.",
              "Note that saving option does only work for local deployed application (i.e. not on server)"))
          })
        }
        else {
          output$message = renderText({
            warning(c("File successfully saved"))
          })
        }
      }
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)

