library(shiny)
library(visualDescent)

# Define default settings
# shiny app on server throws same errors as local application. By default this is not true, so the app deployed on server
# throws a unified error message, i.e. 'error has occured, please contact package author'. To ensure that the errors are also
# thrown on the server set option to 'FALSE'.
options(shiny.sanitize.errors = FALSE)

# Define general parameters of functions
# funData = readRDS("funData.rda")

#added new objective functions, read in via
funData = readRDS("objFunData.RDS")
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
  Scenario4 = list(method = c("GradientDescent", "Momentum"), step.size = 0.002, max.iter = 200,
    phi = 0.5, phi1 = 0, phi2 = 0, fun = c("Schwefel"), startx1 = 200, startx2 = 300))
  # Scenario4 = list(method = c("Momentum"), step.size = 0.002, max.iter = 100,
  #   phi = 0.5, phi1 = 0, phi2 = 0, fun = c("HyperbolicParaboloid"), startx1 = -5, startx2 = 0))



ui <- dashboardPage(dashboardHeader(),
  dashboardSidebar(

    actionButton("run", "Update Window", icon("paper-plane"),
      style="color: #fff; background-color: #0aa321; border-color: #2e6da4"),
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
    # are set by e.g. pre-defined examples. They react to changes in the variables and are permanently updated.
    h4("Opt. B: Play around", align= "left"),
    uiOutput("fun"),
    uiOutput("method"),
    uiOutput("x1coords"),
    uiOutput("x2coords"),
    uiOutput("step.size"),
    uiOutput("max.iter"),
    uiOutput("phi"),
    uiOutput("phi1"),
    uiOutput("phi2"),
    # uiOutput("trace"),

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

    # Specify the order of the plots of the application and their respective size.
    textOutput("text"),
    fluidRow(with = 12,
      box(tabBox(
        title = "Info on selected function",
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", width = "650px", height = "350px",
        tabPanel("Function",
          conditionalPanel(condition = "input.fun == 'Ackley2d'", uiOutput(("text_ackley"))),
          conditionalPanel(condition = "input.fun == 'DeflectedCorrugatedSpring'", uiOutput(("text_spring"))),
          conditionalPanel(condition = "input.fun == 'Exponential'", uiOutput(("text_exponential"))),
          conditionalPanel(condition = "input.fun == 'Generalized-Drop-Wave'", uiOutput(("text_dropwave"))),
          conditionalPanel(condition = "input.fun == 'Giunta'", uiOutput(("text_giunta"))),
          conditionalPanel(condition = "input.fun == 'Himmelblau'", uiOutput(("text_himmelblau"))),
          conditionalPanel(condition = "input.fun == 'Hosaki'", uiOutput(("text_hosaki"))),
          conditionalPanel(condition = "input.fun == 'HyperEllipsoid'", uiOutput(("text_hyperellipsoid"))),
          conditionalPanel(condition = "input.fun == 'Paraboloid'", uiOutput(("text_paraboloid"))),
          conditionalPanel(condition = "input.fun == 'Periodic'", uiOutput(("text_periodic"))),
          conditionalPanel(condition = "input.fun == 'Rosenbrock'", uiOutput(("text_rosenbrock"))),
          conditionalPanel(condition = "input.fun == 'Schwefel'", uiOutput(("text_schwefel"))),
          conditionalPanel(condition = "input.fun == 'Sphere'", uiOutput(("text_sphere"))),
          conditionalPanel(condition = "input.fun == 'StyblinkskiTang'", uiOutput(("text_styblinski")))),
        tabPanel("2D Plot", plotOutput("plot2d_info", height = "320")),
        tabPanel("3D Plot", plotlyOutput("plot3d_info", height = "320px")))),
      box(plotOutput("plotLoss", width = "650px", height = "350px"))
    ),
    fluidRow(width = 12,
      box(plotlyOutput("plot2d", width = "650px", height = "350px")),
      box(plotlyOutput("plot3d", width = "650px", height = "350px")))
    # DT::dataTableOutput("dtxmat")
  )
)


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
    "GD with momentum: Behaviour near saddle point, terminating in saddle point (at first)."
  })

  output$text_ackley <- renderUI({
    #withMathJax("Ackley-2D Function: $$
    withMathJax("$$ \\begin{split}
    f_{\\text{Ackley-2D}}(\\mathbf{x}) =& -20 \\exp \\Bigl(-0.2 \\bigl(\\frac{1}{d} \\sum_{i=1}^d x_{i}^{2}\\bigl)^{1/2} \\Bigl) \\\\
     &-\\exp \\Bigl(\\frac{1}{d} \\sum_{i=1}^{d} \\cos(2 \\pi x_i)\\Bigl) + 20 + \\exp(1), \\\\[0.5cm]
      &\\text{  with dimension d = 2} \\\\[2cm]
     &\\text{Optimum at x1 = 0, x2 = 0}
     \\end{split} $$")
    # \\text{with } & a = 20, b = 0.2, c = 2 \\pi, d = dimension = 2
    # "f = -a * exp(-b)"
  })

  output$text_spring <- renderUI({
    withMathJax("$$ \\begin{split}
      f_{\\text{Deflected-Corrugated-Spring}}&(\\mathbf{x}) = 0.1 * \\sum_{i=1}^d (x_i -5)^2 -
      \\cos \\Bigl(5 * \\sqrt{\\sum_{i=1}^d (x_i -5)^2}\\Bigl), \\\\[0.5cm]
      &\\text{  with dimension d = 2} \\\\[2cm]
     &\\text{Optimum at x1 = 5, x2 = 5}
      \\end{split} $$")
  })

  output$text_exponential <- renderUI({
    withMathJax("$$ \\begin{split}
      f_{\\text{Exponential}}&(\\mathbf{x}) = - \\exp(-0.5 * \\sum_{i=1}^d x_i^2), \\\\[0.5cm]
      &\\text{  with dimension d = 2} \\\\[2cm]
      &\\text{Optimum at x1 = 0, x2 = 0}
      \\end{split} $$")
  })

  output$text_dropwave <- renderUI({
    withMathJax("$$ \\begin{split}
      f_{\\text{Drop-Wave}}&(\\mathbf{x}) = \\frac{1+\\cos \\Bigl(12 \\sqrt{x_1^2+x_2^2} \\Bigl)}{0.5 (x_1^2 + x_2^2) + 2}, \\\\[2cm]
     &\\text{Optimum at x1 = 0, x2 = 0}
      \\end{split} $$")
  })

  output$text_giunta <- renderUI({
    withMathJax("$$ \\begin{split}
      f_{\\text{Giunta}}(\\mathbf{x}) =& 0.6 + \\sum_{i=1}^d \\Bigl(\\sin^2 (1-\\frac{16}{15} x_i) - \\frac{1}{50} \\sin (4-\\frac{64}{15} x_i)
      - \\sin (1 - \\frac{16}{15} x_i) \\Bigl), \\\\[0.5cm]
      &\\text{  with dimension d = 2} \\\\[2cm]
     &\\text{Optimum at x1 = 0.4673, x2 = 0.4673}
      \\end{split} $$")
  })

  output$text_himmelblau <- renderUI({
    withMathJax("$$ \\begin{split}
    f_{\\text{Himmelblau}}&(\\mathbf{x}) = \\big(x_1^2 + x_2 - 11 \\bigl)\\big(x_1 + x_2^2 - 7 \\bigl), \\\\[2cm]
     &\\text{Optimum at x1 = 3, x2 = 2}
      \\end{split} $$")
  })

  output$text_hosaki <- renderUI({
    withMathJax("$$ \\begin{split}
    f_{\\text{Hosaki}}(\\mathbf{x}) =& \\Bigl(1 - 8 x_1 + 7 x_1^2 - 7 * \\frac{x_1^3}{3} + \\frac{x_1^4}{4}\\Bigl)
      \\cdot x_2^2  \\exp(-x_2), \\\\[2cm]
     &\\text{Optimum at x1 = 4, x2 = 2}
      \\end{split} $$")
  })

  output$text_hyperellipsoid <- renderUI({
    withMathJax("$$ \\begin{split}
      &f_{\\text{Hyper-Ellipsoid}}(\\mathbf{x}) = \\sum_{i=1}^{d} \\sum_{j=1}^{i} x_j^2, \\\\[2cm]
     &\\text{Optimum at x1 = 0, x2 = 0}
      \\end{split} $$")
  })

  # output$text_camel <- renderUI({
  #   withMathJax("$$ \\begin{split}
  #   f_{\\text{ThreeHumpCamel}}(\\mathbf{x}) =& 2 * x_1^2 - 1.05 * x_1^4 + \\frac{x_1^6}{6} + x_1 * x_2 + x_2^2, \\\\[2cm]
  #    &\\text{Optimum at x1 = 0, x2 = 0}
  #     \\end{split} $$")
  # })

  output$text_paraboloid <- renderUI({
    withMathJax("$$ \\begin{split}
      f_{\\text{Paraboloid}}(\\mathbf{x}) =&  2*x_1^2 + 50*x_2^2, \\\\[2cm]
     &\\text{Optimum at x1 = 0, x2 = 0}
      \\end{split} $$")

  })

  output$text_periodic <- renderUI({
    withMathJax("$$ \\begin{split}
    f_{\\text{Periodic}}(\\mathbf{x}) =  1 + \\sin(x_1)^2 + \\sin(x_2)^2 - 0.1 * \\exp(-x_1^2 - x_2^2)
      \\end{split} $$")
  })

  output$text_rosenbrock <- renderUI({
    withMathJax("$$ \\begin{split}
      f_{\\text{Rosenbrock}}&(\\mathbf{x}) = \\sum_{i=1}^{d-1} \\bigl(100 (x_i^2 - x_{i+1})\\bigl)^2 + \\bigl(x_i - 1\\bigl)^2, \\\\[2cm]
     &\\text{Optimum at x1 = 1, x2 = 1}
      \\end{split} $$")
  })

  output$text_schwefel <- renderUI({
    withMathJax("$$ \\begin{split}
      f_{\\text{Schwefel}}&(\\mathbf{x}) = 418.9829 d - \\sum_{i=1}^d x_i \\sin \\bigl(\\sqrt{x_i} \\bigl), \\\\[0.5cm]
      &\\text{  with dimension d = 2} \\\\[2cm]
     &\\text{Optimum at x1 = 421, x2 = 421}
      \\end{split} $$")
  })

  output$text_sphere<- renderUI({
    withMathJax("$$ \\begin{split}
      &f_{\\text{Sphere}}(\\mathbf{x}) = \\sum_{i=1}^d x_i^2, \\\\[0.5cm]
      &\\text{  with dimension d = 2} \\\\[2cm]
     &\\text{Optimum at x1 = 0, x2 = 0}
      \\end{split} $$")
  })

  output$text_styblinski <- renderUI({
    withMathJax("$$ \\begin{split}
      &f_{\\text{Styblinski-Tang}}(\\mathbf{x}) = \\frac{1}{2} \\sum_{i=1}^{d} (x_i^4 - 16 x_i^2 + 5 x_i), \\\\[2cm]
     &\\text{Optimum at x1 = -2.904, x2 = -2.904}
      \\end{split} $$")
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

    output$x1coords = renderUI({ sliderInput("startx1", HTML("Choose Start Point x<sub>1</sub>"),
      min = get(Reactives$fun, xValues)[1],
      max = get(Reactives$fun, xValues)[3],
      step = 0.01,
      value = if (is.null(input$startx1)) {
        get(Reactives$fun, xValues)[1]
      } else {
        Reactives$x1
      }
    )})

    output$x2coords = renderUI({ sliderInput("startx2", HTML("Choose Start Point x<sub>2</sub>"),
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
      choices = c("GradientDescent", "Momentum", "AdaGrad", "Adam", "RMS", # "NAG", "AdaDelta" something wrong in code?
        "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN"), #exported from optim funcion, package stats
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

    # output$trace = renderUI({ sliderInput("Trace Steps", HTML("Choose Start Point &theta;<sub>1</sub>"),
    #   min = 1,
    #   max = input$max.iter,
    #   step = 1,
    #   value = if (is.null(input$trace)) {
    #     input$max.iter
    #   } else {
    #     Reactives$trace
    #   }
    # )})
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
    ###
    # if ("AdaDelta" %in% method) {
    #   res = adaDelta(plot, c(x1, x2), gamma = 0.95,
    #     max.iter = max.iter)
    #   resultsAdaDelta = res$results
    #   errorAdaDelta = res$errorOccured
    #
    # } else {
    #   resultsAdaDelta = c(0,0,0)
    #   errorAdaDelta = res$errorOccured
    # }
    ###
    # if ("NAG" %in% method) {
    #   res = NAG(plot, c(x1, x2), step.size = step.size,
    #     max.iter = max.iter, phi = phi)
    #   resultsNAG = res$results
    #
    # } else {
    #   resultsNAG = c(0,0,0)
    #   errorNAG = res$errorOccured
    # }
    ###
    if ("Nelder-Mead" %in% method) {
      res = optimStats(plot, c(x1, x2), method = "Nelder-Mead",
        max.iter = max.iter)
      resultsNelderMead = res$results
      errorNelderMead = res$errorOccured

    } else {
      resultsNelderMead = c(0,0,0)
      errorNelderMead = res$errorOccured
    }

    if ("BFGS" %in% method) {
      res = optimStats(plot, c(x1, x2), method = "BFGS",
        max.iter = max.iter)
      resultsBFGS = res$results
      errorBFGS = res$errorOccured

    } else {
      resultsBFGS = c(0,0,0)
      errorBFGS = res$errorOccured
    }

    if ("L-BFGS-B" %in% method) {
      res = optimStats(plot, c(x1, x2), method = "L-BFGS-B",
        max.iter = max.iter)
      resultsLBFGSB = res$results
      errorLBFGSB = res$errorOccured

    } else {
      resultsLBFGSB = c(0,0,0)
      errorLBFGSB = res$errorOccured
    }

    if ("CG" %in% method) {
      res = optimStats(plot, c(x1, x2), method = "CG",
        max.iter = max.iter)
      resultsCG = res$results
      errorCG = res$errorOccured

    } else {
      resultsCG = c(0,0,0)
      errorCG = res$errorOccured
    }

    if ("SANN" %in% method) {
      res = optimStats(plot, c(x1, x2), method = "SANN",
        max.iter = max.iter)
      resultsSANN = res$results
      errorSANN = res$errorOccured

    } else {
      resultsSANN = c(0,0,0)
      errorSANN = res$errorOccured
    }

    results = list(GradientDescent = resultsGD, Momentum = resultsMomentum, AdaGrad = resultsAdaGrad, Adam = resultsAdam,
      RMS = resultsRMS, 'Nelder-Mead' = resultsNelderMead, BFGS = resultsBFGS, 'L-BFGS-B' = resultsLBFGSB,
      CG = resultsCG, SANN = resultsSANN) # AdaDelta = resultsAdaDelta, NAG = resultsNAG,
    errors = list(GradientDescent = errorGD, Momentum = errorMomentum, AdaGrad = errorAdaGrad, Adam = errorAdam,
      RMS = errorRMS, 'Nelder-Mead' = errorNelderMead, BFGS = errorBFGS,'L-BFGS-B' = errorLBFGSB,
      CG = errorCG, SANN = errorSANN) # AdaDelta = errorAdaDelta, NAG = errorNAG,
    results = results[method]
    errors = get(method, errors)


    Reactives$plot2d_info = plot2d_info(plot, get(fun, xValues)[1], get(fun, xValues)[3],
      get(fun, xValues)[2], get(fun, xValues)[4],
      trueOpt = c(as.numeric(get(fun, opt)$x1), as.numeric(get(fun, opt)$x2)),
      xmat = results, algoName = method, optimError = errors)

    Reactives$plotLoss = plotLoss(plot, get(fun, xValues)[1], get(fun, xValues)[3],
      get(fun, xValues)[2], get(fun, xValues)[4],
      xmat = as.list(results), algoName = as.list(method),
      optimError = errors)

    Reactives$plot2d =  plot2d(plot, get(fun, xValues)[1], get(fun, xValues)[3],
      get(fun, xValues)[2], get(fun, xValues)[4],
      trueOpt = c(as.numeric(get(fun, opt)$x1), as.numeric(get(fun, opt)$x2)),
      xmat = results, algoName = method, optimError = errors)

    Reactives$plot3d =  plot3d(plot, get(fun, xValues)[1], get(fun, xValues)[3],
      get(fun, xValues)[2], get(fun, xValues)[4],
      trueOpt = c(as.numeric(get(fun, opt)$x1), as.numeric(get(fun, opt)$x2)),
      xmat = results, algoName = method, optimError = errors)

    Reactives$plot3d_info =  plot3d.info(plot, get(fun, xValues)[1], get(fun, xValues)[3],
      get(fun, xValues)[2], get(fun, xValues)[4],
      trueOpt = c(as.numeric(get(fun, opt)$x1), as.numeric(get(fun, opt)$x2)))

    Reactives$plotExport = grid.arrange(Reactives$plotLoss, Reactives$plot2d_info, ncol = 2)

    output$plot2d_info = renderPlot({
      Reactives$plot2d_info
    })
    output$plotLoss = renderPlot({
      Reactives$plotLoss
    })
    output$plot2d = renderPlotly({
      Reactives$plot2d
    })
    output$plot3d = renderPlotly({
      Reactives$plot3d # [["plot"]]
    })
    output$plot3d_info = renderPlotly({
      Reactives$plot3d_info
    })

    # output$dtxmat = DT::renderDataTable(Reactives$plot3d[["xmat"]][[1]], server = FALSE)

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

