#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session){

  funData = list(fun1 = funData$testfun1, fun2 = funData$testfun2)

  observe({
    if(input$functions == "Function 1"){
      funPlot <<- funData$fun1
    }
    if(input$functions == "Function 2"){
      funPlot <<- funData$fun2
    }})

  observe({
    startx1 <<- as.numeric(input$startx1)
    startx2 <<- as.numeric(input$startx2)
  })

  observe({
    phi1 <<- as.numeric(input$phi1)
    phi2 <<- input$phi2
  })

  observe({
    if(input$method == "Gradien Descent"){
      output$plot = renderPlot({
        plot2d(funPlot, -4, 4, -4, 4, 30,
               gradDescent(funPlot, c(startx1, startx2), step.size = input$step.size,
                           max.iter = input$max.iter)$results)})
    }
    if(input$method == "Momentum"){
      output$plot = renderPlot({
        plot2d(funPlot, -4, 4, -4, 4, 30,
               gradDescentMomentum(funPlot, c(startx1, startx2), step.size = input$step.size,
                                   max.iter = input$max.iter, phi = input$phi)$results)})
    }
    if(input$method == "AdaGrad"){
      output$plot = renderPlot({
        plot2d(funPlot, -4, 4, -4, 4, 30,
               adaGrad(funPlot, c(startx1, startx2), step.size = input$step.size,
                       max.iter = input$max.iter)$results)})
    }
    if(input$method == "Adam"){
      output$plot = renderPlot({
        plot2d(funPlot, -4, 4, -4, 4, 30,
               adam(funPlot, c(startx1, startx2), step.size = input$step.size,
                    max.iter = input$max.iter, phi1 = phi1, phi2 = phi2)$results)})
    }})
})
