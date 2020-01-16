#' Run Shiny App on local machine
#'
#' This function helps to deploy the shiny app in an easy way
#' on the local machine. As alternativ you can use the app
#' deployed on the server which can be found in the Git repo.
#'
#' @import shiny
#'
#' @export

runAppLocal = function() {

  appDir <- system.file("shinyApp", package = "visualDescent")

  shiny::runApp(appDir, display.mode = "normal")
}
