#' Run Shiny App on local machine
#'
#' @import shiny
#'
#' @export

runAppLocal = function() {

  appDir <- system.file("shinyApp", "app", package = "visualDescent")

  shiny::runApp(appDir, display.mode = "normal")
}
