#' My Shiny App
#'
#' This is a simple Shiny app that demonstrates some features of the shiny package.
#'
#' @import shiny
#' @export
#'
#' @return A Shiny app object.

runMyApp = function(){
  ui <- fluidPage(
    "Hello, world!"
  )
  server <- function(input, output, session) {
  }

  shinyApp(ui, server)
}


