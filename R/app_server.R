#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  options(shiny.maxRequestSize = 10000000 * 1024^2) ## file size limit
  global <- reactiveValues(
    msiData = NULL,
    meanSpec = NULL
  )
  # Your application server logic
  mod_uploadData_server("uploadData_1", global = global)
}
