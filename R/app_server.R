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
    meanSpec = NULL,
    processedMSIData = NULL
  )
  # Your application server logic
  mod_uploadData_server("uploadData_1", global = global)
  mod_viewData_server("viewData_1", global = global)
  mod_segmentation_server("segmentation_1", global = global)
}
