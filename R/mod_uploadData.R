#' uploadData UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinydashboard
mod_uploadData_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      #(0) User guide ==========================================================
      column(width = 12,
             box(
               width = 12,
               title = strong("User Guide"),
               status = "warning",
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = FALSE,
               closable = FALSE,
               p("1. The purpose of data preprocessing is to improve image quality, and prepare data for downstream data mining."),
               p("2. A typical MSI data preprocessing workflow includes normalization, baseline reduction, smoothing,
                 peak picking, spectral alignment, and binning."),
               p("3. The web version of ShinyCardinal does not support parallel computation. Please always set the number of workers to 1 to avoid out of memory error."),
               p("4. Please watch this video tutorial about MSI data preprocessing:",
                 a(href = "https://www.youtube.com/watch?v=1UslDNg7rWU&t=468s", shiny::icon("youtube", style = "color:#d17789; font-size:25px;"), target="_blank")
                 )
               )
             ),

      #(1) Data Input ==========================================================
      column(width = 12, h6("Read MSI Data")),
      mod_readImzML_ui(ns("readImzML_1")),

      #(2) Plot MSI images =====================================================
      column(width = 12, h5("Check MSI images (optional)")),
      mod_plotMSI_ui(ns("plotMSI_1"), inputWidth = 5, showNote = TRUE, showMassWindow = TRUE),

      #(3) Preprocess MSI ======================================================
      mod_preprocessMSI_ui(ns("preprocessMSI_1"))

))}


#' uploadData Server Functions
#' @noRd
mod_uploadData_server <- function(id, global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #(1) Load MSI Data =========================================================
    readImzMLServer <- mod_readImzML_server("readImzML_1", global = global, export_msiDataType = TRUE)

    #(2) Display MSI images ====================================================
    mod_plotMSI_server("plotMSI_1", msiData = reactive({global$msiData}), global = global)

    #(3) Preprocess MSI data ===================================================
    mod_preprocessMSI_server("preprocessMSI_1", EXPR = reactive({readImzMLServer$msiDataType()}), global = global)

})}

## To be copied in the UI
# mod_uploadData_ui("uploadData_1")

## To be copied in the server
# mod_uploadData_server("uploadData_1")

