#' viewData UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_viewData_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      #(0) User guide ==========================================================
      column(width = 12,
             box(width = 12,
                 title = strong("User Guide"),
                 status = "warning",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 collapsed = FALSE,
                 closable = FALSE,
                 p("1. This section includes the most commont MSI data analysis tools, i.e., deisotoping, background noises and matrix peaks removal, image visualization, ROI analysis,
                    MSI data cropping and internal standard-based quantification."),
                 p("2. Some steps are optional, feel free to skip them if they are not relevant to your studies."),
                 p("3. The web version of ShinyCardinal does not support parallel computation. Please always set the number of workers to 1 to avoid out of memory error."),
                 p("4. Please watch video tutorials 2-8:", a(href = "https://www.youtube.com/@MSI_WIS/videos", shiny::icon("youtube", style = "color:#d17789; font-size:25px;"), target="_blank"))
                )
            ),

      #(1) Optional: upload MSI rds Data =======================================
      mod_readRDS_ui(ns("readRDS_1")),

      #(2) Deisotoping =========================================================
      column(width = 12, h5("Deisotoping (optional)")),
      mod_deisotoping_ui(ns("deisotoping_1")),

      #(3) Background noise and matrix removal =================================
      column(width = 12, h5("Remove Background Noises and Matrix Peaks (optional)")),
      mod_rmBackground_ui(ns("rmBackground_1")),

      #(4) Image View ==========================================================
      column(width = 12, h6("View MSI Images")),
      mod_plotMSI_ui(ns("plotMSI_1"), inputWidth = 4),

      #(5) Image Analysis ======================================================
      column(width = 12, h6("Image Analysis")),
      mod_imageAnalysis_ui(ns("imageAnalysis_1"))
  ))}

#' viewData Server Functions
#' @importFrom grDevices dev.off pdf png
#' @importFrom graphics lines par
#' @noRd
mod_viewData_server <- function(id, global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #(1) Load MSI rds Data =====================================================
    mod_readRDS_server("readRDS_1", global = global)

    #(2) Deisotoping ===========================================================
    mod_deisotoping_server("deisotoping_1", global = global)

    #(3) Background Removal ====================================================
    mod_rmBackground_server("rmBackground_1", global = global)

    #(4) Visualize MS images ===================================================
    observeEvent(global$processedMSIData, {
      ## In case users did not perform step 2 or step 3
      if(is.null(global$cleanedMSIData)){
        global$cleanedMSIData <- global$processedMSIData
      }
    })
    plotMSIServer <- mod_plotMSI_server("plotMSI_1", msiData = reactive({global$cleanedMSIData}), global = global, export_msiRun = TRUE)

    #(5) Image Analysis ========================================================
    mod_imageAnalysis_server("imageAnalysis_1", msiRun = reactive({plotMSIServer$msiRun()}), global = global)

})}

## To be copied in the UI
# mod_viewData_ui("viewData_1")

## To be copied in the server
# mod_viewData_server("viewData_1")



