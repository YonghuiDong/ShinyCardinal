#' export UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_export_ui <- function(id){
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
               p("1. Please watch video tutorials about metabolite identification based on accurate mass:",
                 a(href = "https://www.youtube.com/watch?v=vjhJwCZFpN0", shiny::icon("youtube", style = "color:#d17789; font-size:25px;"), target="_blank"))
               )
             ),

      #(1) Optional: upload MSI rds Data =======================================
      mod_readRDS_ui(ns("readRDS_1")),
      #(2) Export result =======================================================
      column(width = 12, h6("Export")),
      column(width = 4,
             box(
               width = 12,
               title = strong("Input Parameters"),
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = TRUE,
               closable = FALSE,
               p(style = "color:#C70039;", shiny::icon("bell"), strong("Note:")),
               p(style = "color:#C70039;", "1. Click the button to download centroid imzmL file."),
               actionButton(inputId = ns("go"),
                            label = "Go",
                            icon = icon("paper-plane"),
                            style = "color: #fff; background-color: #67ac8e; border-color: #67ac8e"
                            )
              )
            ),
      #(2.2) Export result -----------------------------------------------------
      column(width = 8,
             box(
               width = 12,
               title = strong("Result"),
               status = "success",
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = TRUE,
               closable = FALSE,
              shiny::uiOutput(outputId = ns("download"))
             )
            )

))}

#' export Server Functions
#'
#' @noRd
mod_export_server <- function(id, global){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    #(1) Load MSI rds Data =====================================================
    mod_readRDS_server("readRDS_1", global = global)
    #(2) Export result =========================================================
    output$download <- renderUI({
      shiny::validate(need(global$processedMSIData, message = "MSI data not found."))
      if(is.null(global$cleanedMSIData)){global$cleanedMSIData <- global$processedMSIData}
      downloadButton(outputId = ns("downloadFile"),
                     label = "Download zip",
                     icon = icon("download"),
                     style="color: #fff; background-color: #a077b5; border-color: #a077b5"
                     )
    })|>
      bindEvent(input$go)

    output$downloadFile <- downloadHandler(
      filename = function(){paste0("MSIDataFiles.zip")},
      content = function(file) {
        showModal(modalDialog(title = "Downloading...", footer = NULL))
        bundle_dir_name <- file.path(tempdir(),'bundle')
        if(dir.exists(bundle_dir_name)){
          unlink(bundle_dir_name, recursive = TRUE, force = TRUE)
        }
        dir.create(bundle_dir_name)
        Cardinal::writeImzML(object = global$cleanedMSIData, folder = bundle_dir_name, name = 'centroild')
        on.exit(removeModal())
        utils::zip(zipfile = file, files = bundle_dir_name, extras = '-j')
      }
    )


})}

## To be copied in the UI
# mod_export_ui("export_1")

## To be copied in the server
# mod_export_server("export_1")
