#' exportMSI UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_exportMSI_ui <- function(id){
  ns <- NS(id)
  tagList(
    #(1) Input -----------------------------------------------------------------
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
             p(style = "color:#C70039;", "1. Please select data format to export."),
             p(style = "color:#C70039;", "2. Click the button to export the data."),
             radioButtons(inputId = ns("fileType"),
                          label = "Choose file type to export",
                          choices = list("Data Frame" = "dataframe", "Centroid imzML" = "imzML"),
                          selected = "dataframe",
                          inline = TRUE
                          ),
             actionButton(inputId = ns("go"),
                          label = "Go",
                          icon = icon("paper-plane"),
                          style = "color: #fff; background-color: #67ac8e; border-color: #67ac8e"
                          )
           )
    ),

    #(2) Output ----------------------------------------------------------------
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

)}

#' exportMSI Server Functions
#'
#' @noRd
mod_exportMSI_server <- function(id, global){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$download <- renderUI({
      shiny::validate(need(global$processedMSIData, message = "MSI data not found."))
      if(is.null(global$cleanedMSIData)){global$cleanedMSIData <- global$processedMSIData}
      downloadButton(outputId = ns("downloadFile"),
                     label = "Download",
                     icon = icon("download"),
                     style="color: #fff; background-color: #a077b5; border-color: #a077b5"
                     )
    })|>
      bindEvent(input$go)


    observeEvent(input$fileType, {
      if(input$fileType == "imzML"){
        output$downloadFile <- downloadHandler(
          filename = function(){paste0("MSIDataFiles.zip")},
          content = function(file) {
            shiny::withProgress(message = "Downloading", detail = "Be patient...", value = 0.4, {
              bundle_dir_name <- file.path(tempdir(),'bundle')
              if(dir.exists(bundle_dir_name)){
                unlink(bundle_dir_name, recursive = TRUE, force = TRUE)
              }
              dir.create(bundle_dir_name)
              Cardinal::writeImzML(object = global$cleanedMSIData, folder = bundle_dir_name, name = 'centroild')
              utils::zip(zipfile = file, files = bundle_dir_name, extras = '-j')
            })
          }
        )
      } else{
        output$downloadFile <- downloadHandler(
          filename = function(){paste0("MSIData.csv")},
          content = function(file){
            shiny::withProgress(message = "Downloading", detail = "Be patient...", value = 0.4, {
              utils::write.csv(x = exportData(msiData = global$cleanedMSIData), file = file, row.names = FALSE)
            })
          }
        )
      }
    })

})}

## To be copied in the UI
# mod_exportMSI_ui("exportMSI_1")

## To be copied in the server
# mod_exportMSI_server("exportMSI_1")
