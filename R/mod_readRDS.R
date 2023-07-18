#' readRDS UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_readRDS_ui <- function(id){
  ns <- NS(id)
  tagList(
    #(1) Optional: upload MSI rds Data =========================================
    column(width = 12, h5("Upload MSI rds Data (Optional)")),
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
             p(style = "color:#C70039;", "1. This moduel is optional."),
             p(style = "color:#C70039;", "2. If you're starting directly from this module, you need to upload the rds file."),
             fileInput(inputId = ns("rdsMSI"),
                       label = "Please select the rds file.",
                       multiple = FALSE,
                       placeholder = "",
                       accept = c(".rds")
                       ),
             actionButton(inputId = ns("loadData"),
                          label = "Load",
                          icon = icon("paper-plane"),
                          style = "color: #fff; background-color: #67ac8e; border-color: #67ac8e"
                          )
             )

    ),
      #(1.2) Upload MSI rds data result ----------------------------------------
      column(width = 8,
             box(
               width = 12,
               title = strong("Result"),
               status = "success",
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = TRUE,
               closable = FALSE,
               shinycssloaders::withSpinner(
                 image = 'www/img/cardinal.gif',
                 shiny::verbatimTextOutput(outputId = ns("infoMSIData"))
               )
             )
      )

)}

#' readRDS Server Functions
#'
#' @noRd
mod_readRDS_server <- function(id, global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #(1) Load MSI rds Data =====================================================
    output$infoMSIData <- shiny::renderPrint({
      shiny::validate(need(!is.null(input$rdsMSI), message = "rds file not found"))
      global$processedMSIData <- readRDS(input$rdsMSI$datapath)
      if(is.null(global$processedMSIData)){
        cat("MSI data not loaded, please check if your rds file is empty.\n")
      } else {
        cat("MSI data loaded successfully!\n")
        global$processedMSIData
      }
    }) |>
      bindEvent(input$loadData)

  })}

## To be copied in the UI
# mod_readRDS_ui("readRDS_1")

## To be copied in the server
# mod_readRDS_server("readRDS_1")
