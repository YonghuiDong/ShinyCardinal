#' network UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_network_ui <- function(id){
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
               closable = FALSE
               )
      ),

      #(1) Optional: upload MSI rds Data =======================================
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
      ),

      #(2) Network Analysis for all features ===================================
      column(width = 12, h6("Network Analysis for All Features")),
      column(width = 4,
             box(
               width = 12,
               title = strong("Input Parameters"),
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = TRUE,
               closable = FALSE,
               selectInput(inputId = ns('msiRunAll'),
                           label = "(optional) Select a MSI run for network analysis",
                           choices = NULL,
                           selected = NULL
                           ),
               sliderInput(inputId = ns("nthAll"),
                           label = "(optional) Subset MSI data by using every nth pixels",
                           min = 1,
                           max = 10,
                           value = 1,
                           step = 1
                           ),
               shiny::actionButton(inputId = ns("colocAll"),
                                   label = "Go",
                                   icon = icon("paper-plane"),
                                   style = "color: #fff; background-color: #67ac8e; border-color: #67ac8e"
                                   ),
               br(),
               br(),
               sliderInput(inputId = ns("allLabelSize"),
                           label = "Set label size",
                           min = 10,
                           max = 100,
                           value = 40,
                           step = 5
                           ),
               sliderInput(inputId = ns("pccAllThreshold"),
                           label = "Set Network threshould",
                           min = 0.5,
                           max = 1,
                           value = 0.9,
                           step = 0.01
                           )
             )
            ),
      #(2.2) Network Analysis for all features result --------------------------
      column(width = 8,
             box(
               width = 12,
               title = strong("Input Parameters"),
               status = "success",
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = TRUE,
               closable = FALSE,
               shinycssloaders::withSpinner(
                 image = 'www/img/cardinal.gif',
                 shiny::verbatimTextOutput(outputId = ns("allNetworkInfo"))
                ),
               shiny::uiOutput(outputId = ns("downloadAllNetworkButton")),
               visNetwork::visNetworkOutput(outputId = ns("showAllNetwork"), height = "800px")
               )
             )

))}

#' network Server Functions
#'
#' @noRd
mod_network_server <- function(id, global = global){
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

    #(2) Network analysis for all features =====================================
    #(2.0) Update MSI run ------------------------------------------------------
    observeEvent(global$processedMSIData, {
      if(is.null(global$cleanedMSIData)){
        global$cleanedMSIData <- global$processedMSIData
      }
      updateSelectInput(session = session,
                        inputId = "msiRunAll", ## no name space
                        choices = levels(Cardinal::run(global$cleanedMSIData)),
                        selected = levels(Cardinal::run(global$cleanedMSIData))[1]
                        )

    })

    #(2.1) Get network object --------------------------------------------------
    allNetwork <- reactiveValues(PCC = NULL, plot = NULL)
    output$allNetworkInfo <- renderPrint({
      shiny::validate(need(!is.null(global$cleanedMSIData), message = "MSI data not found!"))
      allNetwork$PCC <- getPCC(msiData = global$cleanedMSIData, msiRun = input$msiRunAll)
      cat("You can navigate the network by using either the mouse or the zoom and movement buttons below.")
    }) |>
      bindEvent(input$colocAll)

    #(2.2) Show network --------------------------------------------------------
    output$showAllNetwork <- visNetwork::renderVisNetwork({
      shiny::req(allNetwork$PCC)
      allNetwork$plot <- plotAllNetwork(PCC = allNetwork$PCC,
                                        threshold = input$pccAllThreshold,
                                        labelSize = input$allLabelSize
                                        )
      allNetwork$plot
    })

    #(2.3) Download all Network -------------------------------------------------
    output$downloadAllNetworkButton <- renderUI({
      shiny::req(allNetwork$plot)
      downloadButton(
        outputId = ns("downloadAllNetwork"),
        label = "Download Network",
        icon = icon("download"),
        style="color: #fff; background-color: #a077b5; border-color: #a077b5"
      )
    })

    output$downloadAllNetwork <- downloadHandler(
        filename = function(){
          paste0(Sys.Date(), "_allNetwork", ".html")
        },
        content = function(file){
          visNetwork::visSave(graph = allNetwork$plot, file = file)
        }
    )

})}

## To be copied in the UI
# mod_network_ui("network_1")

## To be copied in the server
# mod_network_server("network_1")