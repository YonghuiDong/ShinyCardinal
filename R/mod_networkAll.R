#' networkAll UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_networkAll_ui <- function(id){
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
             p(style = "color:#C70039;", "1. Perform network analysis"),
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
             p(style = "color:#C70039;", "2. Modify network"),
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
             shinycssloaders::withSpinner(
               image = 'www/img/cardinal.gif',
               shiny::verbatimTextOutput(outputId = ns("allNetworkInfo"))
             ),
             shiny::uiOutput(outputId = ns("downloadAllNetworkButton")),
             visNetwork::visNetworkOutput(outputId = ns("showAllNetwork"), height = "800px")
           )
    )

)}

#' networkAll Server Functions
#'
#' @noRd
mod_networkAll_server <- function(id, global){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #(1) Update MSI run --------------------------------------------------------
    observeEvent(global$processedMSIData, {
      if(is.null(global$cleanedMSIData)){
        global$cleanedMSIData <- global$processedMSIData
      }
      updateSelectInput(session = session,
                        inputId = "msiRunAll",
                        choices = levels(Cardinal::run(global$cleanedMSIData)),
                        selected = levels(Cardinal::run(global$cleanedMSIData))[1]
                        )
    })

    #(2) Get network object ----------------------------------------------------
    allNetwork <- reactiveValues(PCC = NULL, plot = NULL)
    output$allNetworkInfo <- renderPrint({
      shiny::validate(need(!is.null(global$cleanedMSIData), message = "MSI data not found!"))
      allNetwork$PCC <- getPCC(msiData = global$cleanedMSIData,
                               nth = input$nthAll,
                               msiRun = input$msiRunAll
                               )
      cat("You can navigate the network using mouse or the zoom and movement buttons below.")
    }) |>
      bindCache(input$nthAll, input$msiRunAll) |>
      bindEvent(input$colocAll)

    #(3) Show network ----------------------------------------------------------
    output$showAllNetwork <- visNetwork::renderVisNetwork({
      shiny::req(allNetwork$PCC)
      allNetwork$plot <- plotAllNetwork(PCC = allNetwork$PCC,
                                        threshold = input$pccAllThreshold,
                                        labelSize = input$allLabelSize
                                        )
      allNetwork$plot
    })

    #(4) Download all Network --------------------------------------------------
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
        paste0("allNetwork", ".html")
      },
      content = function(file){
        visNetwork::visSave(graph = allNetwork$plot, file = file)
      }
    )

})}

## To be copied in the UI
# mod_networkAll_ui("networkAll_1")

## To be copied in the server
# mod_networkAll_server("networkAll_1")
