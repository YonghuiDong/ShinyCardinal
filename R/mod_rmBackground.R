#' rmBackground UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_rmBackground_ui <- function(id){
  ns <- NS(id)
  tagList(
    #(1) Input -----------------------------------------------------------------
    column(width = 4,
           box(
             width = 12,
             inputId = "input_card",
             title = strong("Input Parameters"),
             status = "primary",
             solidHeader = TRUE,
             collapsible = TRUE,
             collapsed = TRUE,
             closable = FALSE,
             p(style = "color:#C70039;", shiny::icon("bell"), strong("Note:")),
             p(style = "color:#C70039;", "1. This moduel is optional."),
             p(style = "color:#C70039;", "2. Enter a noise or matrix m/z value to detected other noise/matrix peaks."),
             p(style = "color:#C70039;", "3. Speed up by subsetting MSI data and using multiple workers."),
             p(style = "color:#C70039;", "4. Repeat this step to eliminate various noise sources."),
             numericInput(inputId = ns("noisePeak"),
                          label = "Enter a single noise or matrix m/z value",
                          value = NULL,
                          min = 0,
                          max = 10000000
                          ),
             sliderInput(inputId = ns("colocThreshould"),
                         label = "Choose spatial similarity",
                         min = 0.5,
                         max = 1,
                         value = 0.9,
                         step = 0.01
                         ),
             sliderInput(inputId = ns("nth"),
                         label = "(optional) Subset MSI Data by using every nth pixel",
                         min = 1,
                         max = 10,
                         value = 1,
                         step = 1
                         ),
             strong("(optional) Choose number of workers for parallel computation"),
             sliderInput(inputId = ns("colocWorkers"),
                         label = "",
                         min = 1,
                         max = maxCores(),
                         value = 1,
                         step = 1
                         ),
             actionButton(inputId = ns("noiseColoc"),
                          label = "Start",
                          icon = icon("paper-plane"),
                          style = "color: #fff; background-color: #67ac8e; border-color: #67ac8e"
                          )
            )
    ),

    ##(2) Output ---------------------------------------------------------------
    column(width = 8,
           box(
             width = 12,
             inputId = "input_card",
             title = strong("Result"),
             status = "success",
             solidHeader = TRUE,
             collapsible = TRUE,
             collapsed = TRUE,
             closable = FALSE,
             shinycssloaders::withSpinner(
               image = 'www/img/cardinal.gif',
               shiny::verbatimTextOutput(outputId = ns("colocNoiseInfo"))
              ),
             column(width = 6, shiny::uiOutput(outputId = ns("deleteNoiseButton"))),
             column(width = 6, shiny::uiOutput(outputId = ns("resetNoiseButton"))),
             shiny::verbatimTextOutput(outputId = ns("summaryBNMR"))
            )
    )

)}

#' rmBackground Server Functions
#'
#' @noRd
mod_rmBackground_server <- function(id, global){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #(1) Perform colocalization ------------------------------------------------
    ## massList$removedFeatures is used to record removed features;
    ## it makes sure that users will not remove the same feature more than once, which may remove some unwanted mass features.
    ## massList$colocedFeatures is used to store colocalized features each time;
    ## the values are refreshed after each colocalization analysis.
    massList <- reactiveValues(removedFeatures = NULL, colocedFeatures = NULL)
    output$colocNoiseInfo <- shiny::renderPrint({
      shiny::validate(need(global$processedMSIData, message = "MSI data not found!"))
      shiny::validate(
        need(input$noisePeak >= min(Cardinal::mz(global$processedMSIData)) & input$noisePeak <= max(Cardinal::mz(global$processedMSIData)),
             message = "The entered m/z value is out of range."),
        need(!(round(input$noisePeak, 4) %in% massList$removedFeatures), message = "This m/z value has been removed, please try with another one")
      )
      if(is.null(global$cleanedMSIData)){
        global$cleanedMSIData <- global$processedMSIData
      }
      colocDF <- colocAnalysis(msiData = global$cleanedMSIData,
                               precursor = input$noisePeak,
                               nth = input$nth,
                               workers = input$colocWorkers
                               )
      massList$colocedFeatures <- colocDF[colocDF$correlation >= input$colocThreshould, c("mz", "correlation")]
      cat("Below are the detected backgroup noises and/or matrix peaks:\n")
      cat("Click Delete button to delete the peaks.\n")
      cat("Click Reset button to restore original MSI data.\n")
      cat("\n")
      massList$colocedFeatures
    }) |>
      bindEvent(input$noiseColoc)

    #(2) Display buttons -------------------------------------------------------
    output$deleteNoiseButton <- renderUI({
      shiny::req(massList$colocedFeatures)
      actionButton(
        inputId = ns("deleteNoise"),
        label = "Delete",
        icon = icon("trash"),
        style="color: #fff; background-color: #a077b5; border-color: #a077b5"
      )
    }) |>
      bindEvent(input$noiseColoc)

    output$resetNoiseButton <- renderUI({
      shiny::req(massList$colocedFeatures)
      actionButton(
        inputId = ns("resetNoise"),
        label = "Reset",
        icon = icon("circle"),
        style="color: #fff; background-color: #a077b5; border-color: #a077b5"
      )
    }) |>
      bindEvent(input$noiseColoc)

    #(3) Delete features -------------------------------------------------------
    observeEvent(input$deleteNoise, {
      shiny::req(global$cleanedMSIData)
      shiny::req(massList$colocedFeatures)
      global$cleanedMSIData <- removeNoise(msiData = global$cleanedMSIData, subDF = massList$colocedFeatures)
      ## the input noise peak is not exactly the same as in the data, so I need to record it as well.
      massList$removedFeatures <- round(c(massList$removedFeatures, input$noisePeak, massList$colocedFeatures$mz), 4)
      output$summaryBNMR <- shiny::renderPrint({
        cat("Below is the MSI data with background noise/matrix peaks removed: \n")
        global$cleanedMSIData
      })
    })

    #(4) Reset feature ---------------------------------------------------------
    observeEvent(input$resetNoise, {
      shiny::req(global$cleanedMSIData)
      shiny::req(global$processedMSIData)
      global$cleanedMSIData <- global$processedMSIData
      massList$removedFeatures <- NULL
      output$summaryBNMR <- shiny::renderPrint({
        cat("No noises or matrix related peaks were removed.\n")
        global$cleanedMSIData
      })
    })



})}

## To be copied in the UI
# mod_rmBackground_ui("rmBackground_1")

## To be copied in the server
# mod_rmBackground_server("rmBackground_1")
