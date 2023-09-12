#' deisotoping UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_deisotoping_ui <- function(id){
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
             p(style = "color:#C70039;", "1. This module is optional."),
             p(style = "color:#C70039;", "2. It is for 13C deisotoping only."),
             radioButtons(inputId = ns("msiDataType"),
                          label = "Choose MSI data type",
                          choices = list("High-mass-resolution" = "HR", "Low-mass-resolution" = "LR"),
                          selected = "HR",
                          inline = TRUE
                          ),
             sliderInput(inputId = ns("deisotopeTol"),
                         label = "Select mass tolerance (Da)",
                         min = 0.001,
                         max = 0.01,
                         value = 0.005,
                         step = 0.001
                         ),
             sliderInput(inputId = ns("deisotopePCC"),
                         label = "Set spatial similarity",
                         min = 0.7,
                         max = 1,
                         value = 0.9,
                         step = 0.01
                         ),
             actionButton(inputId = ns("deisotope"),
                          label = "Start",
                          icon = icon("paper-plane"),
                          style = "color: #fff; background-color: #67ac8e; border-color: #67ac8e"
                          )
            )
    ),
    #(2) Output ----------------------------------------------------------------
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
               shiny::verbatimTextOutput(outputId = ns("deisotopingInfo"))
             ),
             shiny::uiOutput(outputId = ns("resetIsotopeButton")),
             shiny::verbatimTextOutput(outputId = ns("resetIsotopeInfo"))
            )
    )

)}

#' deisotoping Server Functions
#'
#' @noRd
mod_deisotoping_server <- function(id, global){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #(1) deisotoping -----------------------------------------------------------
    observe({
      switch(EXPR = input$msiDataType,
             "HR" = updateSliderInput(inputId = "deisotopeTol", min = 0.001, max = 0.1, value = 0.005, step = 0.001),
             "LR" = updateSliderInput(inputId = "deisotopeTol", min = 0.1, max = 1, value = 0.5, step = 0.05)
             )
    })
    output$deisotopingInfo <- shiny::renderPrint({
      shiny::validate(need(global$processedMSIData, message = "MSI data not found!"))
      global$cleanedMSIData <- deisotoping(global$processedMSIData, tol = input$deisotopeTol, PCC = input$deisotopePCC)
      cat("Deisotoping done.\n")
      cat("Below is deisotoped MSI data. \n")
      global$cleanedMSIData
    }) |>
      bindCache(global$processedMSIData, input$deisotopeTol, input$deisotopePCC) |>
      bindEvent(input$deisotope)

    #(2) Reset feature ---------------------------------------------------------
    output$resetIsotopeButton <- renderUI({
      shiny::req(global$cleanedMSIData)
      actionButton(
        inputId = ns("resetIsotope"),
        label = "Reset",
        icon = icon("circle"),
        style="color: #fff; background-color: #a077b5; border-color: #a077b5"
      )
    }) |>
      bindEvent(input$deisotope)

    observeEvent(input$resetIsotope, {
      shiny::req(global$cleanedMSIData)
      shiny::req(global$processedMSIData)
      global$cleanedMSIData <- global$processedMSIData
    })

    output$resetIsotopeInfo <- shiny::renderPrint({
      shiny::req(global$cleanedMSIData)
      shiny::req(global$processedMSIData)
      if(identical(global$processedMSIData, global$cleanedMSIData)){
        cat("Deisotoping cancelled.\n")
        global$cleanedMSIData
      }
    }) |>
      bindEvent(input$resetIsotope)

})}

## To be copied in the UI
# mod_deisotoping_ui("deisotoping_1")

## To be copied in the server
# mod_deisotoping_server("deisotoping_1")
