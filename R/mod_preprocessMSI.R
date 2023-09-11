#' preprocessMSI UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_preprocessMSI_ui <- function(id){
  ns <- NS(id)
  tagList(
    #(1) Calculate Mean Spectrum ===============================================
    #(1.1) Input ---------------------------------------------------------------
    column(width = 12, h6("Calculate Mean Spectrum")),
    column(width = 5,
           box(
             width = 12,
             inputId = ns("input_card"),
             title = strong("Input Parameters"),
             status = "primary",
             solidHeader = TRUE,
             collapsible = TRUE,
             collapsed = TRUE,
             closable = FALSE,
             strong("1. (optional) Subset MSI data by select every nth pixel"),
             p(style = "color:#C70039;", shiny::icon("bell"), strong("Note:")),
             p(style = "color:#C70039;", "1. Subsetting every nth pixel speeds up by factor N."),
             p(style = "color:#C70039;", "2. Large nth may miss highly localized mass features."),
             sliderInput(inputId = ns("nth"),
                         label = NULL,
                         min = 1,
                         max = 10,
                         value = 1,
                         step = 1
                         ),
             strong("2. (optional) Choose number of workers for parallel computation"),
             sliderInput(inputId = ns("meanSpecWorkers"),
                         label = "",
                         min = 1,
                         max = maxCores(),
                         value = 1,
                         step = 1
                         ),
             actionButton(inputId = ns("getMeanSpec"),
                          label = "Calculate",
                          icon = icon("paper-plane"),
                          style = "color: #fff; background-color: #67ac8e; border-color: #67ac8e"
                          )
            )
    ),

    #(1.2) Output --------------------------------------------------------------
    column(width = 7,
           box(
             width = 12,
             inputId = "report_card",
             title = strong("Result"),
             status = "success",
             solidHeader = TRUE,
             collapsible = TRUE,
             collapsed = TRUE,
             closable = FALSE,
             shinycssloaders::withSpinner(
               image = 'www/img/cardinal.gif',
               plotly::plotlyOutput(outputId = ns("meanSpecPlot"))
              ),
             shiny::verbatimTextOutput(outputId = ns("meanPeakInfo"))
            )
    ),

    #(2) Get Reference Peaks ===================================================
    #(2.1) Input ---------------------------------------------------------------
    column(width = 12, h6("Calculate Reference Peaks")),
    column(width = 5,
           box(
             width = 12,
             inputId = ns("input_card"),
             title = strong("Input Parameters"),
             status = "primary",
             solidHeader = TRUE,
             collapsible = TRUE,
             collapsed = TRUE,
             closable = FALSE,
             p(style = "color:#C70039;", "Step 1. Peak Picking "),
             selectInput(inputId = ns("ppMethod"),
                         label = "1.1 Select peak picking method",
                         choices = list("Mean absolute deviations noise (mad)" = "mad",
                                        "Simple standard deviations noise (simple)" = "simple",
                                        "Adaptive standard deviations noise (adaptive)" = "adaptive"
                                        ),
                         selected = "mad"
                         ),
             sliderInput(inputId = ns("ppSNR"),
                         label = "1.2 Choose signal to noise ratio",
                         min = 1,
                         max = 100,
                         value = 10,
                         step = 1
                         ),
             p(style = "color:#C70039;", "Step 2. Peak Alignment"),
             sliderInput(inputId = ns("paTolerance"),
                         label = "Choose peak aligment tolerance (ppm)",
                         min = 1,
                         max = 20,
                         value = 5,
                         step = 1
                         ),
             actionButton(inputId = ns("getRefPeaks"),
                          label = "Calculate",
                          icon = icon("paper-plane"),
                          style = "color: #fff; background-color: #67ac8e; border-color: #67ac8e"
                          )
            )
    ),

    #(2.2) Output --------------------------------------------------------------
    column(width = 7,
           box(
             width = 12,
             inputId = "report_card",
             title = strong("Result"),
             status = "success",
             solidHeader = TRUE,
             collapsible = TRUE,
             collapsed = TRUE,
             closable = FALSE,
             shinycssloaders::withSpinner(
               image = 'www/img/cardinal.gif',
               plotly::plotlyOutput(outputId = ns("refPeakPlot"))
             ),
             shiny::verbatimTextOutput(outputId = ns("refPeakInfo"))
            )
    ),

    #(3) Process MSI Data ======================================================
    #(3.1) Input ---------------------------------------------------------------
    column(width = 12, h6("Process MSI Data")),
    column(width = 5,
           box(
             width = 12,
             inputId = ns("input_card"),
             title = strong("Input Parameters"),
             status = "primary",
             solidHeader = TRUE,
             collapsible = TRUE,
             collapsed = TRUE,
             closable = FALSE,
             p(style = "color:#C70039;", "Step 1. Normalization "),
             selectInput(inputId = ns("norMethod"),
                         label = "Select normalization method",
                         choices = list("Total ion current normalization" = "tic",
                                        "Root mean square normalization" = "rms"
                                        ),
                         selected = "tic"
                         ),
             p(style = "color:#C70039;", "Step 2. Spectra smoothing (optional)"),
             checkboxInput(inputId = ns("ifSmoothing"),
                           label = "Should perfrom smoothing?",
                           value = FALSE
                           ),
             selectInput(inputId = ns("smoothMethod"),
                         label = "Select smoothing method",
                         choices = list("Gaussian smoothing" = "gaussian",
                                        "Savitzky-Golay smoothing" = "sgolay",
                                        "Moving average smoothing" = "ma"
                                        ),
                         selected = "gaussian"
                         ),
             p(style = "color:#C70039;", "Step 3. Baseline reduction (optional)"),
             checkboxInput(inputId = ns("ifBLReduction"),
                           label = "Should perfrom baseline reduction?",
                           value = FALSE
                           ),
             selectInput(inputId = ns("blReductionMethod"),
                         label = "Select baseline reduction method",
                         choices = list("Local minima" = "locmin",
                                        "Binned medians" = "median"
                                        ),
                         selected = "locmin"
                         ),
             p(style = "color:#C70039;", "Step 4. Peak Binning"),
             sliderInput(inputId = ns("pbTolerance"),
                         label = "Choose peak binning tolerance (ppm)",
                         min = 1,
                         max = 20,
                         value = 5,
                         step = 1
                         ),
             strong("5. (optional) Choose number of workers for parallel computation"),
             sliderInput(inputId = ns("getProcessMSIWorkers"),
                         label = "",
                         min = 1,
                         max = maxCores(),
                         value = 1,
                         step = 1
                         ),
             actionButton(inputId = ns("processMSIData"),
                          label = "Process",
                          icon = icon("paper-plane"),
                          style = "color: #fff; background-color: #67ac8e; border-color: #67ac8e"
                          )
            )
    ),

    #(3.2) Output --------------------------------------------------------------
    column(width = 7,
           box(
             width = 12,
             inputId = "report_card",
             title = strong("Result"),
             status = "success",
             solidHeader = TRUE,
             collapsible = TRUE,
             collapsed = TRUE,
             closable = FALSE,
             shinycssloaders::withSpinner(
               image = 'www/img/cardinal.gif',
               shiny::verbatimTextOutput(outputId = ns("processedMSIInfo"))
             ),
             br(),
             br(),
             shiny::uiOutput(outputId = ns("downloadButton"))
            )
    )

)}

#' preprocessMSI Server Functions
#'
#' @noRd
mod_preprocessMSI_server <- function(id, EXPR, global){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #(1) Get Mean Spectrum =====================================================
    specData <- reactiveValues(meanSpec = NULL, refPeaks = NULL)
    output$meanSpecPlot <- plotly::renderPlotly({
      #(1.1) validate input ----------------------------------------------------
      shiny::validate(need(global$msiData, message = "MSI data not found."))

      #(1.2) Calculate mean spectrum -------------------------------------------
      specData$meanSpec <- getMeanSpec(msiData = global$msiData,
                                       nth = input$nth,
                                       workers = input$meanSpecWorkers
                                       )
      plotMeanSpec(meanSpec = specData$meanSpec, nth = input$nth)
    }) |>
      bindEvent(input$getMeanSpec)

    #(1.3) Show mean peak info -------------------------------------------------
    output$meanPeakInfo <- shiny::renderPrint({
      shiny::req(specData$meanSpec)
      cat("Below is the mean peak information:\n")
      cat("\n")
      specData$meanSpec
    })

    #(2) Get Reference Peaks ===================================================
    #(2.1) Calculate and display reference spec --------------------------------
    observe({
      switch(EXPR = EXPR(),
             "HR" = updateSliderInput(inputId = "paTolerance", min = 1, max = 20, value = 5, step = 1),
             "LR" = updateSliderInput(inputId = "paTolerance", min = 20, max = 400, value = 50, step = 5)
             )
    })
    output$refPeakPlot <- plotly::renderPlotly({
      shiny::validate(need(specData$meanSpec, message = "Mean spec not found."))
      specData$refPeaks <- getRefPeaks(meanSpec = specData$meanSpec,
                                       method = input$ppMethod,
                                       SNR = input$ppSNR,
                                       tolerance = input$paTolerance
                                       )
      plotMeanSpec(specData$refPeaks, nth = 1)
    })|>
      bindEvent(input$getRefPeaks)

    #(2.2) Show reference peak info --------------------------------------------
    output$refPeakInfo <- shiny::renderPrint({
      shiny::req(specData$refPeaks)
      cat("Below is the reference peak information:\n")
      cat("\n")
      specData$refPeaks
    })

    #(3) Process MSI Data ======================================================
    #(3.1) Process and display MSI data ----------------------------------------
    observe({
      switch(EXPR = EXPR(),
             "HR" = updateSliderInput(inputId = "pbTolerance", min = 1, max = 20, value = 5, step = 1),
             "LR" = updateSliderInput(inputId = "pbTolerance", min = 20, max = 400, value = 50, step = 5)
             )
    })
    output$processedMSIInfo <- shiny::renderPrint({
      shiny::validate(
        need(global$msiData, message = "MSI data not found."),
        need(specData$refPeaks, message = "Reference spec not found.")
      )
      global$processedMSIData <- processMSIData(msiData = global$msiData,
                                                normMethod = input$norMethod,
                                                ref = specData$refPeaks,
                                                ifSmoothing = input$ifSmoothing,
                                                smoothMethod = input$smoothMethod,
                                                ifBLReduction = input$ifBLReduction,
                                                blReductionMethod = input$blReductionMethod,
                                                tolerance = input$pbTolerance,
                                                workers = input$getProcessMSIWorkers
                                                )
      cat("Below is the processed MSI information:\n")
      cat("\n")
      global$processedMSIData
    }) |>
      bindEvent(input$processMSIData)

    #(3.2) Download processed MSI data -----------------------------------------
    output$downloadButton <- renderUI({
      shiny::req(global$processedMSIData)
      downloadButton(
        outputId = ns("downloadProcessedData"),
        label = "Download rds Data",
        style="color: #fff; background-color: #a077b5; border-color: #a077b5"
      )
    }) |>
      bindEvent(input$processMSIData)

    output$downloadProcessedData <- downloadHandler(
      filename = function(){
        paste0("processedMSIData", ".rds")
      },
      content = function(file){
        shiny::withProgress(message = "Downloading", detail = "Be patient...", value = 0.4, {
          saveRDS(global$processedMSIData, file)
        })
      }
    )

})}

## To be copied in the UI
# mod_preprocessMSI_ui("preprocessMSI_1")

## To be copied in the server
# mod_preprocessMSI_server("preprocessMSI_1")
