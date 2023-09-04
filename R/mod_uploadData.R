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
      column(width = 5,
             box(
               width = 12,
               inputId = "input_card",
               title = strong("Input Parameters"),
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = TRUE,
               closable = FALSE,
               strong("1. Read Files"),
               br(),
               p(style = "color:#C70039;", shiny::icon("bell"), strong("Note:")),
               p(style = "color:#C70039;", "Choose one of the two methods to read MSI files."),
               strong("Method 1: Read MSI files locally."),
               p(style = "color:#C70039;", "1. Use method 1 when running ShinyCardinal locally. It's fast."),
               p(style = "color:#C70039;", "2. Please choose the directory where MSI files are located."),
               p(style = "color:#C70039;", "3. For multiple MSI runs (files), place them in the same directory."),
               actionButton(ns("chooseMSI"), label = "Choose Directory", icon = icon("sitemap")),
               br(),
               br(),
               strong("Method 2: Read uploaded MSI files."),
               p(style = "color:#C70039;", "1. Method 2 also works locally, but it's slower."),
               p(style = "color:#C70039;", "2. Use method 2 for the web server version ShinyCardinal."),
               p(style = "color:#C70039;", "3. Please upload both imzML and ibd files to the server."),
               p(style = "color:#C70039;", "4. For multiple MSI runs (files), upload all of them simultaneously."),
               fileInput(inputId = ns("imzmlFile"),
                         label = "",
                         multiple = TRUE,
                         placeholder = "Pleaae select both .imzMl and .ibd files",
                         accept = c(".imzML", ".ibd")
                         ),
               hr(),
               radioButtons(inputId = ns("msiDataType"),
                            label = "2.1 Choose MSI data type",
                            choices = c("High-mass-resolution" = "HR", "Low-mass-resolution" = "LR"),
                            selected = "HR",
                            inline = TRUE
                            ),
               sliderInput(inputId = ns("massResolution"),
                           label = "2.2 Set mass resolution (ppm)",
                           min = 1,
                           max = 20,
                           value = 10,
                           step = 1
                           ),
               hr(),
               radioButtons(inputId = ns("setMass"),
                           label = "3. Do you want to set mass range?",
                           choices = list("Yes" = "Yes", "No" = "No"),
                           selected = "No",
                           inline = TRUE
                           ),
               sliderInput(inputId = ns("massRange"),
                           label = "If Yes, please set mass range (Da)",
                           min = 0,
                           max = 5000,
                           value = c(50, 1500)
                           ),
               hr(),
               strong("4. (optional) Choose MSI data mode"),
               p(style = "color:#C70039;", shiny::icon("bell"), strong("Note:")),
               p(style = "color:#C70039;", "This parameter applies only when multiple runs loaded with unknown/different modes."),
               radioButtons(inputId = ns("msiDataMode"),
                            label = NULL,
                            choices = c("Profile mode" = "0", "Centroid mode" = "1"),
                            selected = "0",
                            inline = TRUE
                            ),
               actionButton(inputId = ns("loadData"),
                            label = "Load Data",
                            icon = icon("paper-plane"),
                            style = "color: #fff; background-color: #67ac8e; border-color: #67ac8e"
                            )
               )
             ),

      #(1.1) Load Data ---------------------------------------------------------
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
                 shiny::verbatimTextOutput(outputId = ns("msiDataInfo"))
                )
              )
            ),
      #(2) Plot MSI images =====================================================
      column(width = 12, h5("Check MSI images (optional)")),
      mod_plotMSI_ui(ns("plotMSI_1"), inputWidth = 5, showNote = TRUE),

      #(3) Get Mean Spectrum ===================================================
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
                           max = 10,
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

      #(3.2) Get Mean Spectrum Result ------------------------------------------
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

      #(4) Get Reference Peaks =================================================
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
                                          "Adaptive standard deviations noise (adaptive)" = "adaptive"),
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

      #(4.2) Get Reference Peaks Result ----------------------------------------
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

      #(5) Process MSI Data ====================================================
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
                           max = 10,
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

      #(5.2) Process MSI Data Result -------------------------------------------
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
))}


#' uploadData Server Functions
#' @noRd
mod_uploadData_server <- function(id, global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #(1) Load MSI Data =========================================================
    filePath <- reactiveValues(root = "~", current = "~", imzmlPath = NULL, ibdPath = NULL)

    #(1.1) Option 1 ------------------------------------------------------------
    observeEvent(input$chooseMSI, {
      showModal(
        modalDialog(
          title = p(style = "color:#C70039;", "Select the directory (not MSI file)"),
          p("Current directory: ", textOutput(outputId = ns("current_path"), inline = TRUE)),
          fluidRow(
            column(width = 2,
                   actionButton(inputId = ns("button_back"),
                                label = "Back",
                                icon = icon("undo"),
                                style = "color: #fff; background-color: #a077b5; border-color: #a077b5"
                                )
                  ),
            column(width = 10,
                   selectInput(inputId = ns("dir"),
                               label = NULL,
                               choices = "Please select",
                               width = "100%"
                               )
                  )
          ),
          size = "l",
          footer = tagList(
            actionButton(inputId = ns("ok"),
                         label = "OK",
                         icon = icon("check"),
                         style = "color: #fff; background-color: #a077b5; border-color: #a077b5"
                        )
          )
        )
      )
      new_choices <- c("Please select", dir(filePath$current))
      updateSelectInput(inputId = "dir", choices = new_choices)
    })

    ## update directory
    observeEvent(input$dir, {
      if(input$dir != "Please select"){
        filePath$current <- file.path(filePath$current, input$dir)
      }
      new_choices <- c("Please select", dir(filePath$current))
      updateSelectInput(inputId = "dir", choices = new_choices)
    })

    ## display directory
    output$current_path = renderText({filePath$current})

    ## back button
    observeEvent(input$button_back, {
      if(filePath$current != filePath$root){
        filePath$current <- dirname(filePath$current)
        new_choices = c("Please select", dir(filePath$current))
        updateSelectInput(inputId = "dir", choices = new_choices)
      }
    })

    ## OK button
    observeEvent(input$ok, {
      ## Get imzML and ibd file path
      filePath$imzmlPath <- unique(list.files(path = filePath$current, pattern = ".imzML", full.names = TRUE, ignore.case = TRUE))
      filePath$ibdPath <- unique(list.files(path = filePath$current, pattern = ".ibd", full.names = TRUE, ignore.case = TRUE))
      removeModal()
    })

    observe({
      switch(EXPR = input$msiDataType,
             "HR" = updateSliderInput(inputId = "massResolution", min = 1, max = 20, value = 10, step = 1),
             "LR" = updateSliderInput(inputId = "massResolution", min = 20, max = 200, value = 50, step = 5)
             )
    })

    output$msiDataInfo <- renderPrint({
      if(identical(filePath$imzmlPath, character(0)) | is.null(filePath$imzmlPath)){
        #(1.2) Option 2 --------------------------------------------------------
        shiny::validate(need(input$imzmlFile$datapath != "", message = "No MSI files found."))
        ## Get data path
        oldName <- input$imzmlFile$datapath
        newName <- file.path(dirname(input$imzmlFile$datapath), input$imzmlFile$name)
        file.rename(from = oldName, to = newName)
        msiFiles <- list.files(path = dirname(input$imzmlFile$datapath), full.names = TRUE)
        filePath$imzmlPath <- unique(grep(pattern = ".imzML", x = msiFiles, value = TRUE, ignore.case = TRUE))
        filePath$ibdPath <- unique(grep(pattern = ".ibd", x = msiFiles, value = TRUE, ignore.case = TRUE))
      }

      ## Load MSI data
      shiny::validate(
        need(filePath$imzmlPath != "", message = ("imzML file missing!")),
        need(filePath$imzmlPath != "", message = "ibd file missing!"),
        need(length(filePath$imzmlPath) == length(filePath$ibdPath), message = "The number of imzML and idb files are not equal!")
        )
      if(input$setMass == "No"){
        selectedMassRange = NULL
      } else {
        selectedMassRange = input$massRange
      }
      global$msiData <- readMSI(path = filePath$imzmlPath,
                                massResolution = input$massResolution,
                                massRange = selectedMassRange,
                                dataCentroid = as.logical(as.numeric(input$msiDataMode))
                                )
      cat("The files have been loaded successfully. Below is the MSI data information:\n")
      cat("\n")
      global$msiData
    }) |>
      bindEvent(input$loadData)

    #(2) Display MSI images ====================================================
    mod_plotMSI_server("plotMSI_1", msiData = reactive({global$msiData}), global = global)

    #(2) Get Mean Spectrum =====================================================
    specData <- reactiveValues(meanSpec = NULL, refPeaks = NULL)

    output$meanSpecPlot <- plotly::renderPlotly({
      #(2.1) validate input --------------------------------------------------
      shiny::validate(need(global$msiData, message = "MSI data not found."))

      #(2.2) Calculate mean spectrum -----------------------------------------
      specData$meanSpec <- getMeanSpec(msiData = global$msiData,
                                       nth = input$nth,
                                       workers = input$meanSpecWorkers
                                       )
      plotMeanSpec(meanSpec = specData$meanSpec, nth = input$nth)
    }) |>
      bindEvent(input$getMeanSpec)

    #(2.3) Show mean peak info -------------------------------------------------
    output$meanPeakInfo <- shiny::renderPrint({
      shiny::req(specData$meanSpec)
      cat("Below is the mean peak information:\n")
      cat("\n")
      specData$meanSpec
    })

    #(3) Get Reference Peaks ===================================================
    #(3.1) Calculate and display reference spec --------------------------------
    observe({
      switch(EXPR = input$msiDataType,
             "HR" = updateSliderInput(inputId = "paTolerance", min = 1, max = 20, value = 5, step = 1),
             "LR" = updateSliderInput(inputId = "paTolerance", min = 20, max = 200, value = 50, step = 5)
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

    #(3.2) Show reference peak info --------------------------------------------
    output$refPeakInfo <- shiny::renderPrint({
      shiny::req(specData$refPeaks)
      cat("Below is the reference peak information:\n")
      cat("\n")
      specData$refPeaks
    })

    #(4) Process MSI Data ======================================================
    #(4.1) Process and display MSI data ----------------------------------------
    observe({
      switch(EXPR = input$msiDataType,
             "HR" = updateSliderInput(inputId = "pbTolerance", min = 1, max = 20, value = 5, step = 1),
             "LR" = updateSliderInput(inputId = "pbTolerance", min = 20, max = 200, value = 50, step = 5)
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

    #(4.2) Download processed MSI data -----------------------------------------
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
# mod_uploadData_ui("uploadData_1")

## To be copied in the server
# mod_uploadData_server("uploadData_1")

