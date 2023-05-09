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
      #(1) User guide ==========================================================
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

      #(2) Data Input ==========================================================
      column(width = 12, h6("Upload MSI Data")),
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
               strong("1. Upload Files"),
               br(),
               p(style = "color:#C70039;", "Choose one of the two provided methods to read MSI files."),
               strong("Method 1: Read MSI files locally."),
               p(style = "color:#1f458f;", "(a) Use method 1 when running ShinyCardinal locally.
                 It's fast."),
               p(style = "color:#1f458f;", "(b) Please choose the directory where MSI files are located."),
               p(style = "color:#1f458f;", "(c) For multiple MSI runs (files), place them in the same directory."),
               actionButton(ns("chooseMSI"), label = "Choose Directory", icon = icon("sitemap")),
               br(),
               br(),
               strong("Method 2: Read uploaded MSI files."),
               p(style = "color:#1f458f;", "(a) Method 2 also works locally, but it's slower."),
               p(style = "color:#1f458f;", "(b) Use method 2 for the web server version ShinyCardinal."),
               p(style = "color:#1f458f;", "(c) Please upload both imzML and ibd files to the server."),
               p(style = "color:#1f458f;", "(d) For multiple MSI runs (files), upload all of them simultaneously."),
               fileInput(inputId = ns("imzmlFile"),
                         label = "",
                         multiple = TRUE,
                         placeholder = "Pleaae select both .imzMl and .ibd files",
                         accept = c(".imzML", ".ibd")
                         ),
               sliderInput(inputId = ns("massResolution"),
                           label = "2. Set mass resolution (ppm)",
                           min = 1,
                           max = 100,
                           value = 10,
                           step = 1
                           ),
               selectInput(inputId = ns("setMass"),
                           label = "3. Do you want to set mass range?",
                           multiple = FALSE,
                           choices = list("Yes" = "Yes", "No" = "No"),
                           selected = "No"
                           ),
               sliderInput(inputId = ns("massRange"),
                           label = "If Yes, please set mass range (Da)",
                           min = 0,
                           max = 5000,
                           value = c(50, 1500)
                           ),
               radioButtons(inputId = ns("msiDataMode"),
                            label = "4. (optional) Choose MSI data mode, only for multiple runs",
                            choices = c("Profile mode" = "0", "Centroid mode" = "1"),
                            selected = "0",
                            inline = TRUE
                            ),
               strong("5. (optional) Choose number of workers for parallel computation"),
               sliderInput(inputId = ns("loadDataWorkers"),
                           label = "",
                           min = 1,
                           max = 10,
                           value = 1,
                           step = 1
                           ),
               actionButton(inputId = ns("loadData"),
                            label = "Load Data",
                            icon = icon("paper-plane"),
                            style = "color: #fff; background-color: #67ac8e; border-color: #67ac8e"
                            )
               )
             ),

      #(2.1) Load Data ---------------------------------------------------------
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
               sliderInput(inputId = ns("nth"),
                           label = "1. (optional) Subset MSI data by select every nth pixel",
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
              ),
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
                )
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
               radioButtons(inputId = ns("ppMethod"),
                            label = "1.1 Select peak picking method",
                            choices = list("mad" = "mad", "simple" = "simple", "adaptive" = "adaptive"),
                            selected = "mad",
                            inline = TRUE,
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
                           label = "2.1 Choose peak aligment tolerance (ppm)",
                           min = 1,
                           max = 100,
                           value = 10,
                           step = 1
                           ),
               p(style = "color:#C70039;", "Step 3. Peak Filter"),
               sliderInput(inputId = ns("pfFreqmin"),
                           label = "3.1 Choose minimum frequencypeak to keep peaks",
                           min = 0,
                           max = 0.1,
                           value = 0.01,
                           step = 0.01
                           ),
               strong("4. (optional) Choose number of workers for parallel computation"),
               sliderInput(inputId = ns("getRefWorkers"),
                           label = "",
                           min = 1,
                           max = 10,
                           value = 1,
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
               radioButtons(inputId = ns("norMethod"),
                            label = "1.1 Select normalization method",
                            choices = list("tic" = "tic", "rms" = "rms"),
                            selected = "tic",
                            inline = TRUE,
                            ),
               p(style = "color:#C70039;", "(optional, TOF) Step 2. Smoothing"),
               p(style = "color:#C70039;", "(optional, TOF) Step 3. Baseline reduction"),
               p(style = "color:#C70039;", "Step 2. Peak Binning"),
               sliderInput(inputId = ns("pbTolerance"),
                           label = "2.1 Choose peak binning tolerance (ppm)",
                           min = 1,
                           max = 100,
                           value = 10,
                           step = 1
                          ),
               strong("3. (optional) Choose number of workers for parallel computation"),
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

    #(2) Load MSI Data =========================================================
    filePath <- reactiveValues(root = "~", current = "~", imzmlPath = NULL, ibdPath = NULL)

    #(2.1) Option 1 ------------------------------------------------------------
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
                                style="color: #fff; background-color: #a077b5; border-color: #a077b5"
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
                         style="color: #fff; background-color: #a077b5; border-color: #a077b5"
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
      filePath$imzmlPath <- unique(list.files(path = filePath$current, pattern = ".imzML", full.names = TRUE))
      filePath$ibdPath <- unique(list.files(path = filePath$current, pattern = ".ibd", full.names = TRUE))
      removeModal()
    })

    output$msiDataInfo <- renderPrint({
      if(identical(filePath$imzmlPath, character(0)) | is.null(filePath$imzmlPath)){
        #(2.2) Option 2 --------------------------------------------------------
        ## validate input
        shiny::validate(need(input$imzmlFile$datapath != "", message = "No MSI files found."))
        ## Get data path
        oldName <- input$imzmlFile$datapath
        newName <- file.path(dirname(input$imzmlFile$datapath), input$imzmlFile$name)
        file.rename(from = oldName, to = newName)
        msiFiles <- list.files(path = dirname(input$imzmlFile$datapath), full.names = TRUE)
        filePath$imzmlPath <- unique(grep(pattern = ".imzML", x = msiFiles, value = TRUE))
        filePath$ibdPath <- unique(grep(pattern = ".ibd", x = msiFiles, value = TRUE))
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
                                dataCentroid = as.logical(as.numeric(input$msiDataMode)),
                                workers = input$loadDataWorkers
                                )
      cat("The files have been loaded successfully. Below is the MSI data information:\n")
      cat("\n")
      global$msiData
    }) |>
      bindEvent(input$loadData)

    #(3) Get Mean Spectrum =====================================================
    specData <- reactiveValues(meanSpec = NULL, refPeaks = NULL)

    output$meanSpecPlot <- plotly::renderPlotly({
      #(3.1) validate input --------------------------------------------------
      shiny::validate(need(global$msiData, message = "MSI data not found."))

      #(3.2) Calculate mean spectrum -----------------------------------------
      specData$meanSpec <- global$msiData[, seq(1, max(Cardinal::pixels(global$msiData)), by = input$nth)] |>
        getMeanSpec(msiData = _,
                    worker = input$meanSpecWorkers
                    )
      plotMeanSpec(meanSpec = specData$meanSpec, nth = input$nth)
    }) |>
      bindEvent(input$getMeanSpec)

    #(4) Get Reference Peaks ===================================================
    #(4.1) Calculate and display reference spec --------------------------------
    output$refPeakPlot <- plotly::renderPlotly({
      shiny::validate(need(specData$meanSpec, message = "Mean spec not found."))
      specData$refPeaks <- getRefPeaks(meanSpec = specData$meanSpec,
                                       method = input$ppMethod,
                                       SNR = input$ppSNR,
                                       tolerance = input$paTolerance,
                                       freq.min = input$pfFreqmin,
                                       workers = input$getRefWorkers
                                       )
      plotMeanSpec(specData$refPeaks, nth = 1)
    })|>
      bindEvent(input$getRefPeaks)

    #(4.2) Show reference peak info --------------------------------------------
    output$refPeakInfo <- shiny::renderPrint({
      shiny::req(specData$refPeaks)
      cat("Below is the reference peak information:\n")
      cat("\n")
      specData$refPeaks
    })

    #(5) Process MSI Data ======================================================
    #(5.1) Process and display MSI data --------------------------------------
    output$processedMSIInfo <- shiny::renderPrint({
      shiny::validate(
        need(global$msiData, message = "MSI data not found."),
        need(specData$refPeaks, message = "Reference spec not found.")
      )
      global$processedMSIData <- processMSIData(msiData = global$msiData,
                                                method = input$norMethod,
                                                ref = specData$refPeaks,
                                                tolerance = input$pbTolerance,
                                                workers = input$getProcessMSIWorkers
                                                )
      cat("Below is the processed MSI information:\n")
      cat("\n")
      global$processedMSIData
    }) |>
      bindEvent(input$processMSIData)

      #(5.2) Download processed MSI data ---------------------------------------
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
        paste0("processedMSIData_", Sys.Date(), ".rds")
      },
      content = function(file){
        saveRDS(global$processedMSIData, file)
      }
    )


})}

## To be copied in the UI
# mod_uploadData_ui("uploadData_1")

## To be copied in the server
# mod_uploadData_server("uploadData_1")

