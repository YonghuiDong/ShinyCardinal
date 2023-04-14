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
               collapsed = TRUE,
               closable = FALSE
               )
             ),

      #(2) Data Input ==========================================================
      column(width = 5,
             box(
               width = 12,
               inputId = "input_card",
               title = strong("Upload Data Panel"),
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = FALSE,
               closable = FALSE,
               fileInput(inputId = ns("imzmlFile"),
                         label = "1. Upload  files:",
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
               strong("4. (optional) Choose number of workers for parallel computation"),
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
               title = strong("MSI Data Overview"),
               status = "success",
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = FALSE,
               closable = FALSE,
               shinycssloaders::withSpinner(shiny::verbatimTextOutput(outputId = ns("dataInfo")))
               )
             ),

      #(3) Get Mean Spectrum ===================================================
      column(width = 12),
      column(width = 5,
             box(
               width = 12,
               inputId = ns("input_card"),
               title = strong("Calculate Mean Spectrum"),
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = TRUE,
               closable = FALSE,
               sliderInput(inputId = ns("nth"),
                           label = "1. (Optional) Subset MSI data by select every nth pixel",
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
               title = strong("Mean Spectrum Overview"),
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
      column(width = 12),
      column(width = 5,
             box(
               width = 12,
               inputId = ns("input_card"),
               title = strong("Calculate Reference Peaks"),
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = TRUE,
               closable = FALSE,
               strong(code("Step 1. Peak Picking ")),
               br(),
               br(),
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
               strong(code("Step 2. Peak Alignment")),
               br(),
               br(),
               sliderInput(inputId = ns("paTolerance"),
                           label = "2.1 Choose peak aligment tolerance (ppm)",
                           min = 1,
                           max = 100,
                           value = 10,
                           step = 1
                           ),
               strong(code("Step 3. Peak Filter")),
               br(),
               br(),
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
               title = strong("Reference Peaks Overview"),
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
      column(width = 12),
      column(width = 5,
             box(
               width = 12,
               inputId = ns("input_card"),
               title = strong("Process MSI Data"),
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = TRUE,
               closable = FALSE,
               strong(code("Step 1. Normalize ")),
               br(),
               br(),
               radioButtons(inputId = ns("norMethod"),
                            label = "1.1 Select normalization method",
                            choices = list("tic" = "tic", "rms" = "rms"),
                            selected = "tic",
                            inline = TRUE,
                            ),
               strong(code("Step 2. Peak Binning")),
               br(),
               br(),
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
               title = strong("Processed MSI Data Overview"),
               status = "success",
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = TRUE,
               closable = FALSE,
               shiny::uiOutput(outputId = ns("downloadButton")),
               br(),
               br(),
               shinycssloaders::withSpinner(
                 shiny::verbatimTextOutput(outputId = ns("processedMSIInfo"))
                 )
               )
             )
))}


#' uploadData Server Functions
#' @noRd
mod_uploadData_server <- function(id, global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #(2) Load MSI Data =========================================================
    output$dataInfo <- renderPrint({
      #(2.1) validate input ----------------------------------------------------
      shiny::validate(need(input$imzmlFile$datapath != "", message = "No files found."))

      #(2.2) Get data path -----------------------------------------------------
      oldName <- input$imzmlFile$datapath
      newName <- file.path(dirname(input$imzmlFile$datapath), input$imzmlFile$name)
      file.rename(from = oldName, to = newName)
      msiFiles <- list.files(path = dirname(input$imzmlFile$datapath), full.names = TRUE)
      imzmlPath <- unique(grep(pattern = ".imzML", x = msiFiles, value = TRUE))
      ibdPath <- unique(grep(pattern = ".ibd", x = msiFiles, value = TRUE))

      #(2.3) Load MSI data -----------------------------------------------------
      shiny::validate(
        need(imzmlPath != "", "imzML file missing!"),
        need(ibdPath != "", "ibd file missing!"),
        need(length(imzmlPath) == length(ibdPath), "The number of imzML and idb files are not the same!")
        )
      if(input$setMass == "No"){
        selectedMassRange = NULL
      } else {
        selectedMassRange = input$massRange
      }
      global$msiData <- readMSI(path = imzmlPath,
                                massResolution = input$massResolution,
                                massRange = selectedMassRange,
                                workers = input$loadDataWorkers
                                )
      cat("The files have been loaded successfully. Below is the MSI data information:\n")
      cat("\n")
      global$msiData
    }) |>
      bindEvent(input$loadData)

    #(3) Get Mean Spectrum =====================================================
    output$meanSpecPlot <- plotly::renderPlotly({
      #(3.1) validate input --------------------------------------------------
      shiny::validate(need(global$msiData, message = "MSI data not found."))

      #(3.2) Calculate mean spectrum -----------------------------------------
      global$meanSpec <- global$msiData[, seq(1, max(Cardinal::pixels(global$msiData)), by = input$nth)] |>
        getMeanSpec(msiData = _,
                    worker = input$meanSpecWorkers
                    )
      plotMeanSpec(meanSpec = global$meanSpec, nth = input$nth)
      }) |>
      bindEvent(input$getMeanSpec)


    #(4) Get Reference Peaks ===================================================
    #(4.1) Calculate and display reference spec --------------------------------
    output$refPeakPlot <- plotly::renderPlotly({
      shiny::validate(need(global$meanSpec, message = "Mean spec not found."))
      global$refPeaks <- getRefPeaks(meanSpec = global$meanSpec,
                                     method = input$ppMethod,
                                     SNR = input$ppSNR,
                                     tolerance = input$paTolerance,
                                     freq.min = input$pfFreqmin,
                                     workers = input$getRefWorkers
                                     )
      plotMeanSpec(global$refPeaks, nth = 1)
    })|>
      bindEvent(input$getRefPeaks)

    #(4.2) Show reference peak info --------------------------------------------
    output$refPeakInfo <- shiny::renderPrint({
      shiny::req(global$refPeaks)
      cat("Below is the reference peak information:\n")
      cat("\n")
      global$refPeaks
      })

    #(5) Process MSI Data ======================================================
    #(5.1) Process and display MSI data --------------------------------------
    output$processedMSIInfo <- shiny::renderPrint({
      shiny::validate(
        need(global$msiData, message = "MSI data not found."),
        need(global$refPeaks, message = "Reference spec not found.")
        )
      global$processedMSIData <- processMSIData(msiData = global$msiData,
                                                method = input$norMethod,
                                                ref = global$refPeaks,
                                                tolerance = input$pbTolerance,
                                                workers = input$getProcessMSIWorkers
                                                )
      cat("Below is the processed MSI information:\n")
      cat("\n")
      global$processedMSIData
      }) |>
      bindEvent(input$processMSIData)

      #(5.2) Download processed MSI data ---------------------------------------
      ## download processed data
    output$downloadButton <- renderUI({
      downloadButton(
        outputId = ns("downloadProcessedData"),
        label = "Download Processed MSI Data",
        style="color: #fff; background-color: #a077b5; border-color: #a077b5"
        )
      }) |>
      bindEvent(input$processMSIData)

    output$downloadProcessedData <- downloadHandler(
      filename = function(){
        if(is.null(global$processedMSIData)){
          paste("No_Data_Found.rds", sep = "")
          } else {
          paste("processedMSIData.rds", sep = "")
          }
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

