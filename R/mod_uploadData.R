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
      waiter::use_waiter(),
      #(1) User guide ==========================================================
      column(width = 12,
             box(
               width = 12,
               title = strong("User Guide"),
               status = "warning",
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = TRUE,
               closable = FALSE
               )
             ),

      #(2) Data Input ========================================================
      column(width = 5,
             box(
               width = 12,
               inputId = "input_card",
               title = strong("Data Input Panel"),
               status = "primary",
               solidHeader = FALSE,
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
                           max = 3000,
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
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = FALSE,
               closable = FALSE,
               verbatimTextOutput(outputId = ns("dataInfo"))
               )
             ),

      #(3) Get Mean Spectrum ===================================================
      column(width = 12),
      column(width = 5,
             box(
               width = 12,
               inputId = ns("input_card"),
               title = strong("Get Mean Spectrum"),
               status = "primary",
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = FALSE,
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
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = FALSE,
               closable = FALSE,
               plotly::plotlyOutput(outputId = ns("meanSpecPlot"))
               )
             ),

      #(4) Get Reference Peaks =================================================
      column(width = 12),
      column(width = 5,
             box(
               width = 12,
               inputId = ns("input_card"),
               title = strong("Get Reference Peaks"),
               status = "primary",
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = FALSE,
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
               title = strong("Reference Peak Overview"),
               status = "success",
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = FALSE,
               closable = FALSE,
               shiny::verbatimTextOutput(outputId = ns("refPeakInfo")),
               plotly::plotlyOutput(outputId = ns("refPeakPlot"))
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
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = FALSE,
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
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = FALSE,
               closable = FALSE,
               shiny::uiOutput(outputId = ns("downloadButton")),
               br(),
               br(),
               shiny::verbatimTextOutput(outputId = ns("processedMSIInfo")),
               shiny::plotOutput(outputId = ns("TICImage"),
                                 click = ns("plot_click"),
                                 hover = ns("plot_hover")
                                 ),
               shiny::verbatimTextOutput(outputId = ns("pixelInfo"))
               )
             )
      ))}


#' uploadData Server Functions
#' @noRd
mod_uploadData_server <- function(id, global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #(2) Load MSI Data =========================================================
    observeEvent(input$loadData,{
      #(2.1) Get data path -----------------------------------------------------
      shiny::req(input$imzmlFile)
      oldName <- input$imzmlFile$datapath
      newName <- file.path(dirname(input$imzmlFile$datapath), input$imzmlFile$name)
      file.rename(from = oldName, to = newName)
      msiFiles <- list.files(path = dirname(input$imzmlFile$datapath), full.names = TRUE)
      imzmlPath <- unique(grep(pattern = ".imzML", x = msiFiles, value = TRUE))
      ibdPath <- unique(grep(pattern = ".ibd", x = msiFiles, value = TRUE))

      #(2.2) Load MSI data -----------------------------------------------------
      shiny::req(imzmlPath)
      shiny::req(ibdPath)
      shiny::req(length(imzmlPath) == length(ibdPath))
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

      output$dataInfo <- renderPrint({
        shiny::validate(
          need(!is.null(imzmlPath), "imzML file missing!"),
          need(!is.null(ibdPath), "ibd file missing!"),
          need(length(imzmlPath) == length(ibdPath), "The number of imzML and idb files are not the same!"),
          need(!is.null(global$msiData), "Files not loaded. Please check your raw data.")
        )
        cat("The files have been loaded successfully. Below is the MSI data information:\n")
        cat("\n")
        print(global$msiData)
      })
    })

    #(3) Get Mean Spectrum =====================================================
    observeEvent(input$getMeanSpec,{
      w3 <- waiter::Waiter$new(id = ns("meanSpecPlot"),
                               html = strong("Please wait, running..."),
                               image = 'www/img/cardinal.gif',
                               fadeout = TRUE
                               )
      w3$show()
      #(3.1) Calculate mean spec -----------------------------------------------
      shiny::req(global$msiData)
      global$meanSpec <- global$msiData[, seq(1, max(Cardinal::pixels(global$msiData)), by = input$nth)] |>
        getMeanSpec(msiData = _,
                    worker = input$meanSpecWorkers
                    )
      #(3.1) Plot mean spec ----------------------------------------------------
      output$meanSpecPlot <- plotly::renderPlotly({
        on.exit({w3$hide()})
        plotMeanSpec(global$meanSpec, shiny::isolate(input$nth))
      })
    })

    #(4) Get Reference Peaks ===================================================
    observeEvent(input$getRefPeaks,{
      w4 <- waiter::Waiter$new(id = ns("refPeakPlot"),
                               html = strong("Please wait, running..."),
                               image = 'www/img/cardinal.gif',
                               fadeout = TRUE
                               )
      w4$show()
      #(4.1) Calculate reference peaks -----------------------------------------
      shiny::req(global$meanSpec)
      global$refPeaks <- getRefPeaks(meanSpec = global$meanSpec,
                                     method = input$ppMethod,
                                     SNR = input$ppSNR,
                                     tolerance = input$paTolerance,
                                     freq.min = input$pfFreqmin,
                                     workers = input$getRefWorkers
                                     )
      #(4.2) Plot reference spec -----------------------------------------------
      output$refPeakInfo <- shiny::renderPrint({
        on.exit({w4$hide()})
        cat("Below is the reference peak information:\n")
        cat("\n")
        print(global$refPeaks)
      })
      output$refPeakPlot <- plotly::renderPlotly({
        plotMeanSpec(global$refPeaks, nth = 1)
      })
    })

    #(5) Process MSI Data ======================================================
    observeEvent(input$processMSIData,{
      w5 <- waiter::Waiter$new(id = ns("TICImage"),
                               html = strong("Please wait, running..."),
                               image = 'www/img/cardinal.gif',
                               fadeout = TRUE
                               )
      w5$show()
      #(5.1) process MSI data --------------------------------------------------
      shiny::req(global$msiData)
      shiny::req(global$refPeaks)
      global$processedMSIData <- processMSIData(msiData = global$msiData,
                                                method = input$norMethod,
                                                ref = global$refPeaks,
                                                tolerance = input$pbTolerance,
                                                workers = input$getProcessMSIWorkers
                                                )
      tic <- Cardinal::pixelApply(global$processedMSIData, sum)
      #(5.2) Show processed MSI data information -------------------------------

      ## download processed data
      output$downloadButton <- renderUI({
        downloadButton(
          outputId = ns("downloadProcessedData"),
          label = "Download Processed MSI Data",
          style="color: #fff; background-color: #a077b5; border-color: #a077b5"
          )
        })
      output$downloadProcessedData <- downloadHandler(
        filename = function() {
          paste("processedMSIData.rds", sep = "")
          },
        content = function(file) {
          saveRDS(global$processedMSIData, file)
        }
      )

      output$processedMSIInfo <- shiny::renderPrint({
        on.exit({w5$hide()})
        cat("Below is the processed MSI information:\n")
        cat("\n")
        print(global$processedMSIData)
        cat("\n")
        cat("Below is the TIC image of the processed MSI data:\n")
      })
      output$TICImage <- shiny::renderPlot({
        Cardinal::darkmode()
        Cardinal::image(global$processedMSIData,
                        tic ~ x * y,
                        contrast.enhance="suppression",
                        normalize.image = "linear"
                        )
      })
      output$pixelInfo <- shiny::renderText({
        xy_str <- function(e) {
          if(is.null(e)) return("NULL\n")
          paste0("x = ", round(e$x, 0), " y = ", round(e$y, 0), "\n")
        }
        paste0(
          paste0("Pixel information:", "\n"),
          "click: ", xy_str(input$plot_click),
          "hover: ", xy_str(input$plot_hover)
          )
      })

    })





  })
}

## To be copied in the UI
# mod_uploadData_ui("uploadData_1")

## To be copied in the server
# mod_uploadData_server("uploadData_1")

