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
               collapsed = FALSE,
               closable = FALSE
               )
             ),

      #(2.1) Data Input ==========================================================
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

      #(2.2) Output ==============================================================
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

      column(width = 12),
      #(3.1) Get Mean Spectrum =================================================
      column(width = 5,
             box(
               width = 12,
               inputId = ns("input_card"),
               title = strong("Get Mean Spectrum"),
               status = "primary",
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = TRUE,
               closable = FALSE,
               sliderInput(inputId = ns("nth"),
                           label = "1. (Optional) Subset MSI Data by select every nth pixel",
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

      #(3.2) Output ============================================================
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
             )

      )
    )
  }


#' uploadData Server Functions
#' @noRd
mod_uploadData_server <- function(id, global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #(1) Load MSI Data =========================================================
    observeEvent(input$loadData,{
      #(1.1) Get data path -----------------------------------------------------
      shiny::req(input$imzmlFile)
      oldName <- input$imzmlFile$datapath
      newName <- file.path(dirname(input$imzmlFile$datapath), input$imzmlFile$name)
      file.rename(from = oldName, to = newName)
      msiFiles <- list.files(path = dirname(input$imzmlFile$datapath), full.names = TRUE)
      imzmlPath <- unique(grep(pattern = ".imzML", x = msiFiles, value = TRUE))
      ibdPath <- unique(grep(pattern = ".ibd", x = msiFiles, value = TRUE))

      #(1.2) Load MSI data -----------------------------------------------------
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

    #(2) Get Mean Spectrum =====================================================
    observeEvent(input$getMeanSpec,{
      w2 <- waiter::Waiter$new(id = ns("meanSpecPlot"),
                               html = h4("Be patient. Cardinal is running..."),
                               image = 'www/img/cardinal.gif',
                               fadeout = TRUE
                               )
      w2$show()
      #(2.1) Calculate mean spec -----------------------------------------------
      shiny::req(global$msiData)
      global$meanSpec <- global$msiData[, seq(1, max(Cardinal::pixels(global$msiData)), by = input$nth)] |>
        getMeanSpec(msiData = _,
                    worker = input$meanSpecWorkers
                    )
      #(2.1) Plot mean spec ----------------------------------------------------
      output$meanSpecPlot <- plotly::renderPlotly({
        on.exit({w2$hide()})
        plotMeanSpec(global$meanSpec, shiny::isolate(input$nth))
      })
    })




  })
}

## To be copied in the UI
# mod_uploadData_ui("uploadData_1")

## To be copied in the server
# mod_uploadData_server("uploadData_1")

