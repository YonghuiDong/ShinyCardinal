#' readImzML UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_readImzML_ui <- function(id){
  ns <- NS(id)
  tagList(
    #(1.1) Input ---------------------------------------------------------------
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
             shinyFiles::shinyDirButton(id = ns("chooseMSI"), label = 'Choose Directory', title = "Select Folder", icon = icon("sitemap")),
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
                       placeholder = "Please select both .imzMl and .ibd files",
                       accept = c(".imzML", ".ibd")
                       ),
             hr(),
             radioButtons(inputId = ns("msiDataType"),
                          label = "2.1 Choose MSI data type",
                          choices = list("High-mass-resolution" = "HR", "Low-mass-resolution" = "LR"),
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
                          choices = list("Profile mode" = "0", "Centroid mode" = "1"),
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
               shiny::verbatimTextOutput(outputId = ns("msiDataInfo"))
              )
            )
          )

)}

#' readImzML Server Functions
#'
#' @noRd
mod_readImzML_server <- function(id, global, export_msiDataType = FALSE){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observe({
      switch(EXPR = input$msiDataType,
             "HR" = updateSliderInput(inputId = "massResolution", min = 1, max = 20, value = 10, step = 1),
             "LR" = updateSliderInput(inputId = "massResolution", min = 20, max = 400, value = 50, step = 5)
             )
    })
    filePath <- reactiveValues(imzmlPath = NULL, ibdPath = NULL)
    #(1.1) Option 1 ------------------------------------------------------------
    volumes <- shinyFiles::getVolumes()()
    shinyFiles::shinyDirChoose(input, id = "chooseMSI", roots = volumes)
    output$msiDataInfo <- renderPrint({
      folder_path <- shinyFiles::parseDirPath(roots = volumes, input$chooseMSI)
      filePath$imzmlPath <- unique(list.files(path = folder_path, pattern = ".imzML", full.names = TRUE, ignore.case = TRUE))
      filePath$ibdPath <- unique(list.files(path = folder_path, pattern = ".ibd", full.names = TRUE, ignore.case = TRUE))
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
        need(length(filePath$imzmlPath) == length(filePath$ibdPath), message = "The number of imzML and idb files is not equal!")
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

    #(1.3) Export input/output -------------------------------------------------
    export_list <- list()
    if(export_msiDataType){
      export_list$msiDataType <- reactive(input$msiDataType)
    }
    if(length(export_list) > 0){
      return(export_list)
    }
})}

## To be copied in the UI
# mod_readImzML_ui("readImzML_1")

## To be copied in the server
# mod_readImzML_server("readImzML_1")
