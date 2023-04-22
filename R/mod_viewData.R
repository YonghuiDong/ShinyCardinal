#' viewData UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_viewData_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      #(0) User guide ==========================================================
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

      #(1) Optional: upload MSI rds Data =======================================
      column(width = 12, h5("Upload MSI rds Data (Optional)")),
      column(width = 4,
             box(
               width = 12,
               title = strong("Input Parameters"),
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = TRUE,
               closable = FALSE,
               p(style = "color:#C70039;", shiny::icon("bell"), strong("Note:")),
               p(style = "color:#C70039;", "1. This moduel is optional."),
               p(style = "color:#C70039;", "2. If you're starting directly from this module, you need to upload the rds file."),
               fileInput(inputId = ns("rdsMSI"),
                         label = "Please select the rds file.",
                         multiple = FALSE,
                         placeholder = "",
                         accept = c(".rds")
                         ),
               actionButton(inputId = ns("loadData"),
                            label = "Load",
                            icon = icon("paper-plane"),
                            style = "color: #fff; background-color: #67ac8e; border-color: #67ac8e"
                            )
               )
             ),
      #(1.2) Upload MSI rds data result ----------------------------------------
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
                 shiny::verbatimTextOutput(outputId = ns("infoMSIData"))
                 )
               )
             ),

      #(2) Image View ==========================================================
      column(width = 12, h6("View MSI Images")),
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
               selectInput(inputId = ns('msiRun'),
                           label = '(optional) Select a MSI run to display.',
                           choices = NULL,
                           selected = NULL
                           ),
               textInput(inputId = ns("mzValues"),
                         label = "1. Enter m/z values to visualize",
                         placeholder = "For multiple m/z values, separate them by a comma."
                         ),
               numericInput(inputId = ns("massWindow"),
                           label = "2. Set mass tolerance window (Da)",
                           min = 0,
                           max = 10,
                           value = 0.001,
                           step = 0.001
                           ),
               selectInput(inputId = ns("normalizeImage"),
                           label = "3. Select normalization method to the image.",
                           multiple = FALSE,
                           choices = list("none" = "none", "linear" = "linear"),
                           selected = "linear"
                           ),
               selectInput(inputId = ns("contrastImage"),
                           label = "4. Select contrast enhancement method to the image.",
                           multiple = FALSE,
                           choices = list("none" = "none", "histogram" = "histogram", "suppression" = "suppression"),
                           selected = "suppression"
                           ),
               selectInput(inputId = ns("smoothImage"),
                           label = "5. Select smoothing method to the image.",
                           multiple = FALSE,
                           choices = list("none" = "none", "gaussian" = "gaussian", "adaptive" = "adaptive"),
                           selected = "none"
                           ),
               selectInput(inputId = ns("colorImage"),
                           label = "6. Slect color scale",
                           multiple = FALSE,
                           choices = list("cividis" = "cividis",
                                          "viridis" = "viridis",
                                          "magma" = "magma",
                                          "inferno" = "inferno",
                                          "plasma" = "plasma",
                                          "rainbow" = "rainbow",
                                          "darkrainbow" = "darkrainbow",
                                          "jet" = "jet",
                                          "hot" = "hot",
                                          "cool" = "cool",
                                          "redblack" = "redblack",
                                          "greenblack" = "greenblack",
                                          "blueblack" = "blueblack",
                                          "grayscale" = "grayscale"
                                          ),
                           selected = "viridis"
                           ),
               radioButtons(inputId = ns("modeImage"),
                            label = "Do you prefer light or dark mode?",
                            choices = list("light" = "light", "dark" = "dark"),
                            selected = "dark",
                            inline = TRUE
                            ),
               radioButtons(inputId = ns("superposeImage"),
                            label = "Do you want to superpose different m/z images",
                            choices = list("Yes" = 1, "NO" = 0),
                            selected = 0,
                            inline = TRUE
                            ),
               actionButton(inputId = ns("viewImage"),
                            label = "Plot",
                            icon = icon("paper-plane"),
                            style = "color: #fff; background-color: #67ac8e; border-color: #67ac8e"
                            )
               )
             ),

      #(2.2) Output ============================================================
      column(width = 8,
             box(
               width = 12,
               inputId = "report_card",
               title = strong("Result"),
               status = "success",
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = TRUE,
               closable = FALSE,
               downloadButton(outputId = ns("saveImage"),
                              label = "Download Image",
                              icon = icon("download"),
                              style = "color: #fff; background-color: #67ac8e; border-color: #67ac8e"
                              ),
               shinycssloaders::withSpinner(
                 image = 'www/img/cardinal.gif',
                 shiny::verbatimTextOutput(outputId = ns("mzList"))
                 ),
               shiny::plotOutput(outputId = ns("ionImage"),
                                 click = ns("plot_click"),
                                 hover = ns("plot_hover")
                                 ),
               shiny::verbatimTextOutput(outputId = ns("info")),
               column(width = 6,
                      shiny::uiOutput(outputId = ns("resetButton"))
                      ),
               column(width = 6,
                      shiny::uiOutput(outputId = ns("undoButton"))
                      ),
               shiny::tableOutput(outputId = ns("pixelTable")),
               plotly::plotlyOutput(outputId = ns("selectedSpec"))
               )
             ),
      #(3) Image Analysis ======================================================
      column(width = 12, h6("Image Analysis")),
      column(width = 4,
             box(
               width = 12,
               inputId = "report_card",
               title = strong("Select ROI"),
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = TRUE,
               closable = FALSE,
               p(style = "color:#C70039;", shiny::icon("bell"), strong("Note:")),
               p(style = "color:#C70039;", "1. In this module, you can draw a line or select
                 a region of interest (ROI) to display ion intensities of the selected ions."),
               p(style = "color:#C70039;", "2. Click on the Ion image to start selecting ROI, and click it again to finsh."),
               p(style = "color:#C70039;", "3. Click on New ROI button to start selecting another ROI."),
               textInput(inputId = ns("roiName"),
                         label = "1. Enter a name of ROI",
                         value = NULL,
                         placeholder = "use a concise and unique roi name",
                         ),
               column(width = 6,
                      actionButton(inputId  = ns("newROI"),
                                   label = "New ROI",
                                   icon = icon("circle"),
                                   style = "color: #fff; background-color: #67ac8e; border-color: #67ac8e"
                                   )
                      ),
               column(width = 6,
                      actionButton(inputId  = ns("recordROI"),
                                   label = "Record ROI",
                                   icon = icon("pencil"),
                                   style = "color: #fff; background-color: #67ac8e; border-color: #67ac8e"
                                   )
                      ),
               br(),
               br(),
               br(),
               strong("2. Display Selected ROIs"),
               br(),
               br(),
               column(width = 12,
                      actionButton(inputId  = ns("displayROI"),
                                   label = "Display",
                                   icon = icon("eye"),
                                   style = "color: #fff; background-color: #67ac8e; border-color: #67ac8e"
                                   )
                      )

               )
             ),
      #(3.2) Image analysis Output ---------------------------------------------
      column(width = 8,
             box(
               width = 12,
               inputId = "report_card",
               title = strong("Result"),
               status = "success",
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = TRUE,
               closable = FALSE,
               shiny::plotOutput(outputId = ns("ionImageROI"),
                                 hover = hoverOpts(id = ns("hover"),
                                                   delay = 500,
                                                   delayType = "throttle",
                                                   clip = TRUE,
                                                   nullOutside = TRUE),
                                 click = ns("click")
                                 ),
               br(),
               shiny::verbatimTextOutput(outputId = ns("infoROI")),
               shiny::uiOutput(outputId = ns("resetROIButton")),
               shiny::verbatimTextOutput(outputId = ns("modifiedROIMessage")),
               shiny::plotOutput(outputId = ns("selectedROIPlot"))
               )
             )

      ))}

#' viewData Server Functions
#'
#' @noRd
mod_viewData_server <- function(id, global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #(1) Load MSI rds Data =====================================================
    output$infoMSIData <- shiny::renderPrint({
      shiny::validate(need(!is.null(input$rdsMSI), message = "rds file not found"))
      global$processedMSIData <- readRDS(input$rdsMSI$datapath)
      if(is.null(global$processedMSIData)){
        cat("MSI data not loaded, please check if your rds file is empty.\n")
        } else {
        cat("MSI data loaded successfully!\n")
        global$processedMSIData
        }
      }) |>
      bindEvent(input$loadData)

    #(2) Visualize MS images ===================================================

    #(2.0) Update MSI run ------------------------------------------------------
    observeEvent(global$processedMSIData,{
      if(length(levels(Cardinal::run(global$processedMSIData))) == 1){
        ## if there is only one run, I don't have to add "All"; It will be easier to record run name for ROI selection.
        updateSelectInput(session = session,
                          inputId = "msiRun", ## no name space
                          choices = levels(Cardinal::run(global$processedMSIData)),
                          selected = levels(Cardinal::run(global$processedMSIData))
                          )
      } else {
        updateSelectInput(session = session,
                          inputId = "msiRun", ## no name space
                          choices = c("All" = "All", levels(Cardinal::run(global$processedMSIData))),
                          selected = "All"
                          )
        }
    })

    msiInfo <- reactiveValues(mzList = NULL, mzMin = NULL, mzMax = NULL, ionImage = NULL)

    #(2.1) Show Input m/z Info  ------------------------------------------------
    ## bindEvent() is used here to show users the feedback messages
    output$mzList <- renderPrint({
      shiny::validate(
        need(global$processedMSIData, message = "MSI data not found."),
        need(input$mzValues != "", message = "m/z value is missing."),
        need(input$massWindow > 0, message = "mass tolerance should be positive value.")
        )
      msiInfo$mzList <- unique(text2Num(input$mzValues))
      msiInfo$mzMin <- round(min(Cardinal::mz(global$processedMSIData)), 4)
      msiInfo$mzMax <- round(max(Cardinal::mz(global$processedMSIData)), 4)
      shiny::validate(need(min(msiInfo$mzList) >= msiInfo$mzMin & max(msiInfo$mzList) <= msiInfo$mzMax,
                           message = paste("m/z value shoud between", msiInfo$mzMin, "and", msiInfo$mzMax, sep = " ")))
      ## Get ion images
      msiInfo$ionImage <- plotImage(msiData = global$processedMSIData,
                                    mz = msiInfo$mzList,
                                    smooth.image = input$smoothImage,
                                    plusminus = input$massWindow,
                                    colorscale = input$colorImage,
                                    normalize.image = input$normalizeImage,
                                    contrast.enhance = input$contrastImage,
                                    superpose = as.logical(as.numeric(input$superposeImage)),
                                    msiRun = input$msiRun
                                    )
      cat(msiInfo$mzList)
    }) |>
      bindEvent(input$viewImage)

    #(2.2) Show MSI images -----------------------------------------------------

    output$ionImage <- renderPlot({
      shiny::req(msiInfo$ionImage)
      if(input$modeImage == "light"){
        Cardinal::lightmode()
      } else {
        Cardinal::darkmode()
      }
      msiInfo$ionImage
      })

    #(2.3) Download MSI images -------------------------------------------------
    output$saveImage <- downloadHandler(
      filename = function(){
        if(is.null(print(msiInfo$ionImage))){
          paste0(Sys.Date(), "_no_image_found", ".pdf")
        } else {
          paste0(Sys.Date(), "_ionImage", ".pdf")
        }
      },
      content = function(file){
        pdf(file)
        print(msiInfo$ionImage)
        dev.off()
      })

    #(2.4) Display selected  spectrum ------------------------------------------
    observeEvent(input$viewImage, {
      output$resetButton <- renderUI({
        shiny::req(msiInfo$ionImage)
        actionButton(
          inputId = ns("reset"),
          label = "Reset",
          icon = icon("circle"),
          style="color: #fff; background-color: #a077b5; border-color: #a077b5"
          )
        })
      output$undoButton <- renderUI({
        shiny::req(msiInfo$ionImage)
        actionButton(
          inputId = ns("undo"),
          label = "Undo",
          icon = icon("undo"),
          style="color: #fff; background-color: #a077b5; border-color: #a077b5"
          )
        })
      ## initiate click event
      rv_click <- reactiveValues(df = data.frame(x = double(), y = double()))
      observeEvent(input$plot_click, {
        shiny::req(input$plot_click$x >= min(Cardinal::coord(global$processedMSIData)$x) &
                     input$plot_click$x <= max(Cardinal::coord(global$processedMSIData)$x)
                   )
        shiny::req(input$plot_click$y >= min(Cardinal::coord(global$processedMSIData)$y)
                   & input$plot_click$y <= max(Cardinal::coord(global$processedMSIData)$y)
                   )
        rv_click$df <-
          isolate(rv_click$df) |>
          rbind(data.frame(x = as.integer(round(input$plot_click$x, 0)),
                           y = as.integer(round(input$plot_click$y, 0))
                           )
                ) |>
          (\(x) x[!duplicated(x), ])()
        })
      observeEvent(input$undo, {
        rv_click$df <- head(isolate(rv_click$df), -1)
        })
      observeEvent(input$reset, {
        rv_click$df <- data.frame(x = double(), y = double())
        })
      output$info <- renderText({
        shiny::req(msiInfo$ionImage)
        print("Please click on the image to select pixels of interest.")
        })
      output$pixelTable <- renderTable({
        shiny::req(msiInfo$ionImage)
        shiny::req(nrow(rv_click$df) > 0)
        rv_click$df
        })
      output$selectedSpec <- plotly::renderPlotly({
        shiny::req(nrow(rv_click$df) > 0)
        shiny::req(global$processedMSIData)
        plotPixelSpec(msiData = global$processedMSIData, pixelDF = rv_click$df)
        })
    })

    #(3) Image Analysis ========================================================

    ##(3.1) Select ROI ---------------------------------------------------------
    inxROI <- reactiveValues(x = double(), y = double())
    draw <- reactiveVal(value = FALSE)
    observeEvent(input$click, {
      temp <- draw()
      draw(!temp)
      if(!draw()) {
        inxROI$x <- c(inxROI$x, NA)
        inxROI$y <- c(inxROI$y, NA)
        }
      })
    observeEvent(input$newROI, {
      inxROI$x <- double()
      inxROI$y <- double()
      })
    observeEvent(input$hover, {
      if (draw()) {
        inxROI$x <- c(inxROI$x, input$hover$x)
        inxROI$y <- c(inxROI$y, input$hover$y)
        }
      })
    output$ionImageROI <- renderPlot({
      shiny::req(global$processedMSIData)
      shiny::req(msiInfo$ionImage)
      print(msiInfo$ionImage)
      lines(x = inxROI$x,
            y = inxROI$y,
            type = "b",
            lwd = 2
            )
      })

    #(3.2) Select ROI ----------------------------------------------------------
    roiData <- reactiveValues(roiDF = NULL, roiMSIData = list())
    output$infoROI <- renderPrint({
      ## get the x,y coordinates of ROI
      roiData$roiDF <- data.frame(x = round(inxROI$x, 0), y = round(inxROI$y, 0)) |>
        (\(x) x[!duplicated(x), ])() |>
        na.omit(object = _)
      ## check input
      shiny::req(msiInfo$ionImage)
      shiny::validate(
        need(nrow(roiData$roiDF) > 0, message = "No ROI selected!"),
        need(all(roiData$roiDF$x >= min(Cardinal::coord(global$processedMSIData)$x) & all(roiData$roiDF$x <= max(Cardinal::coord(global$processedMSIData)$x))),
             message = "Selected ROI is out of x-aixs range"),
        need(all(roiData$roiDF$y >= min(Cardinal::coord(global$processedMSIData)$y) & all(roiData$roiDF$y <= max(Cardinal::coord(global$processedMSIData)$y))),
             message = "Selected ROI is out of y-aixs range"),
        need(input$roiName != "", message = "Please enter an ROI name"),
        need(input$msiRun != "All", message = "Please select only one MSI run when selecting ROI."),
        need(!(paste(input$roiName, input$msiRun, sep = ":") %in% names(roiData$roiMSIData)),
             message = "The entered ROI name already exist, please user another one.")
        )
      ## subset global$processedMSIData
      roiData$roiMSIData <- append(roiData$roiMSIData,
                                   setNames(list(getROI(msiData = global$processedMSIData, selectedRun = input$msiRun, roiDF = roiData$roiDF)), paste(input$roiName, input$msiRun, sep = ":"))
                                   )
      cat("\n")
      cat(paste0(input$roiName, " is successfully recorded.\n"))
      cat(names(roiData$roiMSIData))

      #(3.3) Display Reset button ----------------------------------------------
      output$resetROIButton <- renderUI({
        actionButton(
          inputId = ns("resetROI"),
          label = "Reset selected ROI",
          icon = icon("circle"),
          style="color: #fff; background-color: #a077b5; border-color: #a077b5"
          )
        })
      }) |>
      bindEvent(input$recordROI)

    ##(3.4) Reset and show message----------------------------------------------
    output$modifiedROIMessage <- renderPrint({
      shiny::validate(need(!is.null(roiData$roiMSIData), message = "No ROIs found"))
      roiData$roiMSIData <- list()
      cat("All selected ROIs are removed.")
      }) |>
      bindEvent(input$resetROI)

    ##(3.5) Undo and show message ----------------------------------------------
    ##(3.6) Plot selected ROIs -------------------------------------------------
    output$selectedROIPlot <- renderPlot({
      shiny::req(global$processedMSIData)
      shiny::validate(need(length(roiData$roiMSIData) > 0, message = "No ROIs found"))
      region <- makeFactor2(roiData$roiMSIData)
      Cardinal::image(global$processedMSIData, region ~ x*y, key = TRUE)
    }) |>
      bindEvent(input$displayROI)





})}

## To be copied in the UI
# mod_viewData_ui("viewData_1")

## To be copied in the server
# mod_viewData_server("viewData_1")



