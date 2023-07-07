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
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = FALSE,
               closable = FALSE,
               p("1. This section includes the most commont MSI data analysis tools, i.e., deisotoping, background noises and matrix peaks removal, image visualization, ROI analysis,
                    MSI data cropping and internal standard-based quantification."),
               p("2. Some steps are optional, feel free to skip them if they are not relevant to your studies."),
               p("3. The web version of ShinyCardinal does not support parallel computation. Please always set the number of workers to 1 to avoid out of memory error."),
               p("4. Please watch video tutorials 2-8:", a(href = "https://www.youtube.com/@MSI_WIS/videos", shiny::icon("youtube", style = "color:#d17789; font-size:25px;"), target="_blank"))
              )
            ),

      #(1) Optional: upload MSI rds Data =======================================
      column(width = 12, h5("Upload MSI rds Data (optional)")),
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

      #(2) Deisotoping =========================================================
      column(width = 12, h5("Deisotoping (optional)")),
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
                            choices = c("High-mass-resolution" = "HR", "Low-mass-resolution" = "LR"),
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
      ##(2.2) Deisotoping output -----------------------------------------------
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
      ),

      #(3) Background noise and matrix removal =================================
      column(width = 12, h5("Remove Background Noises and Matrix Peaks (optional)")),
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
                           max = 10,
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

      ##(3.2) Background noise and matrix removal output -----------------------
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
             ),

      #(4) Image View ==========================================================
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
                           label = '(optional) Select a MSI run to display',
                           choices = NULL,
                           selected = NULL
                           ),
               textInput(inputId = ns("mzValues"),
                         label = "1. Enter m/z values to visualize",
                         placeholder = "For multiple m/z values, separate them by a comma"
                         ),
               strong("2. Set mass tolerance window (Da)"),
               br(),
               br(),
               p(style = "color:#C70039;", shiny::icon("bell"), strong("Note:")),
               p(style = "color:#C70039;", "1. Without mass tolerance, exact m/z is displayed."),
               p(style = "color:#C70039;", "2. Otherwise, entered m/z ± tolerance is displayed."),
               numericInput(inputId = ns("massWindow"),
                            label = NULL,
                            min = 0,
                            max = 10,
                            value = NA,
                            step = 0.001
                            ),
               sliderInput(inputId = ns("zlim"),
                           label = "3. Set the range of intensity bar",
                           min = 0,
                           max = 1,
                           value = c(0, 1),
                           step = 0.001
                           ),
               selectInput(inputId = ns("contrastImage"),
                           label = "4. Select image contrast enhancement method",
                           multiple = FALSE,
                           choices = list("none" = "none", "histogram" = "histogram", "suppression" = "suppression"),
                           selected = "suppression"
                           ),
               selectInput(inputId = ns("smoothImage"),
                           label = "5. Select image smoothing method",
                           multiple = FALSE,
                           choices = list("none" = "none", "gaussian" = "gaussian", "adaptive" = "adaptive"),
                           selected = "none"
                           ),
               selectInput(inputId = ns("colorImage"),
                           label = "6. Slect color scale",
                           multiple = FALSE,
                           choices = list("cividis" = "cividis",
                                          "cividis black" = "cividisBlack",
                                          "viridis" = "viridis",
                                          "viridis black" = "viridisBlack",
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
                            label = "Use light or dark mode?",
                            choices = list("Light" = "light", "Dark" = "dark"),
                            selected = "dark",
                            inline = TRUE
                            ),
               radioButtons(inputId = ns("showColorkey"),
                            label = "Should show colorkey?",
                            choices = list("Yes" = "1", "No" = "0"),
                            selected = "1",
                            inline = TRUE
                            ),
               radioButtons(inputId = ns("superposeImage"),
                            label = "Should superpose images?",
                            choices = list("Yes" = "1", "No" = "0"),
                            selected = "0",
                            inline = TRUE
                            ),
               actionButton(inputId = ns("viewImage"),
                            label = "Plot",
                            icon = icon("paper-plane"),
                            style = "color: #fff; background-color: #67ac8e; border-color: #67ac8e"
                            )
              )
            ),

      #(4.2) Output ============================================================
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
               column(width = 12, shiny::uiOutput(outputId = ns("downloadImageButton"))),
               column(width = 12,
                      shinycssloaders::withSpinner(
                        image = 'www/img/cardinal.gif',
                        shiny::verbatimTextOutput(outputId = ns("mzList"))
                      )
               ),
               column(width = 12,
                      shiny::plotOutput(outputId = ns("ionImage"),
                                        click = ns("plot_click"),
                                        hover = ns("plot_hover")
                                        )
                      ),
               shiny::verbatimTextOutput(outputId = ns("info")),
               column(width = 12,
                      column(width = 6,
                             shiny::uiOutput(outputId = ns("resetButton"))
                             ),
                      column(width = 6,
                             shiny::uiOutput(outputId = ns("undoButton"))
                             )
                      ),
               shiny::tableOutput(outputId = ns("pixelTable")),
               plotly::plotlyOutput(outputId = ns("selectedSpec"))
              )
            ),

      #(5) Image Analysis ======================================================
      column(width = 12, h6("Image Analysis")),
      column(width = 4,
             box(
               width = 12,
               inputId = "report_card",
               title = strong("Input Parameters"),
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = TRUE,
               closable = FALSE,
               p(style = "color:#C70039;", shiny::icon("bell"), strong("Note:")),
               p(style = "color:#C70039;", "1. Click on ion image to select ROI, click again to finish.."),
               p(style = "color:#C70039;", "2. Click on New ROI button to select another ROI."),
               strong("1. Select ROIs"),
               br(),
               br(),
               textInput(inputId = ns("roiName"),
                         label = "Enter a name of ROI",
                         value = NULL,
                         placeholder = "use a concise and unique roi name",
                         ),
               column(width = 6,
                      actionButton(inputId  = ns("newROI"),
                                   label = "New ROI",
                                   icon = icon("map-marker"),
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
               hr(style="border-top: solid 2px; border-color: #c9d6d3;"),
               strong("2. Display Selected ROIs"),
               br(),
               br(),
               radioButtons(inputId = ns("roiKey"),
                            label = "Display ROI names?",
                            choices = c("Yes" = "1", "No" = "0"),
                            selected = 1,
                            inline = TRUE
                            ),
               column(width = 12,
                      actionButton(inputId  = ns("displayROI"),
                                   label = "Display",
                                   icon = icon("eye"),
                                   style = "color: #fff; background-color: #67ac8e; border-color: #67ac8e"
                                   )
                      ),
               br(),
               br(),
               hr(style="border-top: solid 2px; border-color: #c9d6d3;"),
               strong("3. Analyse Selected ROIs"),
               br(),
               br(),
               p(style = "color:#C70039;", shiny::icon("bell"), strong("Note:")),
               p(style = "color:#C70039;", "1. This module requires specific ROI name format for statistics."),
               p(style = "color:#C70039;", "2. Use roiName_metaName format to define ROI and meta data."),
               p(style = "color:#C70039;", "3. e.g. four ROIs defined as ROI1_WT, ROI2_WT, ROI1_Mu, ROI2_Mu, 2 are WT and 2 are mutant."),
               p(style = "color:#C70039;", "4. ROIs can be from the same or different runs."),
               p(style = "color:#C70039;", "5. Hypothesis test requires replicates."),
               p(style = "color:#C70039;", "6. It's possible to create replicates by dividing sample area into sub-ROIs."),
               p(style = "color:#C70039;", "7. Invalid ROI names or lack of replicates result in mean intensity calculation only."),
               column(width = 12,
                      actionButton(inputId = ns("compareROIs"),
                                   label = "Compare ROIs",
                                   icon = icon("paper-plane"),
                                   style = "color: #fff; background-color: #67ac8e; border-color: #67ac8e"
                                   )
                      ),
               br(),
               br(),
               hr(style="border-top: solid 2px; border-color: #c9d6d3;"),
               strong("4. (optional) Plot m/z profile along the ROI pixels"),
               br(),
               br(),
               p(style = "color:#C70039;", shiny::icon("bell"), strong("Note:")),
               p(style = "color:#C70039;", "1. This module shows ion intensity profiles of selected m/z values on ROI pixels."),
               p(style = "color:#C70039;", "2. e.g. draw a line over MSI image, and visualize ion intensity along it."),
               p(style = "color:#C70039;", "3. Only one ROI is allowed in this module."),
               p(style = "color:#C70039;", "4. No specific requirement for ROI name."),
               textInput(inputId = ns("mzROI"),
                         label = "Enter m/z values to plot the profile",
                         placeholder = "For multiple m/z values, seperate them by a comma"
                         ),
               column(width = 12,
                      actionButton(inputId = ns("plotROIProfile"),
                                   label = "Plot",
                                   icon = icon("paper-plane"),
                                   style = "color: #fff; background-color: #67ac8e; border-color: #67ac8e"
                                   )
                      ),
               br(),
               br(),
               hr(style="border-top: solid 2px; border-color: #c9d6d3;"),
               strong("5. (optional) ROI-based data cropping"),
               br(),
               br(),
               p(style = "color:#C70039;", shiny::icon("bell"), strong("Note:")),
               p(style = "color:#C70039;", "1. This module removes unwanted areas."),
               p(style = "color:#C70039;", "2. No specific requirement for ROI name."),
               p(style = "color:#C70039;", "3. Select cropping mode before recording ROI."),
               p(style = "color:#C70039;", "4. ROIs can be from the same or different runs."),
               p(style = "color:#C70039;", "5. Updated MSI data are then used for image and data analysis."),
               radioButtons(inputId = ns("cropType"),
                            label = "Cropping mode: keep areas inside/outside the ROI?",
                            choices = c("inside" = "inside", "outside" = "outside"),
                            selected = "inside",
                            inline = TRUE
                            ),
               column(width = 12,
                      actionButton(inputId = ns("cropMSIData"),
                                   label = "Crop Data",
                                   icon = icon("eraser"),
                                   style = "color: #fff; background-color: #67ac8e; border-color: #67ac8e"
                                   )
                      ),
               br(),
               br(),
               hr(style="border-top: solid 2px; border-color: #c9d6d3;"),
               strong("6. (optional) Internal standard-based quantification"),
               br(),
               br(),
               p(style = "color:#C70039;", shiny::icon("bell"), strong("Note:")),
               p(style = "color:#C70039;", "1. No specific requirement for ROI name."),
               numericInput(inputId = ns("mzIS"),
                            label = "Enter m/z value of the internal standard",
                            value = NA
                            ),
               column(width = 12,
                      actionButton(inputId = ns("showQuanTable"),
                                   label = "Show intensity-concentration table",
                                   icon = icon("table"),
                                   style = "color: #fff; background-color: #67ac8e; border-color: #67ac8e"
                                   )
                      ),
               br(),
               br(),
               p(style = "color:#C70039;", "2. Once the table is displayed, please fill the concentration information."),
               p(style = "color:#C70039;", "3. Then click the plot button to generate calibration curve."),
               column(width = 12, shiny::uiOutput(outputId = ns("showPlotCalCurveButton"))),
               br(),
               br(),
               p(style = "color:#C70039;", "4. Enter the intensites for quantification."),
               p(style = "color:#C70039;", "5. Click Quantify button to get quantification result."),
               column(width = 12, shiny::uiOutput(outputId = ns("getQuan"))),
               )
             ),
      #(5.2) Image analysis Output ---------------------------------------------
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
               column(width = 6, shiny::uiOutput(outputId = ns("resetROIButton"))),
               column(width = 6, shiny::uiOutput(outputId = ns("undoROIButton"))),
               shiny::verbatimTextOutput(outputId = ns("resetROIMessage")),
               shiny::verbatimTextOutput(outputId = ns("undoROIMessage")),
               shiny::plotOutput(outputId = ns("selectedROIPlot")),
               br(),
               br(),
               shinycssloaders::withSpinner(
                 image = 'www/img/cardinal.gif',
                 DT::dataTableOutput(outputId = ns("roiStatistics"))
                ),
               plotly::plotlyOutput(outputId = ns("roiProfiles")),
               shiny::verbatimTextOutput(outputId = ns("croppingInfo")),
               shiny::uiOutput(outputId = ns("resetCropButton")),
               shiny::verbatimTextOutput(outputId = ns("resetCropMessage")),
               DT::dataTableOutput(outputId = ns("intConTable")),
               shiny::verbatimTextOutput(outputId = ns("calCurveInfo")),
               plotly::plotlyOutput(outputId = ns("calCurve")),
               DT::dataTableOutput(outputId = ns("predictionTable"))
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

    #(2) Deisotoping ===========================================================
    #(2.1) deisotoping ---------------------------------------------------------
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
      bindCache(input$deisotopeTol, input$deisotopePCC) |>
      bindEvent(input$deisotope)

    #(2.2) Reset feature -------------------------------------------------------
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

    #(3) Background Removal ====================================================
    #(3.1) Perform colocalization ----------------------------------------------
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
      bindCache(input$noisePeak, input$nth) |>
      bindEvent(input$noiseColoc)

    #(3.2) Display buttons -----------------------------------------------------
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

    #(3.3) Delete features -----------------------------------------------------
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

    #(3.4) Reset feature -------------------------------------------------------
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

    #(4) Visualize MS images ===================================================

    #(4.0) Update MSI run ------------------------------------------------------
    observeEvent(global$processedMSIData,{
      ## In case users did not perform step #2
      if(is.null(global$cleanedMSIData)){
        global$cleanedMSIData <- global$processedMSIData
      }
      if(length(levels(Cardinal::run(global$cleanedMSIData))) == 1){
        ## if there is only one run, I don't have to add "All"; It will be easier to record run name for ROI selection.
        updateSelectInput(session = session,
                          inputId = "msiRun", ## no name space
                          choices = levels(Cardinal::run(global$cleanedMSIData)),
                          selected = levels(Cardinal::run(global$cleanedMSIData))
                          )
      } else{
        updateSelectInput(session = session,
                          inputId = "msiRun", ## no name space
                          choices = c("All" = "All", levels(Cardinal::run(global$cleanedMSIData))),
                          selected = "All"
                          )
        }
    })

    msiInfo <- reactiveValues(mzList = NULL, mzMin = NULL, mzMax = NULL)

    #(4.1) Show Input m/z Info  ------------------------------------------------
    output$mzList <- renderPrint({
      shiny::validate(
        need(global$cleanedMSIData, message = "MSI data not found."),
        need(input$mzValues != "", message = "m/z value is missing."),
        need(input$massWindow > 0 | is.na(input$massWindow), message = "mass tolerance should be positive value.")
      )
      msiInfo$mzList <- unique(text2Num(input$mzValues))
      msiInfo$mzMin <- round(min(Cardinal::mz(global$cleanedMSIData)), 4)
      msiInfo$mzMax <- round(max(Cardinal::mz(global$cleanedMSIData)), 4)
      shiny::validate(need(min(msiInfo$mzList) >= msiInfo$mzMin & max(msiInfo$mzList) <= msiInfo$mzMax,
                           message = paste("m/z value should between", msiInfo$mzMin, "and", msiInfo$mzMax, sep = " ")))
      ## Get ion images
      global$ionImage <- plotImage(msiData = global$cleanedMSIData,
                                   mz = msiInfo$mzList,
                                   smooth.image = input$smoothImage,
                                   plusminus = input$massWindow,
                                   colorscale = input$colorImage,
                                   zlim = input$zlim,
                                   colorkey = as.logical(as.numeric(input$showColorkey)),
                                   contrast.enhance = input$contrastImage,
                                   superpose = as.logical(as.numeric(input$superposeImage)),
                                   msiRun = input$msiRun
                                   )
      cat(msiInfo$mzList)
    }) |>
      bindEvent(input$viewImage)

    #(4.2) Show MSI images -----------------------------------------------------
    output$ionImage <- renderPlot({
      shiny::req(global$ionImage)
      if(input$modeImage == "light"){
        Cardinal::lightmode()
      } else{
        Cardinal::darkmode()
      }
      global$ionImage
    })

    #(4.3) Enlarge and Download MSI images -------------------------------------
    output$downloadImageButton <- renderUI({
      shiny::req(print(global$ionImage))
      tagList(
        column(width = 6,
               selectInput(inputId = ns("downloadImageType"),
                           label = "Select format",
                           choices = c("pdf" = "pdf", "png" = "png"),
                           selected = "pdf"
                           )
               ),
        column(width = 6,
               selectInput(inputId = ns("downloadEachImage"),
                           label = "Save images as seperate PDF files?",
                           choices = c("no" = "no", "yes" = "yes"),
                           selected = "no"
                           )
               ),
        column(width = 6,
               downloadButton(outputId = ns("downloadImage"),
                              label = "Download Image",
                              icon = icon("download"),
                              style="color: #fff; background-color: #a077b5; border-color: #a077b5"
                              )
               ),
        column(width = 6,
               actionButton(inputId = ns("enlargeButton"),
                              label = "Enlarge Image",
                              icon = icon("search-plus"),
                              style="color: #fff; background-color: #a077b5; border-color: #a077b5"
                              )
               )
        )
    }) # no need to used bindEvent here, otherwise the download button only shows after 2nd click.

    ##(4.3.1) Enlarge image ----------------------------------------------------
    observeEvent(input$enlargeButton, {
      showModal(modalDialog(
        tags$head(tags$style(HTML(".modal-dialog { width: 100vw; }"))),
        plotOutput(outputId = ns("enlargedImage"), height = "1000px"),
      ))
      output$enlargedImage <- renderPlot({
        global$ionImage
      })
    })

    ##(4.3.2) Download image ---------------------------------------------------
    output$downloadImage <- downloadHandler(
      filename = function(){paste0("ionImage.", input$downloadImageType)},
      content = function(file){
        #download separate images on each page, only support pdf format
        if(input$downloadEachImage == "yes" & length(msiInfo$mzList) > 1){
          if(input$modeImage == "light"){
            Cardinal::lightmode()
          } else{
            Cardinal::darkmode()
          }
          pdf(file = file, onefile = TRUE)
          withProgress(message = 'Making plot', value = 0, {
            for(i in seq_along(msiInfo$mzList)){
              print(plotImage(msiData = global$cleanedMSIData,
                              mz = msiInfo$mzList[i],
                              smooth.image = input$smoothImage,
                              plusminus = input$massWindow,
                              colorscale = input$colorImage,
                              contrast.enhance = input$contrastImage,
                              superpose = FALSE,
                              msiRun = input$msiRun
                              )
                    )
              incProgress(1/length(msiInfo$mzList), detail = paste("Ploting image", i))
            }
          })
          dev.off()
        } else{
          #download combined images
          if(input$downloadImageType == "pdf"){
            pdf(file)
            print(global$ionImage)
            dev.off()
          } else{
            png(file, width = 8000, height = 8000, res = 900)
            print(global$ionImage)
            dev.off()
          }
        }
      }
    )

    #(4.4) Display selected  spectrum ------------------------------------------
    observeEvent(input$viewImage, {
      output$resetButton <- renderUI({
        shiny::req(global$ionImage)
        actionButton(
          inputId = ns("reset"),
          label = "Reset",
          icon = icon("circle"),
          style="color: #fff; background-color: #a077b5; border-color: #a077b5"
        )
      })
      output$undoButton <- renderUI({
        shiny::req(global$ionImage)
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
        shiny::req(input$plot_click$x >= min(Cardinal::coord(global$cleanedMSIData)$x)
                   & input$plot_click$x <= max(Cardinal::coord(global$cleanedMSIData)$x)
                   )
        shiny::req(input$plot_click$y >= min(Cardinal::coord(global$cleanedMSIData)$y)
                   & input$plot_click$y <= max(Cardinal::coord(global$cleanedMSIData)$y)
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
        shiny::req(global$ionImage)
        print("Please click on the image to select pixels of interest.")
      })
      output$pixelTable <- renderTable({
        shiny::req(global$ionImage)
        shiny::req(nrow(rv_click$df) > 0)
        rv_click$df
      })
      output$selectedSpec <- plotly::renderPlotly({
        shiny::req(nrow(rv_click$df) > 0)
        shiny::req(global$cleanedMSIData)
        plotPixelSpec(msiData = global$cleanedMSIData, pixelDF = rv_click$df)
      })
    })

    #(5) Image Analysis ========================================================

    ##(5.1) Select ROI =========================================================
    inxROI <- reactiveValues(x = double(), y = double())
    draw <- reactiveVal(value = FALSE)
    observeEvent(input$click, {
      temp <- draw()
      draw(!temp)
      if(!draw()){
        inxROI$x <- c(inxROI$x, NA)
        inxROI$y <- c(inxROI$y, NA)
      }
    })
    observeEvent(input$newROI, {
      inxROI$x <- double()
      inxROI$y <- double()
    })
    observeEvent(input$hover, {
      if(draw()){
        inxROI$x <- c(inxROI$x, input$hover$x)
        inxROI$y <- c(inxROI$y, input$hover$y)
      }
    })
    output$ionImageROI <- renderPlot({
      shiny::req(global$cleanedMSIData)
      shiny::req(global$ionImage)
      print(global$ionImage)
      lines(x = inxROI$x,
            y = inxROI$y,
            type = "b",
            lwd = 2
            )
    })

    #(5.1.1) Select ROI --------------------------------------------------------
    ## Define roiData
    ## (1) roiDF: x-y pixel of ROI;
    ## (2) roiList: list of selected ROIs, containing TRUE/FALSE value;
    ## (3) cropType: keep area inside or outside ROI;
    roiData <- reactiveValues(roiDF = NULL, roiList = list(), cropType = list())
    output$infoROI <- renderPrint({
      roiData$roiDF <- data.frame(x = round(inxROI$x, 0), y = round(inxROI$y, 0)) |>
        (\(x) x[!duplicated(x), ])() |>
        na.omit(object = _)
      ## check input
      shiny::req(global$ionImage)
      shiny::validate(
        need(nrow(roiData$roiDF) > 0, message = "No ROI selected!"),
        need(all(roiData$roiDF$x >= min(Cardinal::coord(global$cleanedMSIData)$x) & all(roiData$roiDF$x <= max(Cardinal::coord(global$cleanedMSIData)$x))),
             message = "Selected ROI is out of x-aixs range"),
        need(all(roiData$roiDF$y >= min(Cardinal::coord(global$cleanedMSIData)$y) & all(roiData$roiDF$y <= max(Cardinal::coord(global$cleanedMSIData)$y))),
             message = "Selected ROI is out of y-aixs range"),
        need(input$roiName != "", message = "Please enter an ROI name"),
        need(input$msiRun != "All", message = "Please select only one MSI run when selecting ROI."),
        need(!(paste(input$roiName, input$msiRun, sep = ":") %in% names(roiData$roiList)), message = "ROI name already exists, choose a different one.")
      )
      ## get roiList
      roiData$roiList <- append(roiData$roiList,
                                setNames(list(getROI(msiData = global$cleanedMSIData, selectedRun = input$msiRun, roiDF = roiData$roiDF)), paste(input$roiName, input$msiRun, sep = ":"))
                                )
      ## record corresponding cropType for each ROI
      roiData$cropType <- append(roiData$cropType, input$cropType)
      cat("\n")
      cat(paste0(input$roiName, " is successfully recorded.\n"))
      cat(names(roiData$roiList))

      #(5.1.2) Display Reset and Undo buttons ----------------------------------
      output$resetROIButton <- renderUI({
        actionButton(
          inputId = ns("resetROI"),
          label = "Reset selected ROI",
          icon = icon("circle"),
          style="color: #fff; background-color: #a077b5; border-color: #a077b5"
        )
      })

      output$undoROIButton <- renderUI({
        actionButton(
          inputId = ns("undoROI"),
          label = "Undo selected ROI",
          icon = icon("undo"),
          style="color: #fff; background-color: #a077b5; border-color: #a077b5"
        )
      })
    }) |>
      bindEvent(input$recordROI)

    ##(5.1.3) Reset and show message--------------------------------------------
    output$resetROIMessage <- renderPrint({
      shiny::validate(need(length(roiData$roiList) > 0, message = "ROIs not found"))
      roiData$roiList <- list()
      roiData$cropType <- list()
      cat("All selected ROIs are removed.")
    }) |>
      bindEvent(input$resetROI)

    ##(5.1.4) Undo and show message --------------------------------------------
    output$undoROIMessage <- renderPrint({
      shiny::validate(need(length(roiData$roiList) > 0, message = "ROIs not found"))
      removedName <- names(roiData$roiList)[length(roiData$roiList)]
      roiData$roiList <- head(roiData$roiList, -1)
      roiData$cropType <- head(roiData$cropType, -1)
      cat(paste("ROI", removedName, "is removed.", sep = " "))
    }) |>
      bindEvent(input$undoROI)

    ##(5.2) Plot selected ROIs =================================================
    output$selectedROIPlot <- renderPlot({
      shiny::req(global$cleanedMSIData)
      shiny::validate(need(length(roiData$roiList) > 0, message = "ROIs not found"))
      region <- makeFactor2(roiList = roiData$roiList)
      Cardinal::image(global$cleanedMSIData,
                      region ~ x*y,
                      key = as.logical(as.numeric(input$roiKey)),
                      xlab = "Selected ROIs"
                      )
    }) |>
      bindEvent(input$displayROI)

    #(5.3) Analyse selected ROIs ===============================================
    output$roiStatistics <- DT::renderDT(server = FALSE, {
      shiny::req(global$cleanedMSIData)
      shiny::validate(need(length(roiData$roiList) > 1, message = "At least two ROIs are needed."))
      combinedroiList <- combine2(msiData = global$cleanedMSIData, roiList = roiData$roiList)
      statResult <- roiStat(roiMSIData = combinedroiList)
      DT::datatable(
        statResult,
        caption = "Summary of statistical results for user defined ROIs.",
        extensions ="Buttons",
        options = list(dom = 'Bfrtip',
                       buttons = list(list(extend = 'csv', filename= 'roiStat')),
                       scrollX = TRUE
                       ),
        rownames = FALSE
      )
    }) |>
      bindEvent(input$compareROIs)

    #(5.4) Show mz intensity profile alone pixels ==============================
    output$roiProfiles <- plotly::renderPlotly({
      shiny::req(global$cleanedMSIData)
      shiny::validate(need(input$mzROI != "", message = "m/z value not found"))
      mzROIList <- unique(text2Num(input$mzROI))
      shiny::validate(
        need(length(roiData$roiList) == 1, message = "Only one ROI is allowed for plotting ion intensity profiles"),
        need(min(mzROIList) >= msiInfo$mzMin & max(mzROIList) <= msiInfo$mzMax, message = paste("m/z value shoud between", msiInfo$mzMin, "and", msiInfo$mzMax, sep = " "))
      )
      plotROIProfile(msiData = global$cleanedMSIData, roiList = roiData$roiList, mz = mzROIList)
    }) |>
      bindEvent(input$plotROIProfile)

    #(5.5) Crop MSI data =======================================================
    output$croppingInfo <- renderPrint({
      shiny::req(global$cleanedMSIData)
      shiny::validate(need(length(roiData$roiList) > 0, message = "ROI not found."))
      shiny::req(roiData$cropType)
      #(5.5.1) Get and assign cropped data -------------------------------------
      croppedData <- cropData(msiData = global$cleanedMSIData,
                              ROIs = roiData$roiList,
                              cropType = roiData$cropType
                              )
      global$cleanedMSIData <- croppedData
      roiData$roiList <- list()
      roiData$cropType <- list()
      #(5.5.2) Show reset button -----------------------------------------------
      output$resetCropButton <- renderUI({
        shiny::req(croppedData)
        actionButton(
          inputId = ns("resetCropping"),
          label = "Reset cropped MSI Data",
          icon = icon("undo"),
          style="color: #fff; background-color: #a077b5; border-color: #a077b5"
        )
      })
      cat("Data cropping done. Select another ROIs in case you want continue cropping the data.\n")
      cat("Click the reset button to restore the data.\n")
      cat("Below is the cropped data information.\n")
      global$cleanedMSIData
    }) |>
      bindEvent(input$cropMSIData)

    output$resetCropMessage <- renderPrint({
      global$cleanedMSIData <- global$processedMSIData
      roiData$roiList <- list()
      roiData$cropType <- list()
      cat("Data cropping canceled.\n")
      cat("Note: Background noise and matrix peak removal result is also deleted if you have done that step.\n")
      global$cleanedMSIData
    }) |>
      bindEvent(input$resetCropping)

    #(5.6) IS-based quantification =============================================

    ISQuan <- reactiveValues(df = NULL, subDF = NULL, fit = NULL, plot = NULL, prediction = NULL)
    #(5.6.1) Get and display quantification table ------------------------------
    output$intConTable <- DT::renderDataTable(server = FALSE, {
      shiny::req(global$cleanedMSIData)
      shiny::validate(
        need(length(roiData$roiList) > 1, message = "At least two ROIs are needed."),
        need(input$mzIS >= min(Cardinal::mz(global$cleanedMSIData)) & input$mzIS <= max(Cardinal::mz(global$cleanedMSIData)), message = "Entered m/z is out of range.")
      )
      roiMSIData <- combine2(msiData = global$cleanedMSIData, roiList = roiData$roiList)
      ISQuan$df <- roiQuantification(roiMSIData = roiMSIData, mz = input$mzIS)
      shiny::req(nrow(ISQuan$df) >= 2)
      DT::datatable(ISQuan$df,
                    editable = TRUE,
                    caption = "Intensity ~ concentration table",
                    extensions = "Buttons",
                    options = list(dom = 'Bfrtip',
                                   buttons = list(list(extend = 'csv', filename= 'IS_Table')),
                                   scrollX = TRUE
                                   )
                    )
    }) |>
      bindEvent(input$showQuanTable)

    #(5.6.2) Update cell values ------------------------------------------------
    observeEvent(input$intConTable_cell_edit, {
      ISQuan$df[input$intConTable_cell_edit$row, input$intConTable_cell_edit$col] <- input$intConTable_cell_edit$value
    })

    #(5.6.3) Display calibration curve -----------------------------------------
    output$showPlotCalCurveButton <- renderUI({
      shiny::req(nrow(ISQuan$df) >= 2)
      actionButton(inputId = ns("plotCal"),
                   label = "Plot",
                   icon = icon("paper-plane"),
                   style = "color: #fff; background-color: #67ac8e; border-color: #67ac8e"
                   )
    })

    observeEvent(input$plotCal, {
      output$calCurve <- plotly::renderPlotly({
        shiny::validate(need(length(input$intConTable_rows_selected) >= 2, message = "Please select at least two rows."))
        ISQuan$subDF <- ISQuan$df[input$intConTable_rows_selected, ]
        shiny::validate(need(all(ISQuan$subDF$Concentration >= 0), message = "Concentration cannot be negative values."))
        result <- plotCalCurve(df = ISQuan$subDF)
        ISQuan$fit <- result$fit
        ISQuan$plot <- result$plot
        ISQuan$plot
      })
    })

    #(5.6.4) Quantification ----------------------------------------------------
    output$getQuan <- renderUI({
      shiny::req(nrow(ISQuan$df) >= 2)
      tagList(
        textInput(inputId = ns("quanInt"),
                  label = "Enter intensities for quantification",
                  value = NULL,
                  placeholder = "For multiple values, please use comma to seperate them."
                  ),
        actionButton(inputId = ns("ISQuantify"),
                     label = "Quantify",
                     icon = icon("paper-plane"),
                     style = "color: #fff; background-color: #67ac8e; border-color: #67ac8e"
                     )
      )
    })

    output$predictionTable <- DT::renderDT({
      shiny::req(ISQuan$fit)
      shiny::validate(need(input$mzValues != "", message = "Intensities are missing."))
      ISQuan$prediction <- getQuan(mod = ISQuan$fit, Intensity = text2Num(input$quanInt))
      DT::datatable(ISQuan$prediction,
                    caption = "Quantification table",
                    extensions = "Buttons",
                    options = list(dom = 'Bfrtip',
                                   buttons = list(list(extend = 'csv', filename= 'IS_Quantification')),
                                   scrollX = TRUE
                                   )
                    )
    }) |>
      bindEvent(input$ISQuantify)

    observeEvent(input$ISQuantify, ignoreNULL = FALSE, {
      shiny::req(ISQuan$plot)
      shiny::req(nrow(ISQuan$subDF) >=2)
      shiny::req(nrow(ISQuan$prediction) >= 1)
      output$calCurve <- plotly::renderPlotly({
        color <- ifelse(ISQuan$prediction$Intensity >= min(ISQuan$subDF$Intensity) & ISQuan$prediction$Intensity <= max(ISQuan$subDF$Intensity), "#9ed9b4", "#fc0317")
        ISQuan$plot %>%
          plotly::add_markers(data = ISQuan$prediction,
                              x = ~ Intensity,
                              y = ~ Concentration,
                              marker = list(color = color, symbol = 'x', size = 15, line = list(color = "#999797", width = 2))
                              )
      })
    })

    output$calCurveInfo <- renderPrint({
      shiny::req(nrow(ISQuan$df) >= 2)
      shiny::req(ISQuan$plot)
      cat("Below is the calibration curve: \n")
      cat("The quantified results will also be displayed in the calibration curve. \n")
    }) |>
      bindEvent(input$ISQuantify)

})}

## To be copied in the UI
# mod_viewData_ui("viewData_1")

## To be copied in the server
# mod_viewData_server("viewData_1")



