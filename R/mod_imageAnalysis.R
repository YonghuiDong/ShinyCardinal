#' imageAnalysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_imageAnalysis_ui <- function(id){
  ns <- NS(id)
  tagList(
    #(1) Input -----------------------------------------------------------------
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

    #(2) Output ----------------------------------------------------------------
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
                                                 delay = 300,
                                                 delayType = "throttle",
                                                 clip = TRUE,
                                                 nullOutside = TRUE
                                                 ),
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

  )}

#' imageAnalysis Server Functions
#'
#' @noRd
mod_imageAnalysis_server <- function(id, msiRun, global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #(1) Select ROI ============================================================
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

    #(1.1) Select ROI ----------------------------------------------------------
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
        need(msiRun() != "All", message = "Please select only one MSI run when selecting ROI."),
        need(!(paste(input$roiName, msiRun(), sep = ":") %in% names(roiData$roiList)), message = "ROI name already exists, choose a different one.")
      )
      ## get roiList
      roiData$roiList <- append(roiData$roiList,
                                setNames(list(getROI(msiData = global$cleanedMSIData, selectedRun = msiRun(), roiDF = roiData$roiDF)), paste(input$roiName, msiRun(), sep = ":"))
                                )
      ## record corresponding cropType for each ROI
      roiData$cropType <- append(roiData$cropType, input$cropType)
      cat("\n")
      cat(paste0(input$roiName, " is successfully recorded.\n"))
      cat(names(roiData$roiList))

      #(1.2) Display Reset and Undo buttons ------------------------------------
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

    ##(1.3) Reset and show message----------------------------------------------
    output$resetROIMessage <- renderPrint({
      shiny::validate(need(length(roiData$roiList) > 0, message = "ROIs not found"))
      roiData$roiList <- list()
      roiData$cropType <- list()
      cat("All selected ROIs are removed.")
    }) |>
      bindEvent(input$resetROI)

    ##(1.4) Undo and show message ----------------------------------------------
    output$undoROIMessage <- renderPrint({
      shiny::validate(need(length(roiData$roiList) > 0, message = "ROIs not found"))
      removedName <- names(roiData$roiList)[length(roiData$roiList)]
      roiData$roiList <- head(roiData$roiList, -1)
      roiData$cropType <- head(roiData$cropType, -1)
      cat(paste("ROI", removedName, "is removed.", sep = " "))
    }) |>
      bindEvent(input$undoROI)

    ##(2) Plot selected ROIs ===================================================
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

    #(3) Analyse selected ROIs =================================================
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

    #(4) Show mz intensity profile alone pixels ==============================
    msiInfo <- reactiveValues(mzMin = NULL, mzMax = NULL)
    output$roiProfiles <- plotly::renderPlotly({
      shiny::req(global$cleanedMSIData)
      shiny::validate(need(input$mzROI != "", message = "m/z value not found"))
      mzROIList <- unique(text2Num(input$mzROI))
      msiInfo$mzMin <- round(min(Cardinal::mz(global$cleanedMSIData)), 4)
      msiInfo$mzMax <- round(max(Cardinal::mz(global$cleanedMSIData)), 4)
      shiny::validate(
        need(length(roiData$roiList) == 1, message = "Only one ROI is allowed for plotting ion intensity profiles"),
        need(min(mzROIList) >= msiInfo$mzMin & max(mzROIList) <= msiInfo$mzMax, message = paste("m/z value shoud between", msiInfo$mzMin, "and", msiInfo$mzMax, sep = " "))
      )
      plotROIProfile(msiData = global$cleanedMSIData, roiList = roiData$roiList, mz = mzROIList)
    }) |>
      bindEvent(input$plotROIProfile)

    #(5) Crop MSI data =========================================================
    output$croppingInfo <- renderPrint({
      shiny::req(global$cleanedMSIData)
      shiny::validate(need(length(roiData$roiList) > 0, message = "ROI not found."))
      shiny::req(roiData$cropType)
      #(5.1) Get and assign cropped data ---------------------------------------
      croppedData <- cropData(msiData = global$cleanedMSIData,
                              ROIs = roiData$roiList,
                              cropType = roiData$cropType
                              )
      global$cleanedMSIData <- croppedData
      roiData$roiList <- list()
      roiData$cropType <- list()
      #(5.2) Show reset button -------------------------------------------------
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

    #(6) IS-based quantification ===============================================
    ISQuan <- reactiveValues(df = NULL, subDF = NULL, fit = NULL, plot = NULL, prediction = NULL)

    #(6.1) Get and display quantification table --------------------------------
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

    #(6.2) Update cell values --------------------------------------------------
    observeEvent(input$intConTable_cell_edit, {
      ISQuan$df[input$intConTable_cell_edit$row, input$intConTable_cell_edit$col] <- input$intConTable_cell_edit$value
    })

    #(6.3) Display calibration curve -----------------------------------------
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

    #(6.4) Quantification ------------------------------------------------------
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
      shiny::validate(need(input$quanInt != "", message = "Intensities are missing."))
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
# mod_imageAnalysis_ui("imageAnalysis_1")

## To be copied in the server
# mod_imageAnalysis_server("imageAnalysis_1")
