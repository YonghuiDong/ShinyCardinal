#' segmentation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_segmentation_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
    #(0) User guide ============================================================
    column(width = 12,
           box(
             width = 12,
             title = strong("User Guide"),
             status = "warning",
             solidHeader = TRUE,
             collapsible = TRUE,
             collapsed = FALSE,
             closable = FALSE,
             )
           ),

    #(1) Optional: upload MSI rds Data =========================================
    column(width = 12, h5("Upload MSI Data")),
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
    #(1.2) Upload MSI rds data result ------------------------------------------
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

    #(2) PCA ===================================================================
    column(width = 12, h6("Pricipal Component Analysis (PCA)")),
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
             selectInput(inputId = ns('msiRunPCA'),
                         label = '(optional) Select a MSI run to display.',
                         choices = NULL,
                         selected = NULL
                         ),
             sliderInput(inputId = ns("nComp"),
                         label = "1. Select the number of principal components",
                         min = 2,
                         max = 20,
                         value = 2,
                         step = 1
                         ),
             radioButtons(inputId = ns("centerPCA"),
                          label = "2. Should the data be centered?",
                          choices = list("Yes" = 1, "No" = 0),
                          selected = 1,
                          inline = TRUE
                          ),
             radioButtons(inputId = ns("scalePCA"),
                          label = "3. Should the data be scaled?",
                          choices = list("Yes" = 1, "No" = 0),
                          selected = 0,
                          inline = TRUE
                          ),
             actionButton(inputId = ns("viewPCA"),
                          label = "Plot",
                          icon = icon("paper-plane"),
                          style = "color: #fff; background-color: #67ac8e; border-color: #67ac8e"
                          ),
             hr(),
             p(style = "color:#C70039;", "Set image parameters:"),
             selectInput(inputId = ns("pcaClusters"),
                         label = "Select PCA clusters to visualize",
                         choices = NULL,
                         multiple = TRUE
                         ),
             radioButtons(inputId = ns("modePCAImage"),
                          label = "Choose light or dark mode",
                          choices = list("light" = "light", "dark" = "dark"),
                          selected = "dark",
                          inline = TRUE
                          ),
             radioButtons(inputId = ns("superposePCAImage"),
                          label = "Should superpose different images?",
                          choices = list("Yes" = 1, "No" = 0),
                          selected = 0,
                          inline = TRUE
                          )
             )
           ),

    #(2.2) PCA Output ----------------------------------------------------------
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
             column(width = 12, shiny::uiOutput(outputId = ns("downloadPCAImageButton"))),
             column(width = 12,
                    shinycssloaders::withSpinner(image = 'www/img/cardinal.gif',
                                                 shiny::verbatimTextOutput(outputId = ns("infoPCAImage"))
                                                 )
                    ),
             column(width = 12, shiny::plotOutput(outputId = ns("pcaImages"))),
             column(width = 12, shiny::verbatimTextOutput(outputId = ns("infoLoadings"))),
             column(width = 12, plotly::plotlyOutput(outputId = ns("pcaLoadingsSpec"))),
             column(width = 12, DT::dataTableOutput(outputId = ns("pcaLoadingTable")))
            )
          ),

    #(3) Spatial Shrunken Centroids Clustering =================================
    column(width = 12, h6("Spatial-aware Shrunken Centroids Clustering (SSC)")),
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
             selectInput(inputId = ns('msiRunSSC'),
                         label = '(optional) Select a MSI run to display.',
                         choices = NULL,
                         selected = NULL
                         ),
             textInput(inputId = ns("r"),
                       label = "1. r: The spatial neighborhood radius of nearby pixels to consider.",
                       placeholder = "For multiple values, separate them by a comma.",
                       value = "2"
                       ),
             textInput(inputId = ns("s"),
                       label = "2. s: Enter the sparsity thresholding parameter by which to shrink the t-statistics",
                       placeholder = "For multiple values, separate them by a comma.",
                       value = "0"
                       ),
             textInput(inputId = ns("k"),
                       label = "3. k: Enter the maximum number of segments",
                       placeholder = "For multiple values, separate them by a comma.",
                       value = "2"
                       ),
             radioButtons(inputId = ns("sscMethod"),
                          label = "4. Choose the method to use to calculate the spatial smoothing weights",
                          choices = list("gaussian" = "gaussian", "adaptive" = "adaptive"),
                          selected = "gaussian",
                          inline = TRUE
                          ),
             radioButtons(inputId = ns("sscDist"),
                          label = "5. Choose the type of distance metric",
                          choices = list("radial" = "radial", "manhattan" = "manhattan",
                                         "minkowski" = "minkowski", "chebyshev" = "chebyshev"),
                          selected = "chebyshev",
                          inline = FALSE
                          ),
             actionButton(inputId = ns("viewSSC"),
                          label = "Plot",
                          icon = icon("paper-plane"),
                          style = "color: #fff; background-color: #67ac8e; border-color: #67ac8e"
                          ),
             hr(),
             p(style = "color:#C70039;", "Set image parameters:"),
             radioButtons(inputId = ns("modeSSCImage"),
                          label = "Do you prefer light or dark mode?",
                          choices = list("light" = "light", "dark" = "dark"),
                          selected = "dark",
                          inline = TRUE
                          ),
             radioButtons(inputId = ns("superposeSSCImage"),
                          label = "Should superpose different images?",
                          choices = list("Yes" = 1, "No" = 0),
                          selected = 1,
                          inline = TRUE
                          ),
             hr(),
             p(style = "color:#C70039;", "Subset SSC images by selecting r, s, and k below:"),
             column(width = 4,
                    selectInput(inputId = ns("outputR"),
                                label = "Select r:",
                                choices = NULL,
                                multiple = FALSE
                                )
                    ),
             column(width = 4,
                    selectInput(inputId = ns("outputS"),
                                label = "Select s:",
                                choices = NULL,
                                multiple = FALSE
                                )
                    ),
             column(width = 4,
                    selectInput(inputId = ns("outputK"),
                                label = "Select k:",
                                choices = NULL,
                                multiple = FALSE
                                )
                    ),
             hr(),
             column(width = 12,
                    selectInput(inputId = ns("sscClusters"),
                                label = "Select specific clusters to visualize",
                                choices = NULL,
                                multiple = TRUE
                                )
                    )
             )
           ),

    #(3.2) SSC Output ----------------------------------------------------------
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
             column(width = 12, shinycssloaders::withSpinner(
               image = 'www/img/cardinal.gif',
               shiny::verbatimTextOutput(outputId = ns("infoSSCImage"))
               )
             ),
             column(width = 12, shiny::uiOutput(outputId = ns("downloadSSCImageButton"))),
             column(width = 12, shiny::plotOutput(outputId = ns("sscImages"))),
             column(width = 12, shiny::verbatimTextOutput(outputId = ns("infoSSCSpec"))),
             column(width = 12, plotly::plotlyOutput(outputId = ns("sscStatisticSpec"))),
             column(width = 12, DT::dataTableOutput(outputId = ns("sscStatisticTable")))
             )
           )
))}


#' segmentation Server Functions
#'
#' @noRd
mod_segmentation_server <- function(id, global){
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

    #(2) PCA ===================================================================
    #(2.1) Update MSI run ------------------------------------------------------
    observeEvent(global$processedMSIData, {
      if(is.null(global$cleanedMSIData)){
        global$cleanedMSIData <- global$processedMSIData
      }
      updateSelectInput(session = session,
                        inputId = "msiRunPCA",
                        choices = c(levels(Cardinal::run(global$cleanedMSIData))),
                        selected = c(levels(Cardinal::run(global$cleanedMSIData)))[1]
                        )
    })

    #(2.2) Update PCA clusters -------------------------------------------------
    observeEvent(input$viewPCA, {
      updateSelectInput(session = session,
                        inputId = "pcaClusters",
                        choices = 1:input$nComp,
                        selected = 1:input$nComp,
                        )
    })

    #(2.3) Calculate PCA -------------------------------------------------------
    msiPCA <- reactiveValues(result = NULL, image = NULL, loading = NULL)
    output$infoPCAImage <- shiny::renderPrint({
      shiny::validate(need(global$cleanedMSIData, message = "MSI data not found!"))
      set.seed(2023)
      msiPCA$result <- getPCA(msiData = global$cleanedMSIData,
                              ncomp = input$nComp,
                              center = as.logical(as.integer(input$centerPCA)),
                              scale = as.logical(as.integer(input$scalePCA)),
                              msiRun = input$msiRunPCA
                              )
      cat("Below are PCA images:\n")
    }) |>
      bindEvent(input$viewPCA)

    #(2.4) Display PCA images --------------------------------------------------
    output$pcaImages <- shiny::renderPlot({
      shiny::req(global$cleanedMSIData)
      shiny::req(msiPCA$result)
      shiny::validate(need(input$pcaClusters != "", message = "At least 1 PC should be selected."))
      if(input$modePCAImage == "light"){
        Cardinal::lightmode()
      } else {
        Cardinal::darkmode()
      }
      msiPCA$image <- plotPCAImage(pcaResult = msiPCA$result,
                                   clusters = as.numeric(input$pcaClusters),
                                   superpose = as.logical(as.integer(input$superposePCAImage))
                                   )
      ## Display PCA images
      ## Need par() to display separate images
      if(input$superposePCAImage == "1" | length(input$pcaClusters) == 1){
        msiPCA$image
      } else{
        layout1 <- c(ceiling(length(input$pcaClusters)/2), 2)
        layout2 <- c(2, 3)
        if(prod(layout1) < prod(layout2)){
          par(mfrow = layout1)
        } else{
          par(mfrow = layout2)
        }
        msiPCA$image
      }
    })

    #(2.5) Download and enlarge PCA images -------------------------------------
    output$downloadPCAImageButton <- renderUI({
      shiny::req(print(msiPCA$image))
      tagList(
        column(width = 6,
               downloadButton(outputId = ns("downloadPCAImage"),
                              label = "Download Image",
                              icon = icon("download"),
                              style="color: #fff; background-color: #a077b5; border-color: #a077b5"
                              )
               ),
        column(width = 6,
               actionButton(inputId = ns("enlargePCAButton"),
                            label = "Enlarge Image",
                            icon = icon("search-plus"),
                            style="color: #fff; background-color: #a077b5; border-color: #a077b5"
                            )
               )
        )
    })

    ##(2.5.1) Enlarge image ----------------------------------------------------
    observeEvent(input$enlargePCAButton, {
      showModal(modalDialog(
        tags$head(tags$style(HTML(".modal-dialog { width: 100vw; }"))),
        plotOutput(outputId = ns("enlargedPCAImage"), height = "1000px"),
      ))
      output$enlargedPCAImage <- renderPlot({
        if(input$superposePCAImage == "1" | length(input$pcaClusters) == 1){
          msiPCA$image
        } else{
          layout1 <- c(ceiling(length(input$pcaClusters)/2), 2)
          layout2 <- c(2, 3)
          if(prod(layout1) < prod(layout2)){
            par(mfrow = layout1)
          } else{
            par(mfrow = layout2)
          }
          msiPCA$image
        }
      })
    })

    ##(2.5.2) Download PCA image -----------------------------------------------
    output$downloadPCAImage <- downloadHandler(
      filename = function(){paste0("pcaImages", ".pdf")},
      content = function(file){
        pdf(file)
        print(msiPCA$image)
        dev.off()
      }
    )

    #(2.6) Show PCA loading spectrum info --------------------------------------
    output$infoLoadings <- shiny::renderPrint({
      shiny::req(global$cleanedMSIData)
      shiny::req(msiPCA$result)
      cat("Below are the loading plots:\n")
      cat("The loadings of the components show how each mass feature contributes to each component.")
    })

    #(2.7) Display PCA loading spectrum ----------------------------------------
    output$pcaLoadingsSpec <- plotly::renderPlotly({
      shiny::req(msiPCA$result)
      shiny::req(input$pcaClusters != "")
      msiPCA$loading <- plotPCASpec(pcaResult = msiPCA$result,
                                    msiRun = input$msiRunPCA,
                                    clusters = as.numeric(input$pcaClusters)
                                    )
      msiPCA$loading$plot
    })

    #(2.8) Show PCA loading table ----------------------------------------------
    output$pcaLoadingTable <- DT::renderDT(server = FALSE, {
      shiny::req(msiPCA$loading)
      DT::datatable(
        msiPCA$loading$df,
        caption = "PCA loading",
        extensions ="Buttons",
        options = list(dom = 'Bfrtip',
                       buttons = list(list(extend = 'csv', filename= 'pcaLoading')),
                       scrollX = TRUE
                       ),
        rownames = FALSE
      )
    })

    #(3) SSC ===================================================================
    #(3.1) Update MSI run ------------------------------------------------------
    observeEvent(global$processedMSIData, {
      if(is.null(global$cleanedMSIData)){
        global$cleanedMSIData <- global$processedMSIData
      }
      updateSelectInput(session = session,
                        inputId = "msiRunSSC",
                        choices = c(levels(Cardinal::run(global$cleanedMSIData))),
                        selected = c(levels(Cardinal::run(global$cleanedMSIData)))[1]
                        )
    })

    #(3.2) Calculate SSC -------------------------------------------------------
    msiSSC <- reactiveValues(result = NULL, image = NULL, tStatistics = NULL)
    output$infoSSCImage <- shiny::renderPrint({
      shiny::validate(need(global$processedMSIData, message = "MSI data not found!"))
      r <- unique(text2Num(input$r))
      s <- unique(text2Num(input$s))
      k <- unique(text2Num(input$k))
      shiny::validate(
        need(all(r > 0), message = "r should be positive value."),
        need(all(s >= 0), message = "s should be positive value."),
        need(all(k > 0), message = "k should be positive value.")
      )
      observeEvent(input$r,{
        updateSelectInput(session, inputId = 'outputR', choices = r, selected = r[1])
      })
      observeEvent(input$s,{
        updateSelectInput(session, inputId = 'outputS', choices = s, selected = s[1])
      })
      observeEvent(input$k,{
        updateSelectInput(session, inputId = 'outputK', choices = k, selected = k[1])
      })
      set.seed(2023)
      msiSSC$result <- getSSC(msiData = global$cleanedMSIData,
                              r = r,
                              s = s,
                              k = k,
                              method = input$sscMethod,
                              msiRun = input$msiRunSSC
                              #dist = input$sscDist,
                              )
      Cardinal::summary(msiSSC$result)
    }) |>
      bindEvent(input$viewSSC)

    #(3.3) Update SSC clusters -------------------------------------------------
    observe({
      shiny::req(msiSSC$result)
      shiny::req(input$outputR)
      shiny::req(input$outputK)
      shiny::req(input$outputS)
      sscClusterNo <- getSSCClusters(sscResult = msiSSC$result,
                                     r = input$outputR,
                                     k = input$outputK,
                                     s = input$outputS
                                     )
      updateSelectInput(session = session,
                        inputId = "sscClusters",
                        choices = 1:sscClusterNo,
                        selected = 1:sscClusterNo,
                        )
    })

    #(3.3) Plot SSC Images -----------------------------------------------------
    output$sscImages <- shiny::renderPlot({
      shiny::req(global$cleanedMSIData)
      shiny::req(msiSSC$result)
      if(input$modeSSCImage == "light"){
        Cardinal::lightmode()
      } else{
        Cardinal::darkmode()
      }
      msiSSC$image <- plotSSCImage(sscResult = msiSSC$result,
                                   r = input$outputR,
                                   k = input$outputK,
                                   s = input$outputS,
                                   clusters = as.numeric(input$sscClusters),
                                   superpose = as.logical(as.integer(input$superposeSSCImage))
                                   )
      ## Display SSC images
      if(input$superposeSSCImage == "1" | length(input$sscClusters) == 1){
        msiSSC$image
      } else{
        layout1 <- c(ceiling(length(input$sscClusters)/2), 2)
        layout2 <- c(2, 3)
        if(prod(layout1) < prod(layout2)){
          par(mfrow = layout1)
        } else{
          par(mfrow = layout2)
        }
        msiSSC$image
      }
    })

    #(3.4) Download and enlarge SSC images -------------------------------------
    output$downloadSSCImageButton <- renderUI({
      shiny::req(msiSSC$result)
      tagList(
        column(width = 6,
               downloadButton(outputId = ns("downloadSSCImage"),
                              label = "Download Image",
                              icon = icon("download"),
                              style="color: #fff; background-color: #a077b5; border-color: #a077b5"
                              )
               ),
        column(width = 6,
               actionButton(inputId = ns("enlargeSSCButton"),
                            label = "Enlarge Image",
                            icon = icon("search-plus"),
                            style="color: #fff; background-color: #a077b5; border-color: #a077b5"
                            )
               )
      )
    })

    ##(3.5.1) Enlarge SSC image ------------------------------------------------
    observeEvent(input$enlargeSSCButton, {
      showModal(modalDialog(
        tags$head(tags$style(HTML(".modal-dialog { width: 100vw; }"))),
        plotOutput(outputId = ns("enlargedSSCImage"), height = "1000px"),
      ))
      output$enlargedSSCImage <- renderPlot({
        if(input$superposeSSCImage == "1" | length(input$sscClusters) == 1){
          msiSSC$image
        } else{
          layout1 <- c(ceiling(length(input$sscClusters)/2), 2)
          layout2 <- c(2, 3)
          if(prod(layout1) < prod(layout2)){
            par(mfrow = layout1)
          } else{
            par(mfrow = layout2)
          }
          msiSSC$image
        }
      })
    })

    ##(2.5.2) Download SSC image -----------------------------------------------
    output$downloadSSCImage <- downloadHandler(
      filename = function(){paste0("sscImages", ".pdf")},
      content = function(file){
        pdf(file)
        print(msiSSC$image)
        dev.off()
      }
    )

    ##(3.6) Show SSC t-Statistic Spec info ------------------------------------
    output$infoSSCSpec <- shiny::renderPrint({
      shiny::req(global$cleanedMSIData)
      shiny::req(msiSSC$result)
      cat("Below are the SSC t-statistic Spectrum plot:\n")
      cat("t-statistic indicates whether the mass feature is over- or under-expressed in the given cluster relative to the global mean.\n")
      cat("Mass features with t-statistics of zero do not contribute to the segmentation.")
    })

    ##(3.7) Plot SSC t-statistic Spec -----------------------------------------
    output$sscStatisticSpec <- plotly::renderPlotly({
      shiny::req(global$cleanedMSIData)
      shiny::req(msiSSC$result)
      msiSSC$tStatistics <- plotSSCSpec(getSSC = msiSSC$result,
                                        r = input$outputR,
                                        s = input$outputS,
                                        k = input$outputK,
                                        clusters = as.numeric(input$sscClusters),
                                        msiRun = input$msiRunSSC
                                        )
      msiSSC$tStatistics$specPlot
    })

    #(3.8) Show SSC t-statistic table -----------------------------------------
    output$sscStatisticTable <- DT::renderDT(server = FALSE, {
      shiny::req(msiSSC$tStatistics)
      DT::datatable(
        msiSSC$tStatistics$specTable,
        caption = "t-statistics table",
        extensions = "Buttons",
        options = list(dom = 'Bfrtip',
                       buttons = list(list(extend = 'csv', filename= 'tStatisticsTable')),
                       scrollX = TRUE
                       ),
        rownames = FALSE
      )
    })

})}

## To be copied in the UI
# mod_segmentation_ui("segmentation_1")

## To be copied in the server
# mod_segmentation_server("segmentation_1")
