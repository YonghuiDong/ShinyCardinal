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
             selectInput(inputId = ns('msiRun'),
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
             strong("4. (optional) Choose number of workers for parallel computation"),
             sliderInput(inputId = ns("pcaWorkers"),
                         label = "",
                         min = 1,
                         max = 10,
                         value = 1,
                         step = 1
                         ),
             actionButton(inputId = ns("viewPCA"),
                          label = "Plot",
                          icon = icon("paper-plane"),
                          style = "color: #fff; background-color: #67ac8e; border-color: #67ac8e"
                          ),
             br(),
             br(),
             p(style = "color:#C70039;", "Set image parameters:"),
             selectInput(inputId = ns("normalizePCAImage"),
                         label = "Apply normalization to the image",
                         multiple = FALSE,
                         choices = list("none" = "none", "linear" = "linear"),
                         selected = "linear"
                         ),
             selectInput(inputId = ns("contrastPCAImage"),
                         label = "Apply contrast enhancement to the image",
                         multiple = FALSE,
                         choices = list("none" = "none", "histogram" = "histogram", "suppression" = "suppression"),
                         selected = "suppression"
                         ),
             selectInput(inputId = ns("smoothPCAImage"),
                         label = "Apply smoothing to the image",
                         multiple = FALSE,
                         choices = list("none" = "none", "gaussian" = "gaussian", "adaptive" = "adaptive"),
                         selected = "none"
                         ),
             selectInput(inputId = ns("colorPCAImage"),
                         label = "Slect color scale",
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
             radioButtons(inputId = ns("modePCAImage"),
                          label = "Do you prefer light or dark mode?",
                          choices = list("light" = "light", "dark" = "dark"),
                          selected = "dark",
                          inline = TRUE
                          ),
             radioButtons(inputId = ns("superposePCAImage"),
                          label = "Do you want to superpose different m/z images",
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
             shinycssloaders::withSpinner(
               image = 'www/img/cardinal.gif',
               shiny::verbatimTextOutput(outputId = ns("infoPCAImage"))
               ),
             shiny::plotOutput(outputId = ns("pcaImages")),
             shiny::verbatimTextOutput(outputId = ns("infoLoadings")),
             plotly::plotlyOutput(outputId = ns("pcaLoadingsSpec"))
             )
           ),

    #(3) Spatial Shrunken Centroids Clustering =================================
    column(width = 12, h6("Spatial-aware Shrunken Centroids Clustering (SSCC)")),
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
             textInput(inputId = ns("r"),
                       label = "r: The spatial neighborhood radius of nearby pixels to consider.",
                       placeholder = "For multiple values, separate them by a comma.",
                       value = "2"
                       ),
             textInput(inputId = ns("s"),
                       label = "s: Enter the sparsity thresholding parameter by which to shrink the t-statistics",
                       placeholder = "For multiple values, separate them by a comma.",
                       value = "0"
                       ),
             textInput(inputId = ns("k"),
                       label = "k: Enter the maximum number of segments",
                       placeholder = "For multiple values, separate them by a comma.",
                       value = "2"
                       ),
             radioButtons(inputId = ns("ssccMethod"),
                          label = "Choose the method to use to calculate the spatial smoothing weights",
                          choices = list("gaussian" = "gaussian", "adaptive" = "adaptive"),
                          selected = "gaussian",
                          inline = TRUE
                          ),
             radioButtons(inputId = ns("ssccDist"),
                          label = "Choose the type of distance metric",
                          choices = list("radial" = "radial", "manhattan" = "manhattan",
                                         "minkowski" = "minkowski", "chebyshev" = "chebyshev"),
                          selected = "chebyshev",
                          inline = FALSE
                          ),
             strong("5. (optional) Choose number of workers for parallel computation"),
             sliderInput(inputId = ns("ssccWorkers"),
                         label = "",
                         min = 1,
                         max = 10,
                         value = 1,
                         step = 1
                         ),
             actionButton(inputId = ns("viewSSCC"),
                          label = "Plot",
                          icon = icon("paper-plane"),
                          style = "color: #fff; background-color: #67ac8e; border-color: #67ac8e"
                          ),
             br(),
             br(),
             p(style = "color:#C70039;", "Set image parameters:"),
             selectInput(inputId = ns("normalizeSSCCImage"),
                         label = "Apply normalization to the image",
                         multiple = FALSE,
                         choices = list("none" = "none", "linear" = "linear"),
                         selected = "linear"
                         ),
             selectInput(inputId = ns("contrastSSCCImage"),
                         label = "Apply contrast enhancement to the image",
                         multiple = FALSE,
                         choices = list("none" = "none", "histogram" = "histogram", "suppression" = "suppression"),
                         selected = "suppression"
                         ),
             selectInput(inputId = ns("smoothSSCCImage"),
                         label = "Apply smoothing to the image",
                         multiple = FALSE,
                         choices = list("none" = "none", "gaussian" = "gaussian", "adaptive" = "adaptive"),
                         selected = "none"
                         ),
             selectInput(inputId = ns("colorSSCCImage"),
                         label = "Slect color scale",
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

             radioButtons(inputId = ns("modeSSCCImage"),
                          label = "Do you prefer light or dark mode?",
                          choices = list("light" = "light", "dark" = "dark"),
                          selected = "dark",
                          inline = TRUE
                          ),

             radioButtons(inputId = ns("superposeSSCCImage"),
                          label = "Do you want to superpose different m/z images",
                          choices = list("Yes" = 1, "No" = 0),
                          selected = 1,
                          inline = TRUE
                          ),

             p(style = "color:#C70039;", "Subset SSCC images by selecting r, s, and k below:"),
             br(),
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
                    )
             )
           ),

    #(3.2) SSCC Output ----------------------------------------------------------
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
             shinycssloaders::withSpinner(
               image = 'www/img/cardinal.gif',
               shiny::verbatimTextOutput(outputId = ns("infoSSCCImage"))
               ),
             shiny::plotOutput(outputId = ns("ssccImages")),
             shiny::verbatimTextOutput(outputId = ns("infoSSCCSpec")),
             plotly::plotlyOutput(outputId = ns("ssccStatisticSpec"))
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
    #(2.0) Update MSI run ------------------------------------------------------
    observeEvent(global$processedMSIData,{
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
        } else {
        updateSelectInput(session = session,
                          inputId = "msiRun", ## no name space
                          choices = c("All" = "All", levels(Cardinal::run(global$cleanedMSIData))),
                          selected = "All"
                          )
        }
    })

    msiPCA <- reactiveValues(result = NULL, image = NULL)
    #(2.1) Calculate PCA -------------------------------------------------------
    output$infoPCAImage <- shiny::renderPrint({
      shiny::validate(need(global$cleanedMSIData, message = "MSI data not found!"))
      msiPCA$result <- getPCA(msiData = global$cleanedMSIData,
                              ncomp = input$nComp,
                              center = as.logical(as.numeric(input$centerPCA)),
                              scale = as.logical(as.numeric(input$scalePCA)),
                              msiRun = input$msiRun,
                              workers = input$pcaWorkers
                              )
      cat("Below are PCA images:")
    }) |>
      bindEvent(input$viewPCA)

    #(2.2) Display PCA images --------------------------------------------------
    output$pcaImages <- shiny::renderPlot({
      shiny::req(global$cleanedMSIData)
      shiny::req(msiPCA$result)
      if(input$modePCAImage == "light"){
        Cardinal::lightmode()
      } else {
        Cardinal::darkmode()
      }
      msiPCA$image <- Cardinal::image(msiPCA$result,
                                      smooth.image = input$smoothPCAImage,
                                      colorscale = Cardinal::col.map(input$colorPCAImage),
                                      normalize.image = input$normalizePCAImage,
                                      contrast.enhance = input$contrastPCAImage,
                                      superpose = as.logical(as.numeric(input$superposePCAImage))
                                      )
      msiPCA$image
    })

    #(2.3) Show PCA loading spectrum info --------------------------------------
    output$infoLoadings <- shiny::renderPrint({
      shiny::req(global$cleanedMSIData)
      shiny::req(msiPCA$result)
      cat("Below are the loading plots:\n")
      cat("The loadings of the components show how each mass feature contributes to each component.")
    })

    #(2.4) Display PCA loading spectrum ----------------------------------------
    output$pcaLoadingsSpec <- plotly::renderPlotly({
      shiny::req(global$cleanedMSIData)
      shiny::req(msiPCA$result)
      plotPCASpec(msiData = global$cleanedMSIData, pcaResult = msiPCA$result, msiRun = input$msiRun)
    }) |>
      bindEvent(input$viewPCA)

    #(3) SSCC ==================================================================
    msiSSCC <- reactiveValues(result = NULL, image = NULL)
    #(3.1) Calculate SSCC ------------------------------------------------------
    output$infoSSCCImage <- shiny::renderPrint({
      shiny::validate(need(global$processedMSIData, message = "MSI data not found!"))
      if(is.null(global$cleanedMSIData)){global$cleanedMSIData <- global$processedMSIData}
      r <- unique(text2Num(input$r))
      s <- unique(text2Num(input$s))
      k <- unique(text2Num(input$k))

      ## update r, s, k in output
      shiny::validate(
        need(all(r > 0), message = "r should be positive value."),
        need(all(s >= 0), message = "s should be positive value."),
        need(all(k > 0), message = "k should be positive value.")
        )
      observeEvent(input$r,{
        updateSelectInput(session, inputId = 'outputR', choices = r)
      })
      observeEvent(input$s,{
        updateSelectInput(session, inputId = 'outputS', choices = s)
      })
      observeEvent(input$k,{
        updateSelectInput(session, inputId = 'outputK', choices = k)
      })

      set.seed(2023)
      msiSSCC$result <- Cardinal::spatialShrunkenCentroids(x = global$cleanedMSIData,
                                                           r = r,
                                                           s = s,
                                                           k = k,
                                                           method = input$ssccMethod,
                                                           #dist = input$ssccDist,
                                                           BPPARAM = BiocParallel::SnowParam(workers = input$ssccWorkers,
                                                                                             progressbar = FALSE)
                                                           )

      Cardinal::summary(msiSSCC$result)
    }) |>
      bindEvent(input$viewSSCC)

    #(3.3) Plot SSCC Images ----------------------------------------------------
    output$ssccImages <- shiny::renderPlot({
      shiny::req(global$cleanedMSIData)
      shiny::req(msiSSCC$result)
      if(input$modeSSCCImage == "light"){
        Cardinal::lightmode()
      } else {
        Cardinal::darkmode()
      }
      msiSSCC$image <- Cardinal::image(msiSSCC$result,
                                       model = list(r = input$outputR, s = input$outputS, k = input$outputK),
                                       smooth.image = input$smoothSSCCImage,
                                       colorscale = Cardinal::col.map(input$colorSSCCImage),
                                       normalize.image = input$normalizeSSCCImage,
                                       contrast.enhance = input$contrastSSCCImage,
                                       superpose = as.logical(as.numeric(input$superposeSSCCImage))
                                       )
      msiSSCC$image
      })

    ##(3.3) Show SSCC t-Statistic Spec infor -----------------------------------
    output$infoSSCCSpec <- shiny::renderPrint({
      shiny::req(global$cleanedMSIData)
      shiny::req(msiSSCC$result)
      cat("Below are the SSCC t-statistic Spectrum plot:\n")
      cat("Mass features with t-statistics of zero do not contribute to the segmentation.\n")
      cat("t-statistic indicates whether the mass feature is over- or under-expressed in the given cluster relative to the global mean.")
    })

    ##(3.4) Plot SSCC t-statistic Spec -----------------------------------------
    output$ssccStatisticSpec <- plotly::renderPlotly({
      shiny::req(global$cleanedMSIData)
      shiny::req(msiSSCC$result)
      plotSSCCSpec(getSSCC = msiSSCC$result,
                   r = input$outputR,
                   s = input$outputS,
                   k = input$outputK
                   )
    })



  })}

## To be copied in the UI
# mod_segmentation_ui("segmentation_1")

## To be copied in the server
# mod_segmentation_server("segmentation_1")
