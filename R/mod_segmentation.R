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
    waiter::use_waiter(),
    #(0) User guide ============================================================
    column(width = 12,
           box(
             width = 12,
             title = strong("User Guide"),
             status = "warning",
             solidHeader = FALSE,
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
             shiny::verbatimTextOutput(outputId = ns("infoMSIData"))
             )
           ),

    #(2) Background noise and matrix removal ===================================
    column(width = 12, h5("Remove Background Noises and Matrix Peaks")),
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
             p(style = "color:#C70039;", "2. Enter a noise or matrix m/z value below. The software
               will detect its colocalized features and remove them based on the colocalization coefficient."),
             p(style = "color:#C70039;", "3. You can improve the speed by subsetting the MSI data
               and selecting multiple workers."),
             p(style = "color:#C70039;", "4. You can run this step multiple times to efficiently remove noise-
               and matrix-related features."),
             numericInput(inputId = ns("noisePeak"),
                          label = "Enter a single noise or matrix m/z value",
                          value = NULL,
                          min = 0,
                          max = 10000000
                          ),
             sliderInput(inputId = ns("colocThreshould"),
                         label = "Select the threshold for colocalization correlation coefficient",
                         min = 0.5,
                         max = 1,
                         value = 0.9,
                         step = 0.01
                         ),
             sliderInput(inputId = ns("nth"),
                         label = "(Optional) Subset MSI Data by selecting every nth pixel",
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

    ##(2.1) Background noise and matrix removal output -------------------------
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
             ## tagAppendAttributes is used to display waiter image in textOutput
             tagAppendAttributes(shiny::verbatimTextOutput(outputId = ns("infoBNMR")), style = "height:400px;"),
             column(width = 6, shiny::uiOutput(outputId = ns("deleteButton"))),
             column(width = 6, shiny::uiOutput(outputId = ns("resetButton"))),
             shiny::verbatimTextOutput(outputId = ns("summaryBNMR")),
             shiny::verbatimTextOutput(outputId = ns("massList2"))
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
             sliderInput(inputId = ns("nComp"),
                         label = "2. Select the number of principal components",
                         min = 2,
                         max = 20,
                         value = 2,
                         step = 1
                         ),
             radioButtons(inputId = ns("centerPCA"),
                          label = "3. Should the data be centered?",
                          choices = list("Yes" = 1, "No" = 0),
                          selected = 1,
                          inline = TRUE
                          ),
             radioButtons(inputId = ns("scalePCA"),
                          label = "4. Should the data be scaled?",
                          choices = list("Yes" = 1, "No" = 0),
                          selected = 0,
                          inline = TRUE
                          ),
             strong("5. (optional) Choose number of workers for parallel computation"),
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
             column(width = 6,
                    selectInput(inputId = ns("normalizePCAImage"),
                                label = "Apply normalization to the image",
                                multiple = FALSE,
                                choices = list("none" = "none", "linear" = "linear"),
                                selected = "linear"
                                )
                    ),
             column(width = 6,
                    selectInput(inputId = ns("contrastPCAImage"),
                                label = "Apply contrast enhancement to the image",
                                multiple = FALSE,
                                choices = list("none" = "none", "histogram" = "histogram", "suppression" = "suppression"),
                                selected = "suppression"
                                )
                    ),
             column(width = 6,
                    selectInput(inputId = ns("smoothPCAImage"),
                                label = "Apply smoothing to the image",
                                multiple = FALSE,
                                choices = list("none" = "none", "gaussian" = "gaussian", "adaptive" = "adaptive"),
                                selected = "none"
                                )
                    ),
             column(width = 6,
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
                                selected = "cividis"
                                )
                    ),
             column(width = 6,
                    radioButtons(inputId = ns("modePCAImage"),
                                 label = "Do you prefer light or dark mode?",
                                 choices = list("light" = "light", "dark" = "dark"),
                                 selected = "dark",
                                 inline = TRUE
                                 )
                    ),
             column(width = 6,
                    radioButtons(inputId = ns("superposePCAImage"),
                                 label = "Do you want to superpose different m/z images",
                                 choices = list("Yes" = 1, "No" = 0),
                                 selected = 0,
                                 inline = TRUE
                                 )
                    ),
             shiny::verbatimTextOutput(outputId = ns("infoPCAImage")),
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
                       placeholder = "For multiple values, separate them by a comma...",
                       value = "2"
                       ),
             textInput(inputId = ns("s"),
                       label = "s: Enter the sparsity thresholding parameter by which to shrink the t-statistics",
                       placeholder = "For multiple values, separate them by a comma...",
                       value = "0"
                       ),
             textInput(inputId = ns("k"),
                       label = "k: Enter the maximum number of segments",
                       placeholder = "For multiple values, separate them by a comma...",
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
             column(width = 6,
                    selectInput(inputId = ns("normalizeSSCCImage"),
                                label = "Apply normalization to the image",
                                multiple = FALSE,
                                choices = list("none" = "none", "linear" = "linear"),
                                selected = "linear"
                                )
                    ),
             column(width = 6,
                    selectInput(inputId = ns("contrastSSCCImage"),
                                label = "Apply contrast enhancement to the image",
                                multiple = FALSE,
                                choices = list("none" = "none", "histogram" = "histogram", "suppression" = "suppression"),
                                selected = "suppression"
                                )
                    ),
             column(width = 6,
                    selectInput(inputId = ns("smoothSSCCImage"),
                                label = "Apply smoothing to the image",
                                multiple = FALSE,
                                choices = list("none" = "none", "gaussian" = "gaussian", "adaptive" = "adaptive"),
                                selected = "none"
                                )
                    ),
             column(width = 6,
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
                                selected = "cividis"
                                )
                    ),
             column(width = 6,
                    radioButtons(inputId = ns("modeSSCCImage"),
                                 label = "Do you prefer light or dark mode?",
                                 choices = list("light" = "light", "dark" = "dark"),
                                 selected = "dark",
                                 inline = TRUE
                                 )
                    ),
             column(width = 6,
                    radioButtons(inputId = ns("superposeSSCCImage"),
                                 label = "Do you want to superpose different m/z images",
                                 choices = list("Yes" = 1, "No" = 0),
                                 selected = 1,
                                 inline = TRUE
                                 )
                    ),
             column(width = 12,
                    h4("Subset SSCC images by selecting r, s, and k below:"),
                    br()
                    ),
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
             shiny::verbatimTextOutput(outputId = ns("infoSSCCImage")),
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
    observeEvent(input$loadData, {
      if(!is.null(input$rdsMSI)){
        global$processedMSIData <- readRDS(input$rdsMSI$datapath)
      }

      #(1.1) Show MSI data info ------------------------------------------------
      output$infoMSIData <- shiny::renderPrint({
        shiny::validate(need(!is.null(global$processedMSIData), message = "MSI data not found"))
        cat("MSI data loaded successfully!\n")
        global$processedMSIData
      })
    })

    #(2) Background Removal ====================================================
    ## massList is used to ensure that user will not perform coloclaizaiton on the same m/z more than once.
    ## This may remove some unwanted mass features.
    massList <- reactiveValues(value = NULL)
    observeEvent(input$noiseColoc,{
      #(2.1) Check input -------------------------------------------------------
      shiny::req(global$processedMSIData)
      shiny::req(input$noisePeak)
      shiny::req(input$noisePeak >= min(Cardinal::mz(global$processedMSIData)) &
                   input$noisePeak <= max(Cardinal::mz(global$processedMSIData))
                 )
      shiny::req(!(isolate(input$noisePeak) %in% isolate(massList$value)))
      w <- waiter::Waiter$new(id = ns("infoBNMR"),
                              html = "Please wait, running...",
                              image = 'www/img/cardinal.gif',
                              fadeout = TRUE
                              )
      w$show()

      #(2.2) Perform colocalization --------------------------------------------
      if(is.null(global$cleanedMSIData)){
        global$cleanedMSIData <- global$processedMSIData
      }
      colocDF <- colocAnalysis(msiData = global$cleanedMSIData,
                               precursor = input$noisePeak,
                               nth = input$nth,
                               worker = input$colocWorkers
                               )
      global$subDF <- colocDF[colocDF$correlation >= input$colocThreshould, c("mz", "correlation")]
      on.exit({w$hide()})

      #(2.3) Display buttons ---------------------------------------------------
      output$deleteButton <- renderUI({
        actionButton(
          inputId = ns("deleteFeatures"),
          label = "Delete",
          icon = icon("trash"),
          style="color: #fff; background-color: #a077b5; border-color: #a077b5"
          )
        })
      output$resetButton <- renderUI({
        actionButton(
          inputId = ns("resetFeatures"),
          label = "Reset",
          icon = icon("circle"),
          style="color: #fff; background-color: #a077b5; border-color: #a077b5"
          )
        })

      #(2.4) Display cocolization information ----------------------------------
      output$infoBNMR <- shiny::renderPrint({
        shiny::validate(
          need(!is.null(global$processedMSIData), message = "MSI data not found."),
          need(!is.null(input$noisePeak), message = "Input m/z peak not found."),
          need(!(isolate(input$noisePeak) %in% isolate(massList$value)),
               message = "This input feature has been removed, Please choose another one."),
          need(nrow(colocDF) > 0, message = "Input m/z peak is out of range.")
          )
        cat("The selected features are shown below:\n")
        cat("You can click on the Reset button to restore the original MSI data. \n")
        cat("\n")
        global$subDF
        })
      })

    #(2.5) Delete features -----------------------------------------------------
    observeEvent(input$deleteFeatures,{
      global$cleanedMSIData <- removeNoise(msiData = global$cleanedMSIData, subDF = global$subDF)
      ## the input noise peak is not exactly the same as in the data, so I need to record it as well.
      massList$value <- round(c(massList$value, input$noisePeak, global$subDF$mz), 3)
    })

      #(2.7) Reset feature -----------------------------------------------------
    observeEvent(input$resetFeatures,{
      shiny::req(global$cleanedMSIData)
      shiny::req(global$processedMSIData)
      global$cleanedMSIData <- global$processedMSIData
      massList$value <- NULL
    })

      #(2.8) Show summarized result --------------------------------------------
    output$summaryBNMR <- shiny::renderPrint({
      shiny::req(global$cleanedMSIData)
      shiny::req(global$processedMSIData)
      if(identical(global$processedMSIData, global$cleanedMSIData)){
        cat("No noises or matrix related peaks were removed.\n")
      } else{
        cat("MSI data after noises and matrix related peaks removal: \n")
        global$cleanedMSIData
      }
    })

    output$massList2 <- shiny::renderPrint({
      print(input$noisePeak %in% massList$value)
      massList$value
    })

    #(3) PCA ===================================================================
    observeEvent(input$viewPCA,{

      #(3.1) Check input -------------------------------------------------------
      shiny::req(global$processedMSIData)
      if(is.null(global$cleanedMSIData)){
        global$cleanedMSIData <- global$processedMSIData
        }
      w2 <- waiter::Waiter$new(id = ns("pcaImages"),
                               html = strong(""),
                               image = 'www/img/cardinal.gif',
                               fadeout = TRUE
                               )
      w2$show()

      #(3.2) Show PCA images ---------------------------------------------------
      getPCA <- Cardinal::PCA(x = global$cleanedMSIData,
                              ncomp = input$nComp,
                              center = as.logical(as.numeric(input$centerPCA)),
                              scale = shiny::isolate(as.logical(as.numeric(input$scalePCA))),
                              BPPARAM = BiocParallel::SnowParam(workers = input$pcaWorkers, progressbar = T)
                              )
      output$infoPCAImage <- shiny::renderPrint({
        cat("Below are PCA images:")
      })
      output$pcaImages <- shiny::renderPlot({
        on.exit({w2$hide()})
        if(input$modePCAImage == "light"){
          Cardinal::lightmode()
        } else {
          Cardinal::darkmode()
        }
        Cardinal::image(getPCA,
                        smooth.image = input$smoothPCAImage,
                        colorscale = Cardinal::col.map(input$colorPCAImage),
                        normalize.image = input$normalizePCAImage,
                        contrast.enhance = input$contrastPCAImage,
                        superpose = as.logical(as.numeric(input$superposePCAImage))
                        )
        })

      #(3.3) Show PCA loading spectrum info ------------------------------------
      output$infoLoadings <- shiny::renderPrint({
        cat("Below are the loading plots:\n")
        cat("The loadings of the components show how each mass feature contributes to each component.")
      })

      #(3.4) Show PCA loading spectrum -----------------------------------------
      output$pcaLoadingsSpec <- plotly::renderPlotly({
        shiny::req(
          !is.null(global$cleanedMSIData),
          !is.null(getPCA)
        )
        plotPCASpec(msiData = global$cleanedMSIData, pcaResult = getPCA)
        })
      })

    #(4) SSCC ==================================================================
    observeEvent(input$viewSSCC,{
      shiny::req(global$processedMSIData)
      if(is.null(global$cleanedMSIData)){
        global$cleanedMSIData <- global$processedMSIData
      }
      w3 <- waiter::Waiter$new(id = ns("ssccImages"),
                               html = strong(""),
                               image = 'www/img/cardinal.gif',
                               fadeout = TRUE
                               )
      w3$show()

      #(4.1) Format input parameters -------------------------------------------
      r <- unique(text2Num(input$r))
      s <- unique(text2Num(input$s))
      k <- unique(text2Num(input$k))
      observeEvent(input$r,{
        updateSelectInput(session, inputId = 'outputR', choices = r)
      })
      observeEvent(input$s,{
        updateSelectInput(session, inputId = 'outputS', choices = s)
      })
      observeEvent(input$k,{
        updateSelectInput(session, inputId = 'outputK', choices = k)
      })

      #(3.2) Show SSCC images --------------------------------------------------
      set.seed(2023)
      getSSCC <- Cardinal::spatialShrunkenCentroids(x = global$cleanedMSIData,
                                                    r = r,
                                                    s = s,
                                                    k = k,
                                                    method = input$ssccMethod,
                                                    #dist = input$ssccDist,
                                                    BPPARAM = BiocParallel::SnowParam(workers = input$ssccWorkers, progressbar = T)
                                                    )
      output$infoSSCCImage <- shiny::renderPrint({
        Cardinal::summary(getSSCC)
      })

      output$ssccImages <- shiny::renderPlot({
        on.exit({w3$hide()})
        if(input$modeSSCCImage == "light"){
          Cardinal::lightmode()
        } else {
          Cardinal::darkmode()
        }
        Cardinal::image(getSSCC,
                        model = list(r = input$outputR, s = input$outputS, k = input$outputK),
                        smooth.image = input$smoothSSCCImage,
                        colorscale = Cardinal::col.map(input$colorSSCCImage),
                        normalize.image = input$normalizeSSCCImage,
                        contrast.enhance = input$contrastSSCCImage,
                        superpose = as.logical(as.numeric(input$superposeSSCCImage))
                        )
      })

      ##(3.3) Show SSCC t-Statistic Spec infor ---------------------------------
      output$infoSSCCSpec <- shiny::renderPrint({
        cat("Below are the SSCC t-statistic Spectrum plot:\n")
        cat("Mass features with t-statistics of zero do not contribute to the segmentation.\n")
        cat("t-statistic indicates whether the mass feature is over- or under-expressed in the given cluster relative to the global mean.")
        })

      ##(3.4) Plot SSCC t-statistic Spec ---------------------------------------
      output$ssccStatisticSpec <- plotly::renderPlotly({
        shiny::req(getSSCC)
        plotSSCCSpec(getSSCC = getSSCC,
                     r = input$outputR,
                     s = input$outputS,
                     k = input$outputK
                     )
        })



      })




    })}

## To be copied in the UI
# mod_segmentation_ui("segmentation_1")

## To be copied in the server
# mod_segmentation_server("segmentation_1")
