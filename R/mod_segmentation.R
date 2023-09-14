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
             p("1. Spatial segmentation partitions an MSI data into distinct regions associated with different physiological or pathological status. The MSI dataset is represented
               by a segmentation map, where pixels with similar mass spectra share the same color."),
             p("2. Please watch video tutorials 9 and 10 about image segmentation with PCA and SSC:", a(href = "https://www.youtube.com/@MSI_WIS/videos", shiny::icon("youtube", style = "color:#d17789; font-size:25px;"), target="_blank"))
             )
           ),

    #(1) Optional: upload MSI rds Data =========================================
    mod_readRDS_ui(ns("readRDS_1")),

    #(2) PCA ===================================================================
    column(width = 12, h6("Pricipal Component Analysis (PCA)")),
    mod_PCA_ui(ns("PCA_1")),

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
             p(style = "color:#C70039;", shiny::icon("bell"), strong("Note:")),
             p(style = "color:#C70039;", "1. Use integers for r, s, and k."),
             p(style = "color:#C70039;", "2. Multiple values can be entered for r, s and k."),
             p(style = "color:#C70039;", "3. For multiple values, separate them by a comma."),
             p(style = "color:#C70039;", "4. Multiple values increase computation time."),
             textInput(inputId = ns("r"),
                       label = "1. r: spatial smoothing radii.",
                       value = "2"
                       ),
             textInput(inputId = ns("s"),
                       label = "2. s: shrinkage parameter",
                       value = "0"
                       ),
             textInput(inputId = ns("k"),
                       label = "3. k: maximum number of segments",
                       value = "2"
                       ),
             radioButtons(inputId = ns("sscMethod"),
                          label = "4. Select a method to calculate spatial smoothing weights",
                          choices = list("Gaussian" = "gaussian", "Adaptive" = "adaptive"),
                          selected = "gaussian",
                          inline = TRUE
                          ),
             radioButtons(inputId = ns("sscDist"),
                          label = "5. Select a distance metric type",
                          choices = list("Radial" = "radial", "Manhattan" = "manhattan",
                                         "Minkowski" = "minkowski", "Chebyshev" = "chebyshev"),
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
                          label = "Choose light or dark mode?",
                          choices = list("Light" = "light", "Dark" = "dark"),
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
                    ),
             column(width = 12, hr()),
             p(style = "color:#C70039;", "Send SSC images for ROI analysis"),
             actionButton(inputId = ns("sendSSC2ROI"),
                          label = "Send",
                          icon = icon("paper-plane"),
                          style = "color: #fff; background-color: #67ac8e; border-color: #67ac8e"
                          ),
             br(),
             shiny::verbatimTextOutput(outputId = ns("sendSSCInfo"))
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
    mod_readRDS_server("readRDS_1", global = global)

    #(2) PCA ===================================================================
    mod_PCA_server("PCA_1", global = global)

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
                              msiRun = input$msiRunSSC,
                              dist = input$sscDist
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
      sscClusterNum <- getSSCClusters(sscResult = msiSSC$result,
                                      r = input$outputR,
                                      k = input$outputK,
                                      s = input$outputS
                                      )
      shiny::req(sscClusterNum)
      updateSelectInput(session = session,
                        inputId = "sscClusters",
                        choices = 1:sscClusterNum,
                        selected = 1:sscClusterNum,
                        )
    })

    #(3.4) Plot SSC Images -----------------------------------------------------
    output$sscImages <- shiny::renderPlot({
      shiny::req(global$cleanedMSIData)
      shiny::req(msiSSC$result)
      shiny::validate(need(input$sscClusters != "", message = "At least 1 cluster should be selected."))
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

    #(3.5) Download and enlarge SSC images -------------------------------------
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

    ##(3.5.2) Download SSC image -----------------------------------------------
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
      shiny::req(input$sscClusters != "")
      cat("Note that a miximum of 6 images are displayed above.\n")
      cat("Below are the SSC t-statistic Spectrum plot:\n")
      cat("t-statistic indicates whether the mass feature is over- or under-expressed in the given cluster relative to the global mean.\n")
      cat("Mass features with t-statistics of zero do not contribute to the segmentation.")
    })

    #(3.7) Plot SSC t-statistic Spec -------------------------------------------
    output$sscStatisticSpec <- plotly::renderPlotly({
      shiny::req(global$cleanedMSIData)
      shiny::req(msiSSC$result)
      shiny::req(input$sscClusters != "")
      msiSSC$tStatistics <- plotSSCSpec(getSSC = msiSSC$result,
                                        r = input$outputR,
                                        s = input$outputS,
                                        k = input$outputK,
                                        clusters = as.numeric(input$sscClusters),
                                        msiRun = input$msiRunSSC
                                        )
      msiSSC$tStatistics$specPlot
    })

    #(3.8) Show SSC t-statistic table ------------------------------------------
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

    #(3.9) Send SSC image for ROI analysis -------------------------------------
    output$sendSSCInfo <- renderPrint({
      shiny::validate(need(input$superposeSSCImage == "1", message = "Superpose SSC images first."))
      shiny::req(msiSSC$image)
      global$ionImage <- msiSSC$image
      cat("Done! Go to ROI module for ROI analysis.\n")
    }) |>
      bindEvent(input$sendSSC2ROI)




})}

## To be copied in the UI
# mod_segmentation_ui("segmentation_1")

## To be copied in the server
# mod_segmentation_server("segmentation_1")
