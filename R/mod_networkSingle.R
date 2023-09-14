#' networkSingle UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_networkSingle_ui <- function(id){
  ns <- NS(id)
  tagList(
    #(1) Input -----------------------------------------------------------------
    column(width = 4,
           box(
             width = 12,
             title = strong("Input Parameters"),
             status = "primary",
             solidHeader = TRUE,
             collapsible = TRUE,
             collapsed = TRUE,
             closable = FALSE,
             p(style = "color:#C70039;", "1. Perform network analysis"),
             selectInput(inputId = ns('msiRunSingle'),
                         label = "(optional) Select a MSI run for network analysis",
                         choices = NULL,
                         selected = NULL
                         ),
             sliderInput(inputId = ns("nthSingle"),
                         label = "(optional) Subset MSI data by using every nth pixels.",
                         min = 1,
                         max = 10,
                         value = 1,
                         step = 1
                         ),
             shiny::numericInput(inputId = ns("singleMZ"),
                                 label = "Enter an m/z value for network analysis.",
                                 value = NULL,
                                 min = 0,
                                 max = 1000000,
                                 step = 0.001
                                 ),
             shiny::actionButton(inputId = ns("colocSingle"),
                                 label = "Go",
                                 icon = icon("paper-plane"),
                                 style = "color: #fff; background-color: #67ac8e; border-color: #67ac8e"
                                 ),
             br(),
             br(),
             p(style = "color:#C70039;", "2. Modify network"),
             sliderInput(inputId = ns("singleLabelSize"),
                         label = "Set label size",
                         min = 10,
                         max = 100,
                         value = 10,
                         step = 5
                         ),
             sliderInput(inputId = ns("pccSingleThreshold"),
                         label = "Set Network threshould",
                         min = 0.5,
                         max = 1,
                         value = 0.9,
                         step = 0.01
                         ),
             hr(),
             p(style = "color:#C70039;", "3. Plot ion images"),
             p(style = "color:#C70039;", shiny::icon("bell"), strong("Note:")),
             p("1. Up to 6 images are displayed."),
             p("2. Download the images to view all of them."),
             sliderInput(inputId = ns("zlim"),
                         label = "Set the range of intensity bar",
                         min = 0,
                         max = 1,
                         value = c(0, 1),
                         step = 0.001
                         ),
             selectInput(inputId = ns("contrastImage"),
                         label = "Select image contrast enhancement method",
                         multiple = FALSE,
                         choices = list("none" = "none", "histogram" = "histogram", "suppression" = "suppression"),
                         selected = "suppression"
                         ),
             selectInput(inputId = ns("smoothImage"),
                         label = "Select image smoothing method",
                         multiple = FALSE,
                         choices = list("none" = "none", "gaussian" = "gaussian", "adaptive" = "adaptive"),
                         selected = "none"
                         ),
             selectInput(inputId = ns("colorImage"),
                         label = "Slect color scale",
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
             actionButton(inputId = ns("plotSingleNetworkImage"),
                          label = "Plot",
                          icon = icon("paper-plane"),
                          style = "color: #fff; background-color: #67ac8e; border-color: #67ac8e"
                          )
            )
    ),

    #(2) Output ----------------------------------------------------------------
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
               shiny::verbatimTextOutput(outputId = ns("singleNetworkInfo"))
             ),
             shiny::uiOutput(outputId = ns("downloadSingleNetworkButton")),
             visNetwork::visNetworkOutput(outputId = ns("showSingleNetwork"), height = "400px"),
             plotly::plotlyOutput(outputId = ns("pseudoMS")),
             br(),
             shinycssloaders::withSpinner(
               image = 'www/img/cardinal.gif',
               shiny::plotOutput(outputId = ns("singleNetworkImage"))
             ),
             br(),
             shiny::uiOutput(outputId = ns("downloadSNImageButton"))
           )
    )

)}

#' networkSingle Server Functions
#'
#' @noRd
mod_networkSingle_server <- function(id, global){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #(1) Update MSI run --------------------------------------------------------
    observeEvent(global$processedMSIData, {
      if(is.null(global$cleanedMSIData)){
        global$cleanedMSIData <- global$processedMSIData
      }
      updateSelectInput(session = session,
                        inputId = "msiRunSingle",
                        choices = levels(Cardinal::run(global$cleanedMSIData)),
                        selected = levels(Cardinal::run(global$cleanedMSIData))[1]
                        )
    })

    #(2) Get network object ----------------------------------------------------
    singleNetwork <- reactiveValues(PCC = NULL, plot = NULL, MZs = NULL, image = NULL)
    output$singleNetworkInfo <- renderPrint({
      shiny::validate(need(!is.null(global$cleanedMSIData), message = "MSI data not found!"))
      mzMin <- round(min(Cardinal::mz(global$cleanedMSIData)), 4)
      mzMax <- round(max(Cardinal::mz(global$cleanedMSIData)), 4)
      shiny::validate(need(input$singleMZ >= mzMin & input$singleMZ <= mzMax,
                           message = paste("m/z value should between", mzMin, "and", mzMax, sep = " "))
                      )
      singleNetwork$PCC <- getPCC(msiData = global$cleanedMSIData,
                                  mz = input$singleMZ,
                                  nth = input$nthSingle,
                                  msiRun = input$msiRunSingle
                                  )
      cat("You can navigate the network using mouse or the zoom and movement buttons below.")
    }) |>
      bindEvent(input$colocSingle)

    #(3) Show network ----------------------------------------------------------
    output$showSingleNetwork <- visNetwork::renderVisNetwork({
      shiny::req(singleNetwork$PCC)
      singleNetwork$plot <- plotSingleNetwork(PCC = singleNetwork$PCC,
                                              mz = input$singleMZ,
                                              threshold = input$pccSingleThreshold,
                                              labelSize = input$singleLabelSize
                                              )
      singleNetwork$plot
    })

    #(4) Download single network -----------------------------------------------
    output$downloadSingleNetworkButton <- renderUI({
      shiny::req(singleNetwork$plot)
      downloadButton(
        outputId = ns("downloadSingleNetwork"),
        label = "Download Network",
        icon = icon("download"),
        style="color: #fff; background-color: #a077b5; border-color: #a077b5"
      )
    })

    output$downloadSingleNetwork <- downloadHandler(
      filename = function(){
        paste0("Network_", input$singleMZ, ".html")
      },
      content = function(file){
        visNetwork::visSave(graph = singleNetwork$plot, file = file)
      }
    )

    #(5) Plot pseudo MS/MS spectrum --------------------------------------------
    output$pseudoMS <- plotly::renderPlotly({
      shiny::req(global$cleanedMSIData)
      shiny::req(singleNetwork$PCC)
      plotMSMS(msiData = global$cleanedMSIData, PCC = singleNetwork$PCC, threshold = input$pccSingleThreshold)
    })

    #(6) Plot single network images --------------------------------------------
    output$singleNetworkImage <- renderPlot({
      shiny::req(global$cleanedMSIData)
      shiny::req(singleNetwork$PCC)
      singleNetwork$MZs <- subset(singleNetwork$PCC, correlation >= input$pccSingleThreshold)$mz
      if(length(singleNetwork$MZs) <= 6){
        snMZs <- singleNetwork$MZs
      } else{
        snMZs <- singleNetwork$MZs[1:6]
      }
      singleNetwork$Image <- plotImage(msiData = global$cleanedMSIData,
                                       mz = snMZs,
                                       smooth.image = input$smoothImage,
                                       colorscale = input$colorImage,
                                       zlim = input$zlim,
                                       colorkey = as.logical(as.numeric(input$showColorkey)),
                                       contrast.enhance = input$contrastImage,
                                       msiRun = input$msiRunSingle
                                       )
      if(input$modeImage == "light"){
        Cardinal::lightmode()
      } else{
        Cardinal::darkmode()
      }
      singleNetwork$Image
    }) |>
      bindEvent(input$plotSingleNetworkImage)

    #(7) Download ion images ---------------------------------------------------
    output$downloadSNImageButton <- renderUI({
      shiny::req(print(singleNetwork$Image))
      downloadButton(outputId = ns("downloadSNImage"),
                     label = "Download Image",
                     icon = icon("download"),
                     style="color: #fff; background-color: #a077b5; border-color: #a077b5"
                     )
    })
    output$downloadSNImage <- downloadHandler(
      filename = function(){paste0(input$singleMZ, "_networkImage.", "pdf")},
      content = function(file){
        if(input$modeImage == "light"){
          Cardinal::lightmode()
        } else{
          Cardinal::darkmode()
        }
        pdf(file, onefile = TRUE)
        withProgress(message = 'Plotting', value = 0, {
          for(i in seq_along(singleNetwork$MZs)){
            print(plotImage(msiData = global$cleanedMSIData,
                            mz = singleNetwork$MZs[i],
                            smooth.image = input$smoothImage,
                            colorscale = input$colorImage,
                            zlim = input$zlim,
                            colorkey = as.logical(as.numeric(input$showColorkey)),
                            contrast.enhance = input$contrastImage,
                            msiRun = input$msiRunSingle
                            )
            )
            incProgress(1/length(singleNetwork$MZs), detail = paste("image", i))
          }
        })
        dev.off()
      }
    )

})}

## To be copied in the UI
# mod_networkSingle_ui("networkSingle_1")

## To be copied in the server
# mod_networkSingle_server("networkSingle_1")
