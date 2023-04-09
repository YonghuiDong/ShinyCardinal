#' statistics UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_statistics_ui <- function(id){
  ns <- NS(id)
  tagList(
    waiter::use_waiter(),
    #(1) User guide ==========================================================
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

    #(2.1) PCA =================================================================
    column(width = 4,
           box(
             width = 12,
             inputId = "input_card",
             title = strong("Pricipal Component Analysis"),
             status = "primary",
             solidHeader = FALSE,
             collapsible = TRUE,
             collapsed = FALSE,
             closable = FALSE,
             fileInput(inputId = ns("rdsMSI"),
                       label = "(optional) 1. Upload rds data:",
                       multiple = TRUE,
                       placeholder = "Pleaae select rds data",
                       accept = c(".rds")
                       ),
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

    #(2.2) Output ==============================================================
    column(width = 8,
           box(
             width = 12,
             inputId = "report_card",
             title = strong("PCA Result"),
             status = "success",
             solidHeader = FALSE,
             collapsible = TRUE,
             collapsed = FALSE,
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
                                 choices = list("Yes" = 1, "NO" = 0),
                                 selected = 0,
                                 inline = TRUE
                                 )
                    ),
             shiny::plotOutput(outputId = ns("pcaImages"))
             # shiny::verbatimTextOutput(outputId = ns("info")),
             # column(width = 6,
             #        shiny::uiOutput(outputId = ns("resetButton"))
             #        ),
             # column(width = 6,
             #        shiny::uiOutput(outputId = ns("undoButton"))
             #        ),
             # shiny::tableOutput(outputId = ns("pixelTable")),
             # plotly::plotlyOutput(outputId = ns("selectedSpec"))
             # )
           ))

  )}

#' statistics Server Functions
#'
#' @noRd
mod_statistics_server <- function(id, global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #(2) PCA ===================================================================
    observeEvent(input$viewPCA,{
      w2 <- waiter::Waiter$new(id = ns("pcaImages"),
                               html = strong("Please wait, running..."),
                               image = 'www/img/cardinal.gif',
                               fadeout = TRUE
                               )
      w2$show()
      #(2.1) Allow users to upload processed MSI data --------------------------
      if(!is.null(input$rdsMSI)){
        global$processedMSIData <- readRDS(input$rdsMSI$datapath)
        }
      shiny::req(!is.null(global$processedMSIData))

      #(2.2) Show PCA images ---------------------------------------------------
      getPCA <- Cardinal::PCA(x = global$processedMSIData,
                              ncomp = input$nComp,
                              center = as.logical(as.numeric(input$centerPCA)),
                              scale = shiny::isolate(as.logical(as.numeric(input$scalePCA))),
                              worker = input$pcaWorkers
                              )
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

      })


    })}

## To be copied in the UI
# mod_statistics_ui("statistics_1")

## To be copied in the server
# mod_statistics_server("statistics_1")
