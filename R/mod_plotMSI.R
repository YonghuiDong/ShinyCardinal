#' plotMSI UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plotMSI_ui <- function(id, inputWidth = 4){
  ns <- NS(id)
  tagList(
    #(1.1) Input ===============================================================
    column(width = inputWidth,
           box(
             width = 12,
             inputId = "input_card",
             title = strong("Input Parameters"),
             status = "primary",
             solidHeader = TRUE,
             collapsible = TRUE,
             collapsed = TRUE,
             closable = FALSE,
             selectInput(inputId = ns("msiRun"),
                         label = "(optional) Select a MSI run to display",
                         choices = NULL,
                         selected = NULL
                         ),
             textInput(inputId = ns("mzValues"),
                       label = "1. Enter m/z values to visualize",
                       placeholder = "For multiple m/z values, separate them by a comma."
                       ),
             strong("2. Set mass tolerance window (Da)"),
             br(),
             br(),
             p(style = "color:#C70039;", shiny::icon("bell"), strong("Note:")),
             p(style = "color:#C70039;", "1. Without mass tolerance, exact m/z is displayed."),
             p(style = "color:#C70039;", "2. Otherwise, entered m/z +/- tolerance is displayed."),
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

    #(1.2) Output ==============================================================
    column(width = (12-inputWidth),
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
           )

)}

#' plotMSI Server Functions
#'
#' @noRd
mod_plotMSI_server <- function(id, msiData, global){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    #(1.0) Update MSI run ======================================================
    observeEvent(msiData(),{
      if(length(levels(Cardinal::run(msiData()))) == 1){
        ## if there is only one run, I don't have to add "All"; It will be easier to record run name for ROI selection.
        updateSelectInput(session = session,
                          inputId = "msiRun", ## no name space
                          choices = levels(Cardinal::run(msiData())),
                          selected = levels(Cardinal::run(msiData()))
                          )
      } else{
        updateSelectInput(session = session,
                          inputId = "msiRun", ## no name space
                          choices = c("All" = "All", levels(Cardinal::run(msiData()))),
                          selected = "All"
                          )
      }
    })

    msiInfo <- reactiveValues(mzList = NULL, mzMin = NULL, mzMax = NULL)

    #(1.1) Show Input m/z Info  ================================================
    output$mzList <- renderPrint({
      shiny::validate(
        need(msiData(), message = "MSI data not found."),
        need(input$mzValues != "", message = "m/z value is missing."),
        need(input$massWindow > 0 | is.na(input$massWindow), message = "mass tolerance should be positive value.")
      )
      msiInfo$mzList <- unique(text2Num(input$mzValues))
      msiInfo$mzMin <- round(min(Cardinal::mz(msiData())), 4)
      msiInfo$mzMax <- round(max(Cardinal::mz(msiData())), 4)
      shiny::validate(need(min(msiInfo$mzList) >= msiInfo$mzMin & max(msiInfo$mzList) <= msiInfo$mzMax,
                           message = paste("m/z value should between", msiInfo$mzMin, "and", msiInfo$mzMax, sep = " ")))
      ## Get ion images
      global$ionImage <- plotImage(msiData = msiData(),
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

    #(1.2) Show MSI images =====================================================
    output$ionImage <- renderPlot({
      shiny::req(global$ionImage)
      if(input$modeImage == "light"){
        Cardinal::lightmode()
      } else{
        Cardinal::darkmode()
      }
      global$ionImage
    })

    #(1.3) Enlarge and Download MSI images =====================================
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

    ##(1.3.1) Enlarge image ----------------------------------------------------
    observeEvent(input$enlargeButton, {
      showModal(modalDialog(
        tags$head(tags$style(HTML(".modal-dialog { width: 100vw; }"))),
        plotOutput(outputId = ns("enlargedImage"), height = "1000px"),
      ))
      output$enlargedImage <- renderPlot({global$ionImage})
    })

    ##(1.3.2) Download image ---------------------------------------------------
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
              print(plotImage(msiData = msiData(),
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

    #(1.4) Display selected  spectrum ==========================================
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
        shiny::req(input$plot_click$x >= min(Cardinal::coord(msiData())$x)
                   & input$plot_click$x <= max(Cardinal::coord(msiData())$x)
                   )
        shiny::req(input$plot_click$y >= min(Cardinal::coord(msiData())$y)
                   & input$plot_click$y <= max(Cardinal::coord(msiData())$y)
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
        shiny::req(msiData())
        plotPixelSpec(msiData = msiData(), pixelDF = rv_click$df)
      })
    })

})}

## To be copied in the UI
# mod_plotMSI_ui("plotMSI_1")

## To be copied in the server
# mod_plotMSI_server("plotMSI_1")
