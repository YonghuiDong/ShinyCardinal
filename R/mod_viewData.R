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

      #(2.1) Image View ========================================================
      column(width = 4,
             box(
               width = 12,
               inputId = "input_card",
               title = strong("Image View Panel"),
               status = "primary",
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = FALSE,
               closable = FALSE,
               fileInput(inputId = ns("rdsMSI"),
                         label = "(optional) 1. Upload  data:",
                         multiple = TRUE,
                         placeholder = "Pleaae select rds data",
                         accept = c(".rds")
                         ),
               textInput(inputId = ns("mzValues"),
                         label = "Input m/z values to visualize",
                         placeholder = "For multiple m/z values, separate them by a comma..."
                         ),
               sliderInput(inputId = ns("massWindow"),
                           label = "2. Set mass tolerance window (da)",
                           min = 0,
                           max = 1,
                           value = 0.001,
                           step = 0.001
                           ),
               selectInput(inputId = ns("normalizeImage"),
                           label = "3. Apply normalization to the image",
                           multiple = FALSE,
                           choices = list("none" = "none", "linear" = "linear"),
                           selected = "linear"
                           ),
               selectInput(inputId = ns("contrastImage"),
                           label = "4. Apply contrast enhancement to the image",
                           multiple = FALSE,
                           choices = list("none" = "none", "histogram" = "histogram", "suppression" = "suppression"),
                           selected = "suppression"
                           ),
               selectInput(inputId = ns("smoothImage"),
                           label = "5. Apply smoothing to the image",
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
                           selected = "cividis"
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
               title = strong("MSI Images"),
               status = "success",
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = FALSE,
               closable = FALSE,
               shiny::verbatimTextOutput(outputId = ns("processedMSIInfo")),
               shiny::plotOutput(outputId = ns("msiImages"),
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
             )



      )
  )
}

#' viewData Server Functions
#'
#' @noRd
mod_viewData_server <- function(id, global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #(2) Visualize MS images ===================================================
    observeEvent(input$viewImage,{
      #(2.1) Allow users to upload processed MSI data --------------------------
      if(!is.null(input$rdsMSI)){
        global$processedMSIData <- readRDS(input$rdsMSI$datapath)
      }
      shiny::req(!is.null(global$processedMSIData))

      #(2.2) Format m/z values -------------------------------------------------
      mzList <- gsub(" ", "", input$mzValues) |>
        strsplit(x = _, ",", fixed = FALSE) |>
        unlist(x = _) |>
        as.numeric(x = _)

      #(2.3) Plot Images -------------------------------------------------------
      output$processedMSIInfo <- renderPrint({
        shiny::validate(
          need(mzList != "", message = "m/z value is missing"),
          need(min(mzList) >= min(Cardinal::mz(global$processedMSIData)) &
                 max(mzList) <= max(Cardinal::mz(global$processedMSIData)),
               message = paste("m/z valus out of range, it shoud between", min(Cardinal::mz(global$processedMSIData)),
                                "and", max(Cardinal::mz(global$processedMSIData)),sep = " "))
          )
        cat(input$mzValues)
      })

      output$msiImages <- renderPlot({
        shiny::validate(
          need(mzList != "", message = ""),
          need(min(mzList) >= min(Cardinal::mz(global$processedMSIData)) &
                 max(mzList) <= max(Cardinal::mz(global$processedMSIData)),
               message = "")
          )
        if(input$modeImage == "light"){
          Cardinal::lightmode()
        } else {
          Cardinal::darkmode()
        }
        plotImage(msiData = global$processedMSIData,
                  mz = isolate(mzList),
                  smooth.image = input$smoothImage,
                  plusminus = input$massWindow,
                  colorscale = input$colorImage,
                  normalize.image = input$normalizeImage,
                  contrast.enhance = input$contrastImage,
                  superpose = as.logical(as.numeric(input$superposeImage))
                  )
        })

      output$message <- renderPrint({
        cat("You can click over the image to select the pixels of interest")
      })

      #(2.4) Display selected spectrum -----------------------------------------
      output$resetButton <- renderUI({
        actionButton(
          inputId = ns("reset"),
          label = "Reset",
          icon = icon("circle"),
          style="color: #fff; background-color: #a077b5; border-color: #a077b5"
          )
        })
      output$undoButton <- renderUI({
        actionButton(
          inputId = ns("undo"),
          label = "Undo",
          icon = icon("undo"),
          style="color: #fff; background-color: #a077b5; border-color: #a077b5"
          )
        })

      ## click
      rv_click <- reactiveValues(tb = data.frame(x = double(), y = double()))
      observeEvent(input$plot_click, {
        rv_click$tb <-
          isolate(rv_click$tb) |>
          rbind(data.frame(x = as.integer(round(input$plot_click$x, 0)),
                           y = as.integer(round(input$plot_click$y, 0))
                           )
                ) |>
          (\(x) x[!duplicated(x), ])()
        })
      observeEvent(input$undo, {
        rv_click$tb <- head(isolate(rv_click$tb), -1)
        })
      observeEvent(input$reset, {
        rv_click$tb <- data.frame(x = double(), y = double())
        })
      output$info <- renderText({
        print("Please click on the image to select pixels of interest.")
        })
      output$pixelTable <- renderTable({
        rv_click$tb
        })
      output$selectedSpec <- plotly::renderPlotly({
        shiny::req(global$processedMSIData)
        shiny::req(nrow(rv_click$tb) > 0)
        plotPixelSpec(msiData = global$processedMSIData, pixelDF = rv_click$tb)
        })

      })




  })
}

## To be copied in the UI
# mod_viewData_ui("viewData_1")

## To be copied in the server
# mod_viewData_server("viewData_1")



