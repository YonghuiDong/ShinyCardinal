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
               textInput(inputId = "mzImage",
                         label = "Input m/z values to visualize",
                         placeholder = "For multiple m/z values, separate them by a comma..."),
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
                                          "magma" = "magma",
                                          "grayscale" = "grayscale"),
                           selected = "cividis"
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
               plotOutput(outputId = ns("msiImages"))
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
    observeEvent(input$viewImage,{
      if(!is.null(input$rdsMSI)){
        global$processedMSIData <- readRDS(input$rdsMSI$datapath)
        }
      output$processedMSIInfo <- renderPrint({
        global$processedMSIData
      })
      # output$msiImages <- renderPlot({
      #   plotImage(msiData = global$processedMSIData,
      #             mz = input$mzImage,
      #             smooth.image = input$smoothImage,
      #             plusminus = input$massWindow,
      #             colorscale = input$colorImage,
      #             normalize.image = input$normalizeImage,
      #             contrast.enhance = input$contrastImage
      #             )
      # })
    })




  })
}

## To be copied in the UI
# mod_viewData_ui("viewData_1")

## To be copied in the server
# mod_viewData_server("viewData_1")
