#' identification UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_identification_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      #(0) User guide ==========================================================
      column(width = 12,
             box(
               width = 12,
               title = strong("User Guide"),
               status = "warning",
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = FALSE,
               closable = FALSE
             )
      ),

      #(1) Optional: upload MSI rds Data =======================================
      column(width = 12, h5("Upload MSI rds Data (Optional)")),
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
      #(1.2) Upload MSI rds data result ----------------------------------------
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

      #(2) Identification ======================================================
      column(width = 12, h6("Metabolite Identification")),
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
               radioButtons(inputId = ns("mzValueType"),
                            label = "1. Choose m/z value",
                            choices = list("enter m/z values" = "singleMZ", "all m/z values" = "allMZ"),
                            selected = "allMZ",
                            inline = TRUE
                            ),
               conditionalPanel(
                 condition = 'input.mzValueType == "singleMZ"',
                 ns = ns,
                 textInput(inputId = ns("mzValues"),
                           label = "Enter m/z values",
                           value = "",
                           placeholder = "For multiple values, use comma to seperate them."
                           )
                 ),
               radioButtons(inputId = ns("ionMode"),
                            label = "2. Choose ion mode",
                            choices = list("positive mode" = "+", "negative mode" = "-"),
                            selected = "",
                            inline = TRUE
                            ),
               sliderInput(inputId = ns("ppm"),
                           label = "3. Choose mass tolerance (ppm)",
                           min = 1,
                           max = 50,
                           value = 5,
                           step = 1
                           ),
               radioButtons(inputId = ns("DB"),
                            label = "4. Choose a database",
                            choices = list("HMDB" = "HMDB", "KEGG" = "kegg"),
                            selected = "HMDB",
                            inline = TRUE
                            ),
               actionButton(inputId = ns("identify"),
                            label = "Go",
                            icon = icon("paper-plane"),
                            style = "color: #fff; background-color: #67ac8e; border-color: #67ac8e"
                            )
             )
      ),
      #(2.2) Metabolite identification result ----------------------------------
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
                 DT::dataTableOutput(outputId = ns("idTable"))
               )
             )
      )




))}

#' identification Server Functions
#'
#' @noRd
mod_identification_server <- function(id, global){
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

    #(2) Identification ========================================================

    output$idTable <- DT::renderDT(server = FALSE, {
      #(1) Get m/z values ------------------------------------------------------
      if(input$mzValueType == "singleMZ"){
        shiny::validate(need(input$mzValues != "", message = "m/z value is missing."))
        mzList <- unique(text2Num(input$mzValues))
      } else {
        shiny::validate(need(global$processedMSIData, message = "MSI data not found."))
        if(is.null(global$cleanedMSIData)){global$cleanedMSIData <- global$processedMSIData}
        mzList <- round(Cardinal::mz(global$cleanedMSIData), 4)
      }
      #(2) Search DB -----------------------------------------------------------
      shiny::validate(
        need(all(mzList > 0), message = "m/z should be positive value."),
        need(input$ionMode != "", message = "Please select ion mode: positive or negative.")
      )
      idResult <- MSbox::what(myMZ = mzList, mode = input$ionMode, ppm = input$ppm, useDB = input$DB)

      #(3) Show result ---------------------------------------------------------
      if(is.null(idResult)){idResult <- data.frame(Result = "Not Found")}
      DT::datatable(
        idResult,
        caption = "DB searching result",
        extensions ="Buttons",
        options = list(dom = 'Bfrtip',
                       buttons = list(list(extend = 'csv', filename= 'identification')),
                       scrollX = TRUE
                       ),
        rownames = FALSE
        )
    }) |>
      bindEvent(input$identify)




})}

## To be copied in the UI
# mod_identification_ui("identification_1")

## To be copied in the server
# mod_identification_server("identification_1")