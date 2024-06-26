#' dbSearch UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dbSearch_ui <- function(id){
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
             radioButtons(inputId = ns("mzValueType"),
                          label = "1. Choose m/z value for identification",
                          choices = list("Enter m/z values" = "singleMZ", "All m/z values" = "allMZ"),
                          selected = "allMZ",
                          inline = TRUE
                          ),
             conditionalPanel(
               condition = 'input.mzValueType == "singleMZ"',
               ns = ns,
               textInput(inputId = ns("mzValues"),
                         label = "Enter m/z values",
                         value = "",
                         placeholder = "For multiple values, use comma to seperate them"
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
               DT::dataTableOutput(outputId = ns("idTable"))
             )
           )
    )

)}

#' dbSearch Server Functions
#'
#' @noRd
mod_dbSearch_server <- function(id, global){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

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
# mod_dbSearch_ui("dbSearch_1")

## To be copied in the server
# mod_dbSearch_server("dbSearch_1")
