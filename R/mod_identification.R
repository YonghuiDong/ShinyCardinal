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
                            choices = list("single m/z value" = "singleMZ", "all m/z values" = "allMZ"),
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
                            choices = list("positive mode" = "positive", "negative mode" = "negative"),
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
                            label = "4. Choose database",
                            choices = list("HMDB" = "HMDB", "Lipid MAPS" = "LMSD", "Home DB" = "HDB"),
                            selected = "HMDB",
                            inline = TRUE
                            ),
               conditionalPanel(
                 condition = 'input.DB == "HDB"',
                 ns = ns,
                 fileInput(inputId = ns("HDBFile"),
                           label = "Upload your database (csv format only)",
                           multiple = FALSE,
                           accept = ".csv",
                           placeholder = "DB file should contain a column named mz"
                           )
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
                 shiny::verbatimTextOutput(outputId = ns("idInfo"))
               ),
               DT::dataTableOutput(outputId = ns("idTable"))
             )
      ),




))}

#' identification Server Functions
#'
#' @noRd
mod_identification_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_identification_ui("identification_1")

## To be copied in the server
# mod_identification_server("identification_1")
