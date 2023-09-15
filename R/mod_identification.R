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
               closable = FALSE,
               p("1. Please watch video tutorials about metabolite identification based on accurate mass:",
                 a(href = "https://www.youtube.com/watch?v=vjhJwCZFpN0", shiny::icon("youtube", style = "color:#d17789; font-size:25px;"), target="_blank"))
             )
            ),

      #(1) Optional: upload MSI rds Data =======================================
      mod_readRDS_ui(ns("readRDS_1")),

      #(2) Identification ======================================================
      column(width = 12, h6("Metabolite Identification")),
      mod_dbSearch_ui(ns("dbSearch_1"))

))}

#' identification Server Functions
#'
#' @noRd
mod_identification_server <- function(id, global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #(1) Load MSI rds Data =====================================================
    mod_readRDS_server("readRDS_1", global = global)

    #(2) Identification ========================================================
    mod_dbSearch_server("dbSearch_1", global = global)

})}

## To be copied in the UI
# mod_identification_ui("identification_1")

## To be copied in the server
# mod_identification_server("identification_1")
