#' export UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_export_ui <- function(id){
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
               p("1. This module enables export of MSI data into data frame (.csv) and centroid imzML files."),
               p("2. Please watch the video tutorial about how to export MSI data:",
                 a(href = "https://www.youtube.com/watch?v=30ZL184f-Ec", shiny::icon("youtube", style = "color:#d17789; font-size:25px;"), target="_blank"))
               )
             ),

      #(1) Optional: upload MSI rds Data =======================================
      mod_readRDS_ui(ns("readRDS_1")),

      #(2) Export result =======================================================
      column(width = 12, h6("Export")),
      mod_exportMSI_ui(ns("exportMSI_1"))

))}

#' export Server Functions
#'
#' @noRd
mod_export_server <- function(id, global){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    #(1) Load MSI rds Data =====================================================
    mod_readRDS_server("readRDS_1", global = global)

    #(2) Export result =========================================================
    mod_exportMSI_server("exportMSI_1", global = global)

})}

## To be copied in the UI
# mod_export_ui("export_1")

## To be copied in the server
# mod_export_server("export_1")
