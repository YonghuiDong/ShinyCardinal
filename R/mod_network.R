#' network UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_network_ui <- function(id){
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
               p("1. Network analysis allows to cluster ions stemming from the same precursor or different ions with similar substructures (i.e., ions sharing the same in-source fragments).
                 These clustered ions provide valuable information on multimers, adducts, natural isotopes, and in-source fragments, which significantly facilitates metabolite identification in MSI experiments."),
               p("2. Please watch video tutorials about network analysis:", a(href = "https://www.youtube.com/watch?v=c49wlOuERN8", shiny::icon("youtube", style = "color:#d17789; font-size:25px;"), target="_blank"))
               )
      ),

      #(1) Optional: upload MSI rds Data =======================================
      mod_readRDS_ui(ns("readRDS_1")),

      #(2) Network Analysis for all features ===================================
      column(width = 12, h6("Network Analysis for All Features")),
      mod_networkAll_ui(ns("networkAll_1")),

      #(3) Network Analysis for single feature =================================
      column(width = 12, h6("Network Analysis for Single Feature")),
      mod_networkSingle_ui(ns("networkSingle_1"))

))}

#' network Server Functions
#'
#' @noRd
mod_network_server <- function(id, global = global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #(1) Load MSI rds Data =====================================================
    mod_readRDS_server("readRDS_1", global = global)

    #(2) Network analysis for all features =====================================
    mod_networkAll_server("networkAll_1", global = global)

    #(3) Network analysis for single feature ===================================
    mod_networkSingle_server("networkSingle_1", global = global)

})}

## To be copied in the UI
# mod_network_ui("network_1")

## To be copied in the server
# mod_network_server("network_1")
