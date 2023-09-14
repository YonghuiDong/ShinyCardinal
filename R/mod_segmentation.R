#' segmentation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_segmentation_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
    #(0) User guide ============================================================
    column(width = 12,
           box(
             width = 12,
             title = strong("User Guide"),
             status = "warning",
             solidHeader = TRUE,
             collapsible = TRUE,
             collapsed = FALSE,
             closable = FALSE,
             p("1. Spatial segmentation partitions an MSI data into distinct regions associated with different physiological or pathological status. The MSI dataset is represented
               by a segmentation map, where pixels with similar mass spectra share the same color."),
             p("2. Please watch video tutorials 9 and 10 about image segmentation with PCA and SSC:", a(href = "https://www.youtube.com/@MSI_WIS/videos", shiny::icon("youtube", style = "color:#d17789; font-size:25px;"), target="_blank"))
             )
           ),

    #(1) Optional: upload MSI rds Data =========================================
    mod_readRDS_ui(ns("readRDS_1")),

    #(2) PCA ===================================================================
    column(width = 12, h6("Pricipal Component Analysis (PCA)")),
    mod_PCA_ui(ns("PCA_1")),

    #(3) Spatial Shrunken Centroids Clustering =================================
    column(width = 12, h6("Spatial-aware Shrunken Centroids Clustering (SSC)")),
    mod_SSC_ui(ns("SSC_1"))

))}


#' segmentation Server Functions
#'
#' @noRd
mod_segmentation_server <- function(id, global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #(1) Load MSI rds Data =====================================================
    mod_readRDS_server("readRDS_1", global = global)

    #(2) PCA ===================================================================
    mod_PCA_server("PCA_1", global = global)

    #(3) SSC ===================================================================
    mod_SSC_server("SSC_1", global = global)

})}

## To be copied in the UI
# mod_segmentation_ui("segmentation_1")

## To be copied in the server
# mod_segmentation_server("segmentation_1")
