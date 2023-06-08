#' contact UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_contact_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      #column(width = 10, includeMarkdown(app_sys("app/www/contact.md")))
      column(width = 10, includeHTML(app_sys("app/www/contact.html")))
    )
  )
}

#' contact Server Functions
#'
#' @noRd
mod_contact_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_contact_ui("contact_1")

## To be copied in the server
# mod_contact_server("contact_1")
