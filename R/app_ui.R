#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      dashboardPage(
        ## Header --------------------------------------------------------------
        header = dashboardHeader(title = strong("ShinyCardinal")),
        ## Sidebar -------------------------------------------------------------
        sidebar = dashboardSidebar(
          sidebarMenu(
            id = "sidebarmenu",
            menuItem(text = strong("Home"), tabName = "home", icon = icon("home")),
            hr(),
            menuItem(text = strong("Data Preprocessing"), tabName = "upload", icon = icon("shower")),
            hr(),
            menuItem(text = strong("Image Visualization"), tabName = "visualize", icon = icon("eye")),
            hr(),
            menuItem(text = strong("Image Segmentation"), tabName = "segmentation", icon = icon("object-group")),
            hr(),
            menuItem(text = strong("Network Analysis"), tabName = "network", icon = icon("snowflake")),
            hr(),
            menuItem(text = strong("Identification"), tabName = "identification", icon = icon("info-circle")),
            hr(),
            menuItem(text = strong("Contact"), tabName = "contact", icon = icon("smile"))
          )
        ),

        ## Body ----------------------------------------------------------------
        body = dashboardBody(
          tabItems(
            tabItem(tabName = "home", mod_home_ui("home_1")),
            tabItem(tabName = "upload", mod_uploadData_ui("uploadData_1")),
            tabItem(tabName = "visualize", mod_viewData_ui("viewData_1")),
            tabItem(tabName = "segmentation", mod_segmentation_ui("segmentation_1")),
            tabItem(tabName = "network", mod_network_ui("network_1")),
            tabItem(tabName = "identification", mod_identification_ui("identification_1"))
            #tabItem(tabName = "contact", mod_contact_ui("contact_1"))
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "MSI"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
