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
            menuItem(text = strong("Load Data"), tabName = "upload", icon = icon("book")),
            hr(),
            menuItem(text = strong("Visualize Data"), tabName = "visualize", icon = icon("check-square")),
            hr(),
            menuItem(text = strong("Statistics"), tabName = "statistics", icon = icon("check-square")),
            hr(),
            menuItem(text = strong("Contact"), tabName = "contact", icon = icon("smile"))
          )
        ),

        ## Body ----------------------------------------------------------------
        body = dashboardBody(
          tabItems(
            #tabItem(tabName = "home",  mod_home_ui("home_1")),
            tabItem(tabName = "upload",  mod_uploadData_ui("uploadData_1")),
            tabItem(tabName = "visualize",  mod_viewData_ui("viewData_1")),
            tabItem(tabName = "statistics",  mod_statistics_ui("statistics_1"))
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
