#' PCA UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_PCA_ui <- function(id){
  ns <- NS(id)
  tagList(
    #(1) Input -----------------------------------------------------------------
    column(width = 4,
           box(
             width = 12,
             inputId = "input_card",
             title = strong("Input Parameters"),
             status = "primary",
             solidHeader = TRUE,
             collapsible = TRUE,
             collapsed = TRUE,
             closable = FALSE,
             selectInput(inputId = ns('msiRunPCA'),
                         label = '(optional) Select a MSI run to display.',
                         choices = NULL,
                         selected = NULL
                         ),
             sliderInput(inputId = ns("nComp"),
                         label = "1. Select the number of principal components",
                         min = 2,
                         max = 20,
                         value = 2,
                         step = 1
                         ),
             radioButtons(inputId = ns("centerPCA"),
                          label = "2. Should the data be centered?",
                          choices = list("Yes" = 1, "No" = 0),
                          selected = 1,
                          inline = TRUE
                          ),
             radioButtons(inputId = ns("scalePCA"),
                          label = "3. Should the data be scaled?",
                          choices = list("Yes" = 1, "No" = 0),
                          selected = 0,
                          inline = TRUE
                          ),
             actionButton(inputId = ns("viewPCA"),
                          label = "Plot",
                          icon = icon("paper-plane"),
                          style = "color: #fff; background-color: #67ac8e; border-color: #67ac8e"
                          ),
             hr(),
             p(style = "color:#C70039;", "Set image parameters:"),
             selectInput(inputId = ns("pcaClusters"),
                         label = "Select PCA clusters to visualize",
                         choices = NULL,
                         multiple = TRUE
                         ),
             radioButtons(inputId = ns("modePCAImage"),
                          label = "Choose light or dark mode",
                          choices = list("Light" = "light", "Dark" = "dark"),
                          selected = "dark",
                          inline = TRUE
                          ),
             radioButtons(inputId = ns("superposePCAImage"),
                          label = "Should superpose different images?",
                          choices = list("Yes" = 1, "No" = 0),
                          selected = 0,
                          inline = TRUE
                          ),
             hr(),
             p(style = "color:#C70039;", "Send PCA images for ROI analysis"),
             actionButton(inputId = ns("sendPCA2ROI"),
                          label = "Send",
                          icon = icon("paper-plane"),
                          style = "color: #fff; background-color: #67ac8e; border-color: #67ac8e"
                          ),
             br(),
             shiny::verbatimTextOutput(outputId = ns("sendPCAInfo"))

            )
    ),

    #(2) Output ----------------------------------------------------------------
    column(width = 8,
           box(
             width = 12,
             inputId = "report_card",
             title = strong("Result"),
             status = "success",
             solidHeader = TRUE,
             collapsible = TRUE,
             collapsed = TRUE,
             closable = FALSE,
             column(width = 12, shiny::uiOutput(outputId = ns("downloadPCAImageButton"))),
             column(width = 12,
                    shinycssloaders::withSpinner(image = 'www/img/cardinal.gif',
                                                 shiny::verbatimTextOutput(outputId = ns("infoPCAImage"))
                                                 )
                    ),
             column(width = 12, shiny::plotOutput(outputId = ns("pcaImages"))),
             column(width = 12, shiny::verbatimTextOutput(outputId = ns("infoLoadings"))),
             column(width = 12, plotly::plotlyOutput(outputId = ns("pcaLoadingsSpec"))),
             column(width = 12, DT::dataTableOutput(outputId = ns("pcaLoadingTable")))
            )
    )

)}

#' PCA Server Functions
#'
#' @noRd
mod_PCA_server <- function(id, global){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #(1) Update MSI run --------------------------------------------------------
    observeEvent(global$processedMSIData, {
      if(is.null(global$cleanedMSIData)){
        global$cleanedMSIData <- global$processedMSIData
      }
      updateSelectInput(session = session,
                        inputId = "msiRunPCA",
                        choices = c(levels(Cardinal::run(global$cleanedMSIData))),
                        selected = c(levels(Cardinal::run(global$cleanedMSIData)))[1]
                        )
    })

    #(2) Update PCA clusters ---------------------------------------------------
    observeEvent(input$viewPCA, {
      updateSelectInput(session = session,
                        inputId = "pcaClusters",
                        choices = 1:input$nComp,
                        selected = 1:input$nComp,
                        )
    })

    #(3) Calculate PCA ---------------------------------------------------------
    msiPCA <- reactiveValues(result = NULL, image = NULL, loading = NULL)
    output$infoPCAImage <- shiny::renderPrint({
      shiny::validate(need(global$cleanedMSIData, message = "MSI data not found!"))
      set.seed(2023)
      msiPCA$result <- getPCA(msiData = global$cleanedMSIData,
                              ncomp = input$nComp,
                              center = as.logical(as.integer(input$centerPCA)),
                              scale = as.logical(as.integer(input$scalePCA)),
                              msiRun = input$msiRunPCA
                              )
      cat("Below are PCA images:\n")
      cat("Note that a miximum of 6 images are displayed.")
    }) |>
      bindEvent(input$viewPCA)

    #(4) Display PCA images ----------------------------------------------------
    output$pcaImages <- shiny::renderPlot({
      shiny::req(global$cleanedMSIData)
      shiny::req(msiPCA$result)
      shiny::validate(need(input$pcaClusters != "", message = "At least 1 PC should be selected."))
      if(input$modePCAImage == "light"){
        Cardinal::lightmode()
      } else {
        Cardinal::darkmode()
      }
      msiPCA$image <- plotPCAImage(pcaResult = msiPCA$result,
                                   clusters = as.numeric(input$pcaClusters),
                                   superpose = as.logical(as.integer(input$superposePCAImage))
                                   )
      ## Display PCA images
      ## Need par() to display separate images
      if(input$superposePCAImage == "1" | length(input$pcaClusters) == 1){
        msiPCA$image
      } else{
        layout1 <- c(ceiling(length(input$pcaClusters)/2), 2)
        layout2 <- c(2, 3)
        if(prod(layout1) < prod(layout2)){
          par(mfrow = layout1)
        } else{
          par(mfrow = layout2)
        }
        msiPCA$image
      }
    })

    #(5) Download and enlarge PCA images ---------------------------------------
    output$downloadPCAImageButton <- renderUI({
      shiny::req(print(msiPCA$image))
      tagList(
        column(width = 6,
               downloadButton(outputId = ns("downloadPCAImage"),
                              label = "Download Image",
                              icon = icon("download"),
                              style="color: #fff; background-color: #a077b5; border-color: #a077b5"
                              )
               ),
        column(width = 6,
               actionButton(inputId = ns("enlargePCAButton"),
                            label = "Enlarge Image",
                            icon = icon("search-plus"),
                            style="color: #fff; background-color: #a077b5; border-color: #a077b5"
                            )
               )
      )
    })

    ##(5.1) Enlarge image ------------------------------------------------------
    observeEvent(input$enlargePCAButton, {
      showModal(modalDialog(
        tags$head(tags$style(HTML(".modal-dialog { width: 100vw; }"))),
        plotOutput(outputId = ns("enlargedPCAImage"), height = "1000px"),
      ))
      output$enlargedPCAImage <- renderPlot({
        if(input$superposePCAImage == "1" | length(input$pcaClusters) == 1){
          msiPCA$image
        } else{
          layout1 <- c(ceiling(length(input$pcaClusters)/2), 2)
          layout2 <- c(2, 3)
          if(prod(layout1) < prod(layout2)){
            par(mfrow = layout1)
          } else{
            par(mfrow = layout2)
          }
          msiPCA$image
        }
      })
    })

    ##(5.2) Download PCA image -------------------------------------------------
    output$downloadPCAImage <- downloadHandler(
      filename = function(){paste0("pcaImages", ".pdf")},
      content = function(file){
        pdf(file)
        print(msiPCA$image)
        dev.off()
      }
    )

    #(6) Show PCA loading spectrum info ----------------------------------------
    output$infoLoadings <- shiny::renderPrint({
      shiny::req(global$cleanedMSIData)
      shiny::req(msiPCA$result)
      cat("Below are the loading plots:\n")
      cat("The loadings of the components show how each mass feature contributes to each component.")
    })

    #(7) Display PCA loading spectrum ------------------------------------------
    output$pcaLoadingsSpec <- plotly::renderPlotly({
      shiny::req(msiPCA$result)
      shiny::req(input$pcaClusters != "")
      msiPCA$loading <- plotPCASpec(pcaResult = msiPCA$result,
                                    msiRun = input$msiRunPCA,
                                    clusters = as.numeric(input$pcaClusters)
                                    )
      msiPCA$loading$plot
    })

    #(8) Show PCA loading table ------------------------------------------------
    output$pcaLoadingTable <- DT::renderDT(server = FALSE, {
      shiny::req(msiPCA$loading)
      DT::datatable(
        msiPCA$loading$df,
        caption = "PCA loading",
        extensions ="Buttons",
        options = list(dom = 'Bfrtip',
                       buttons = list(list(extend = 'csv', filename= 'pcaLoading')),
                       scrollX = TRUE
                       ),
        rownames = FALSE
      )
    })

    #(9) Send PCA image for ROI analysis ---------------------------------------
    output$sendPCAInfo <- renderPrint({
      shiny::validate(need(input$superposePCAImage == "1", message = "Superpose PCA images first."))
      shiny::req(msiPCA$image)
      global$ionImage <- msiPCA$image
      cat("Done! Go to ROI module for ROI analysis.\n")
    }) |>
      bindEvent(input$sendPCA2ROI)

})}

## To be copied in the UI
# mod_PCA_ui("PCA_1")

## To be copied in the server
# mod_PCA_server("PCA_1")
