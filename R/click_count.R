#' @import shiny
#' @import shinydashboard
#' @import dplyr
#' @import imager
#' @author Felipe de Moraes Kowalski
#' @description This function launch a \pkg{shiny} application in browser to
#'     count every user click in the image. See section Details for usage.
#' @title click_count
#' @export


click_count <- function(image_path = system.file("example_images", package = "ClickMetrics")){
  app <- shinyApp(
    ui <- fluidPage(

    titlePanel("Click Count"),

    fluidRow(
      plotOutput("IMG", click = "click_plot", "50%", "500px"),
      sidebarPanel(actionButton("clear", "Clear Points"))
    ),

    sidebarPanel(selectInput("IMAGE", "Sample image:",
                             list.files(path = image_path,
                                        pattern = ".jpg",
                                        full.names = TRUE,
                                        include.dirs = FALSE))),

    fluidRow(
      verbatimTextOutput("info")
    )

  ),

    server <- function(input, output, session){


    # Creating a reactive variable that recognizes the selected image
    img <- reactive({
      f <- input$IMAGE
      imager::load.image(f)
    })

    # Creating a spot where i can store reactive values
    initX <- 1
    initY <- 2

    source_coords <- reactiveValues(xy = c(x=initX,y=initY))

    numberOfClicks <- reactiveVal(0)

    # Coords
    dest_coords <- reactiveValues(x=initX, y = initY)
    observeEvent(plot_click(),{
      numberOfClicks(numberOfClicks()+1)
      dest_coords$x <- c(dest_coords$x, floor(plot_click()$x))
      dest_coords$y <- c(dest_coords$y, floor(plot_click()$y))
      dest_coords$dt <- data.frame(dest_coords$x, dest_coords$y)
      dest_coords$names <- as.numeric(row.names(dest_coords$dt))-1
    })

    plot_click <- debounce(reactive(input$click_plot), 300)

    observe({
      if(input$clear > 0){
        dest_coords$names <- NULL
      }
    })

    output$IMG <- renderPlot({
      plot(img(), axes = FALSE)
      n <- numberOfClicks()
      dt <- dest_coords$dt
      names <- dest_coords$names
      text(dest_coords$x, dest_coords$y,names,cex = 2 ,col = 'red')
    })

    output$info <- renderPrint({
      req(input$click_plot)
      x <- round(input$click_plot$x,2)
      y <- round(input$click_plot$y,2)
      cat("[", x, ", ", y, "]", sep = "")
    })
  })

  runApp(app)

}






