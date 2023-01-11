#' @title click_length
#' @import shiny
#' @import shinydashboard
#' @import dplyr
#' @import imager
#' @author Felipe de Moraes Kowalski
#' @description This function launch a \pkg{shiny} application in browser to
#'     measure the distance between every pair of clicks. See Details for usage.
#' @export

click_length <- function(image_path = system.file("example_images", package = "ClickMetrics")){
  app <- shiny::shinyApp(
  ui <- fluidPage(
    titlePanel("Click Length"),

    fluidRow(
      plotOutput("IMG",
                 click = "click_plot",
                 "50%",
                 "500px")
    ),

    sidebarPanel(selectInput("IMAGE", "Sample Image:",
                             list.files(path = image_path,
                                        pattern = ".jpg",
                                        full.names = TRUE,
                                        include.dirs = FALSE))),

    fluidRow(verbatimTextOutput("INFO"))

  ),

  server <- function(input, output, session){

    # Creating a reactive value that receives image input

    img <- reactive({
      f <- input$IMAGE
      imager::load.image(f)
    })

    # Store reactive values for coordinates

    CLICKS <- reactiveValues(
      x = NULL,
      y = NULL,
      n = NULL,
      pair = NULL
    )

    observeEvent(eventExpr = input$click_plot$x, handlerExpr = {
      CLICKS$x <- append(CLICKS$x, input$click_plot$x)
      CLICKS$y <- append(CLICKS$y, input$click_plot$y)
      CLICKS$n <- append(CLICKS$n, length(CLICKS$x))
      CLICKS$pair <-
        append(CLICKS$pair,
               as.integer(ceiling(length(CLICKS$x)/2)))
      df <- data.frame(CLICKS$x, CLICKS$y, CLICKS$pair)
      df <- split(df, CLICKS$pair)
      print(df)
    })

    output$IMG <- renderPlot({
      expr = {
        img <- img()
        par(mar = c(0.5, 0.5, 1.75, 0.5))
        plot(img, axes = FALSE)
        box(col = 'gray')
        mtext(text = input$IMAGE,
              side = 3,
              line = 0.5,
              adj = 0.5,
              cex = 1.23)
        if (!is.null(CLICKS$x) && length(CLICKS$x) > 0) {
          points(x = CLICKS$x,
                 y = CLICKS$y,
                 pch = 19,
                 cex = 0.75,
                 col = "red")
          text(x = CLICKS$x,
               y = CLICKS$y,
               label = CLICKS$n,
               pos = 3)
          n_par <- 2 * floor(length(CLICKS$x)/2)
          tb_pairs <- cbind(
            matrix(CLICKS$x[1:n_par], ncol = 2, byrow = TRUE),
            matrix(CLICKS$y[1:n_par], ncol = 2, byrow = TRUE))
          segments(x0 = tb_pairs[, 1],
                   x1 = tb_pairs[, 2],
                   y0 = tb_pairs[, 3],
                   y1 = tb_pairs[, 4],
                   col = "black")
        }
      }
    })

  })

  runApp(app)
}




