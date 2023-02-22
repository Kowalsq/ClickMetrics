#' @title click_length
#' @import shiny
#' @import shinydashboard
#' @import dplyr
#' @import imager
#' @import reactable
#' @import DT
#' @author Felipe de Moraes Kowalski
#' @param image_path insert the path with the images you want to use. Default is a set of example_images
#' @description This function launch a \pkg{shiny} application in browser to
#'     measure the distance between every pair of clicks. See Details for usage.
#' @export

click_length <- function(image_path = system.file("example_images", package = "ClickMetrics")){
  app <- shinyApp(
    ui <- dashboardPage(
      skin = 'purple',
      dashboardHeader(title = "ClickMetrics"),
      dashboardSidebar(disable = TRUE),
      dashboardBody(
        fluidRow(
          box(plotOutput("IMG", 
                         height = 400,
                         click = "click_plot")),
          
          box(
            (selectInput("IMAGE",
                         "Images:",
                         list.files(path = image_path,
                                    pattern = ".jpg",
                                    full.names = TRUE,
                                    include.dirs = FALSE))),
            numericInput("dpi", "DPI:", 96, min = 1, max = 9999)
            
          ),
          
          actionButton("clear","Clear Points"),
          
          DTOutput("INFO")
          
        )
      )
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
        pair = NULL,
        distances = NULL,
        distances_cm = NULL
      )
      
      ns <- session$ns
      
      observeEvent(eventExpr = input$click_plot$x, handlerExpr = { ## Adds the info about clicks
        CLICKS$x <- append(CLICKS$x, input$click_plot$x)
        CLICKS$y <- append(CLICKS$y, input$click_plot$y)
        CLICKS$n <- append(CLICKS$n, length(CLICKS$x))
        CLICKS$pair <-
          append(CLICKS$pair,
                 as.integer(ceiling(length(CLICKS$x)/2)))
        df <- data.frame(CLICKS$x, CLICKS$y, CLICKS$pair)
        df <- split(df, CLICKS$pair)
        
        # Calculate the distances between each pair of clicks
        if (length(CLICKS$x) >= 2) {
          n_par <- 2 * floor(length(CLICKS$x)/2)
          pairs <- cbind(
            matrix(CLICKS$x[1:n_par], ncol = 2, byrow = TRUE),
            matrix(CLICKS$y[1:n_par], ncol = 2, byrow = TRUE))
          distances <- sqrt((pairs[,1] - pairs[,2])^2 + (pairs[,3] - pairs[,4])^2)
          distances_cm <- distances/ input$dpi * 2.54 # the W3C yields that 1cm is equal to 96px / 2.54. 
          #That 96px is the Pixels Per Inch value (default PPI in web)/ 2.54 converts from inches to cm
          CLICKS$distances <- distances
          CLICKS$distances_cm <- distances_cm
        }
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
                     col = "red")
            mid_x <- (tb_pairs[, 1] + tb_pairs[, 2]) / 2
            mid_y <- (tb_pairs[, 3] + tb_pairs[, 4]) / 2
            text(x = mid_x - 1, y = mid_y, label = paste0(round(as.numeric(CLICKS$distances_cm), 2), " cm"), cex = 1.25,
                 col = 'white')
          }
        }
      })
      
      output$INFO <- renderDT({
        df1 <- data.frame(X=round(CLICKS$x,2), Y=round(CLICKS$y,2), Pair=CLICKS$pair, Distance=round(CLICKS$distances_cm,2))
        datatable(df1,
                  extensions = c("Buttons"),
                  
                  options = list(
                    paging = TRUE,
                    searching = TRUE,
                    fixedColumns = TRUE,
                    autoWidth = TRUE,
                    ordering = TRUE,
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'pdf', 'excel')
                  ),
                  class = 'display')
      })
      
      observe({ # clear clicked points
        if(input$clear>0){
          session$reload()
        }
      })
      
    })
  
  runApp(app)
}








