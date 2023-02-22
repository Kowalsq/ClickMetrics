#' @import shiny
#' @import shinydashboard
#' @import imager
#' @import tidyr
#' @import spatstat
#' @import tibble
#' @import purrr
#' @import pracma
#' @author Felipe de Moraes Kowalski
#' @description This function launch a \pkg{shiny} application in browser to
#'     draw a polygon by clicking on points.
#' @title click_poly
#' @export


click_poly <- function(image_path = system.file("example_images", package = "ClickMetrics")){
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
          box(selectInput("IMAGE",
                          "Images:",
                          list.files(path = image_path,
                                     pattern = ".jpg",
                                     full.names = TRUE,
                                     include.dirs = FALSE))),
          numericInput("dpi", "DPI:", 96, min = 1, max = 9999),
          textInput("polygon_name", label = "Polygon name:", value = "polygon 1")
        ),
        
        actionButton("clear", "Clear Points"),
        DTOutput("TABLE")
      )
    ),
    
    server <- function(input, output, session){
      img <- reactive({
        f <- input$IMAGE
        imager::load.image(f)
      })
      
      CLICKS <- reactiveVal(
        value = tibble(
          x = numeric(),
          y = numeric(),
          name = character(),
          area = numeric(),
          proportion = numeric()
        )
      )
      
      ns <- session$ns
      
      shoelace_area <- function(x, y) {
        n <- length(x)
        (sum(x[-n] * y[-1] - x[-1] * y[-n]) / 2)
      }
      
      observeEvent(eventExpr = input$click_plot$x, handlerExpr = {
        add_row(CLICKS(),
                x = isolate(input$click_plot$x),
                y = isolate(input$click_plot$y),
                name = isolate(input$polygon_name)
        ) %>% CLICKS()
        teste <- CLICKS()
        area_pixels <- pracma::polyarea(teste$x, teste$y)
        area_inches <- area_pixels / (input$dpi)^2
        area_cm <- area_inches * (2.54^2)
        teste$area <- abs(area_cm)
        image_area_pixels <- dim(img())[1] * dim(img())[2]
        teste$proportion <- abs(area_pixels / image_area_pixels)
        CLICKS(teste)
      })
      
      
      
      observe({
        if(input$clear>0){
          session$reload()
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
        }
        CLICKS() %>%
          nest(cols = -name) %>%
          deframe() %>%
          map(~ polygon(.x$x, .x$y,
                        col = rgb(1, 0, 0,0.3),
                        lwd = 1.5) +
                text(mean(.x$x), mean(.x$y), paste0(tail(round(CLICKS()$area, 2),1), " cm²"), cex = 1, col = 'white'))
        
      })
      
      output$TABLE <- renderDT(CLICKS(),
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
    }
  )
  runApp(app)
}

###



 
