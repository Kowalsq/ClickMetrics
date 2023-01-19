#' @import shiny
#' @import shinydashboard
#' @import imager
#' @import tidyr
#' @import spatstat
#' @import tibble
#' @import purrr
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
                                     include.dirs = FALSE)))
        ),
        
        actionButton("clear", "Clear Points"),
        tableOutput("TABLE"),
        textInput("polygon_name", label = "Polygon name:", value = "polygon 1")
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
        name = character()
        )
      )
      
      observeEvent(eventExpr = input$click_plot$x, handlerExpr = {
        add_row(CLICKS(),
                x = isolate(input$click_plot$x),
                y = isolate(input$click_plot$y),
                name = isolate(input$polygon_name)
                ) %>% CLICKS()
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
          map(~polygon(.x$x, .x$y))
      })
      
      output$TABLE <- renderTable(CLICKS())
    }
  )
  runApp(app)
}


 
