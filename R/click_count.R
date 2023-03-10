#' @import shiny
#' @import shinydashboard
#' @import dplyr
#' @import imager
#' @import reactable
#' @import DT
#' @author Felipe de Moraes Kowalski
#' @description This function launch a \pkg{shiny} application in browser to
#'     count every user click in the image. See section Details for usage.
#' @title click_count
#' @export


click_count <- function(image_path = system.file("example_images", package = "ClickMetrics")){
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
                                     pattern = c(".jpg", ".PNG"),
                                     full.names = TRUE,
                                     include.dirs = FALSE)),
              actionButton("clear", "Clear Points"))
        ),
        
        
        DTOutput("INFO")
      )
    ),

    server <- function(input, output, session){


    # Creating a reactive variable that recognizes the selected image
    img <- reactive({
      f <- input$IMAGE
      imager::load.image(f)
    })
    
    ns <- session$ns

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
        session$reload()
      }
    })

    output$IMG <- renderPlot({
      plot(img(), axes = FALSE)
      n <- numberOfClicks()
      dt <- dest_coords$dt
      names <- dest_coords$names
      text(dest_coords$x, dest_coords$y,names,cex = 1.5 ,col = 'red')
    })

    output$INFO <- renderDT({
      df1 <- data.frame(X = dest_coords$x, Y = dest_coords$y)
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
  })

  runApp(app)

}







