#' @import shiny
#' @import shinydashboard
#' @import dplyr
#' @import imager
#' @import ggplot2
#' @author Felipe de Moraes Kowalski
#' @description This function launch a \pkg{shiny} application in browser to
#'     detect the hex code and the RGBA of the clicked pixel on the image using javascript.
#' @title color_picker
#' @export

color_picker <- function(image_path = system.file("example_images", package = "ClickMetrics")){
  js <- '
$(document).ready(function(){
  var canvas = document.createElement("canvas");
  var getPixelColor = setInterval(function(){
    var $img = $("#ggplot>img");
    if($img.length){
      clearInterval(getPixelColor);
      var img = $img[0];
      canvas.width = img.width;
      canvas.height = img.height;
      canvas.getContext("2d").drawImage(img, 0, 0, img.width, img.height);
      $img.on("click", function(e){
        var pixelData = canvas.getContext("2d").
          getImageData(e.offsetX, e.offsetY, 1, 1).data;
        Shiny.setInputValue("color", Array.prototype.slice.call(pixelData));
      })
    }
  }, 100);
})
'
  app <- shinyApp(
    ui <- fluidPage(
      tags$head(
        tags$script(HTML(js))
      ),
      br(),
      fluidRow(
        column(
          width = 9,
          plotOutput("ggplot",
                     click = "click_plot"),
          selectInput("IMAGE",
                      "Images:",
                      list.files(path = image_path,
                                 pattern = ".jpg",
                                 full.names = TRUE,
                                 include.dirs = FALSE))
        ),
        column(
          width = 3,
          h3("pixel RGBA:"),
          verbatimTextOutput("pixelRGBA"),
          br(),
          h3("pixel color:"),
          verbatimTextOutput("pixelColor")
        )
      )
    ),
    
    server <- function(input, output, session){
      
      img <- reactive({
        f <- input$IMAGE
        imager::load.image(f)
      })
      
      output[["ggplot"]] <- renderPlot({
        img <- img()
        plot(img, axes = FALSE)
        box(col = 'grey')
        
      })
      
      output[["pixelRGBA"]] <- renderPrint({
        input[["color"]]
      })
      
      pixelColor <- reactive({
        req(input[["color"]])
        rgba <- input[["color"]]
        rgb(rgba[1], rgba[2], rgba[3], rgba[4], maxColorValue = 255)
      })
      
      output[["pixelColor"]] <- renderPrint({
        pixelColor()
      })
      
    })
  
  runApp(app)
}



