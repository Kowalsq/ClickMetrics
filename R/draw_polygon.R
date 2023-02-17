#' @import shiny
#' @import shinydashboard
#' @import dplyr
#' @import imager
#' @import leaflet
#' @import spatstat
#' @import sf
#' @author Felipe de Moraes Kowalski
#' @description This function launch a \pkg{shiny} application in browser to
#'     make possible drawing a polygon above a map.
#' @title draw_polygon
#' @export


draw_polygon <- function () {
  head <- dashboardHeader(disable = TRUE)
  sidebar <- dashboardSidebar(disable = TRUE)
  body <- dashboardBody(fluidRow(box(width = 8, leafletOutput("map",
                                                              height = 400)), box(width = 4, textInput("file_name",
                                                                                                       label = "File Name", value = "polygons.rds"),
                                                                                  actionButton("make", "Make"), actionButton("clear",
                                                                                                                             "Clear"), actionButton("save", "Save"),
                                                                                  actionButton("load", "Load"))))
  ui <- dashboardPage(head, sidebar, body)
  server <- function(input, output) {
    rv <- reactiveValues(clicks = data.frame(lng = numeric(),
                                             lat = numeric()), objects = list())
    output$map <- {
      renderLeaflet({
        leaflet() %>% addTiles() %>% setView(lat = -25.428189920084876,
                                             lng = -49.26790053715797, zoom = 5)
      })
    }
    observeEvent(input$map_click, {
      lastest.click <- data.frame(lng = input$map_click$lng,
                                  lat = input$map_click$lat)
      rv$clicks <- rbind(rv$clicks, lastest.click)
      leafletProxy("map") %>% addCircles(data = rv$clicks,
                                         lng = ~lng, lat = ~lat, radius = 2, color = "black",
                                         opacity = 1, layerId = "circles") %>% addPolylines(data = rv$clicks,
                                                                                            lng = ~lng, lat = ~lat, weight = 2, dashArray = 3,
                                                                                            color = "black", opacity = 1, layerId = "lines")
    })
    observeEvent(input$make, {
      if (nrow(rv$clicks) > 0) {
        rv$clicks <- rbind(rv$clicks, rv$clicks[1, ])
        new.polygon <- rv$clicks %>% as.matrix %>% list %>%
          st_polygon
        rv$objects[[length(rv$objects) + 1]] <- new.polygon
        rv$clicks <- data.frame(lng = numeric(), lat = numeric())
        leafletProxy("map") %>% removeShape("circles") %>%
          removeShape("lines") %>% addPolygons(data = new.polygon %>%
                                                 st_sfc, weight = 1, color = "black",
                                               fillColor = "black", fillOpacity = 0.5)
        t <- st_sfc(new.polygon, crs = 4326)
        area <- st_area(t)
        print(paste0("Polygon area: ", round((area/1000000), 2), " sqkm"))
      }
    })
    observeEvent(input$clear, {
      rv$clicks <- data.frame(lng = numeric(), lat = numeric())
      rv$objects <- list()
      leafletProxy("map") %>% clearShapes()
    })
    observeEvent(input$save, {
      base_crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
      rv$objects %>% st_sfc(crs = base_crs) %>% saveRDS(file = input$file_name)
      save.file.message <- paste("polygons are saved at: ",
                                 getwd(), "/", input$file_name, sep = "")
      print(save.file.message)
    })
    observeEvent(input$load, {
      rv$objects <- readRDS(input$file_name) %>% st_sfc
      rv$clicks <- data.frame(lng = numeric(), lat = numeric())
      leafletProxy("map") %>% clearShapes() %>% addPolygons(data = rv$objects %>%
                                                              st_sfc, weight = 1, color = "black", fillColor = "black",
                                                            fillOpacity = 0.5)
    })
  }
  shinyApp(ui, server)
}




