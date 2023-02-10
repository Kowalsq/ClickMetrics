#' @title click_dpi
#' @import graphics
#' @author Felipe de Moraes Kowalski
#' @description Function if the user dont know the dpi.
#' @export

click_dpi <- function (image, plot = TRUE) 
{
  if (isTRUE(interactive())) {
    if (isTRUE(plot)) {
      plot(image)
    }
    message("click in the image")
    coords <- locator(type = "l", n = 2, lwd = 2, col = "pink")
    pix <- sqrt((coords$x[1] - coords$x[2])^2 + (coords$y[1] - 
                                                   coords$y[2])^2)
    return(pix)
  }
}