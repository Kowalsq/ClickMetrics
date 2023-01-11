#' @export
#' @author Felipe de Moraes Kowalski
#' @description This function display an example image.

example_images <- function(n) {
  if ((n<1)|(n>3)) {
    stop("Image not available", call = FALSE)
  }
  images <- c("fig.jpg", "fig2.jpg", "fig3.jpg")
  if(n < 4) {
    return(system.file("example_images", package = "ClickMetrics", images[n]))
  }
}
