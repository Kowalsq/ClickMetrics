#' @title get_dpi
#' @import graphics
#' @author Felipe de Moraes Kowalski
#' @description Function if the user dont know the dpi.
#' @export

get_dpi <- function (image, plot = TRUE) 
{
  if (isTRUE(interactive())) {
    pix <- click_dpi(image, plot = plot)
    known <- as.numeric(readline("real distance in cm: "))
    pix/(known/2.54)
  }
}


