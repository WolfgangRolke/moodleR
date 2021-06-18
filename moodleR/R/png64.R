#' png64 Function
#' 
#' This function creates a plot object that can be used in a moodle quiz
#' @param plt some graph object
#' @keywords moodle, graph
#' @export
#' @examples
#' plt <- ggplot(mtcars, aes(x=disp, y=mpg))+geom_point()
#' plt64 <- png64(plt)

png64 <- function(plt) {
  require(base64)
  pngfile <- tempfile()
  png(pngfile, width = 400, height = 400)
  print(plt)
  dev.off()
  pltout <- img(pngfile, Rd = TRUE, alt = "a")
  m <- nchar(pltout)
  pltout <- substring(pltout, 6, m-1)
  pltout
}
