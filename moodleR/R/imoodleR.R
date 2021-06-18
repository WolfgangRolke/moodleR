#' imoodleR 
#' 
#' This function runs the imoodleR shiny app
#' @keywords moodle, CLOZE
#' @export
#' @examples 
#' shinymoodleR()


imoodleR <-
function (txt, w=100, caps=TRUE, pts=1) 
{
    shiny::runApp(system.file('imoodleR', package='moodleR'))
    
}
