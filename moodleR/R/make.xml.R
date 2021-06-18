#' make.xml
#' 
#' This function is a wrapper for genquiz. It reads file from folder and runs genquiz
#' @param fun name of function that makes a quiz
#' @param k how many quizzes?
#' @param delete.fun should file be deleted from workspace after we are done?
#' @keywords moodle
#' @export
#' @examples
#' make.xml(correlation, 10, problem=1)

make.xml <-
function (fun, k=1, delete.fun=TRUE, ...) 
{
    whichcomp <- strsplit(getwd(),"/")[[1]][3]
    folder <- paste0("c:/users/", whichcomp, "/Dropbox/teaching/moodler/")
    funname <- deparse(substitute(fun))    
    source(paste0(folder, funname, ".R"))
    genquiz(k, fun, folder=folder, ...)
    if(delete.fun)
        remove(list=funname, pos=.GlobalEnv)

}
