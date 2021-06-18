#' nm
#'
#' This function generates the code for a numerical CLOZE question
#' @param x  vector of values
#' @param w list of weights
#' @param ndigits answers have to be rounded to ndigits, otherwise gives partial credit. Overrides eps
#' @param eps vector of precision
#' @param pts how many points is question worth?
#' @keywords moodle
#' @export
#' @examples
#' nm(50)
#' nm(c(50, 40), w=c(100, 50))
#' nm(c(50, 40), w=c(100, 50), ndigits=0) #for 100% answer has to be rounded to nearest integer

nm <-
  function(x, w, eps, ndigits, pts = 1) {
    n <- length(x)
    if(missing(w)) w <- rep(100, n)    
    if(!missing(ndigits)) {
       x <- rep(x, each=2)
       eps <- rep(10^(-ndigits)*c(0, 0.49), n)
       w <- rep(w, each=2)*rep(c(1, 0.75),n)
    }
    w <- round(w)
    if(missing(eps))       
      out <- paste0("{", pts, ":NM:", paste0("%", w, "%", x, "~", collapse = ""))
    else
      out <- paste0("{", pts, ":NM:", paste0("%", w, "%", x, ":", eps, "~", collapse = ""))
    
    paste0(substring(out, 1, nchar(out)-1), "}")
  }
