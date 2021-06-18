#' qamatrix
#'
#' This function takes a matrix and generates the html code for questions and answers in a
#' moodel quiz 
#' @param tbl a matrix
#' @keywords moodle
#' @export
#' @examples
#' qamatrix(x)


qamatrix <- function(tbl) {
  nr <- nrow(tbl)
  rnames=rownames(tbl)
  nc <- ncol(tbl)
  cnames=colnames(tbl)
  dimnames(tbl)=NULL
  if(!is.null(cnames)) {
    tmp=paste0(cnames, collapse="</th>&nbsp;&nbsp;<th>")
    if(!is.null(rnames)) tmp=paste0("</th>&nbsp;&nbsp;<th>", tmp)
    qtxt = paste0("<tr><th>&nbsp;&nbsp;", tmp, "&nbsp;&nbsp;</th></tr>")   
  }  
  else qtxt=NULL 
  atxt=qtxt  
  for(i in 1:nr) {   
    tmp=ifelse(is.null(rnames), "", paste0("<th>&nbsp;&nbsp;", rnames[i],"&nbsp;&nbsp;</th>"))
    qtmp = paste0(tmp, "<td>&nbsp;&nbsp;", paste0(unlist(lapply(tbl[i, ], nm)), collapse="</td><td>"), "</td>")
    atmp = paste0(tmp, "<td>&nbsp;&nbsp;", paste0(tbl[i, ], collapse="</td>&nbsp;&nbsp;<td>"), "</td>")
    qtxt <- paste0(qtxt, "<tr>", qtmp, "</tr>")        
    atxt <- paste0(atxt, "<tr>", atmp, "</tr>")
  }
  qtxt <- paste0("<p><table Border=1>", qtxt, "</table>")
  atxt <- paste0("<p><table Border=1>", atxt, "</table>")
  list(qtxt=qtxt,atxt=atxt)
}