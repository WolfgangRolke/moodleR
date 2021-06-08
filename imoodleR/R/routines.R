nm = function (x, w, eps, ndigits, pts = 1) 
{
    n <- length(x)
    if (missing(w)) 
        w <- rep(100, n)
    if (!missing(ndigits)) {
        x <- rep(x, each = 2)
        eps <- rep(10^(-ndigits) * c(0, 0.49), n)
        w <- rep(w, each = 2) * rep(c(1, 0.75), n)
    }
    w <- round(w)
    if (missing(eps)) 
        out <- paste0("{", pts, ":NM:", paste0("%", 
            w, "%", x, "~", collapse = ""))
    else out <- paste0("{", pts, ":NM:", paste0("%", 
        w, "%", x, ":", eps, "~", collapse = ""))
    paste0(substring(out, 1, nchar(out) - 1), "}")
}

mc = function (options, w, which.true, pts = 1) 
{
    option.list <- list(o1 = c("lower", "not equal to", 
        "higher", "can't tell"), o2 = c("lower", 
        "not equal to", "higher"), o3 = c("is statistically significant", 
        "is not statistically significant"), o4 = c("is statistically significant", 
        "is not statistically significant", "can't tell"), 
        o5 = c("is", "is not"), o6 = c("Male", 
            "Female"), o7 = c("true", "false"), 
        o8 = c("has", "does not have"), o9 = c("!=", 
            "<", ">", "can't tell"), o10 = c("!=", 
            "<", ">"), o11 = c("&mu;", "&pi;", 
            "&sigma;", "&lambda;", "&rho;", 
            "other"))
    if (is.numeric(options)) 
        options <- option.list[[options]]
    if (!missing(which.true)) {
        if (is.logical(which.true)) {
            if (which.true) 
                which.true <- 1
            else which.true <- 2
        }
        w <- rep(0, length(options))
        w[which.true] <- 100
    }
    qmc <- paste0("{", pts, ":MC:", paste0("%", 
        w, "%", options, "~", collapse = ""))
    qmc <- paste0(substring(qmc, 1, nchar(qmc) - 1), "}")
    list(qmc = qmc, amc = options[w == 100])
}

genquiz = function (B = 1, fun, folder, problem = 0, Show = FALSE, ...) 
{
    if (missing(folder)) 
        folder <- getwd()
    outfile <- c("<?xml version=\"1.0\" encoding=\"UTF-8\"?>", 
        "<quiz>")
    for (j in problem) {
        for (i in 1:B) {
            if (problem[1] == 0) 
                info <- fun(...)
            else info <- fun(problem = j, ...)
            if (Show) 
                print(info)
            lns <- c("<!-- question: 0   -->", "<question type=\"category\">", 
                "<category>", paste0("<text>$course$/", 
                  info$category, "</text>"), "</category>", 
                "</question>", " ", "<!-- question: ", 
                sample(1:1e+06, 1), "  -->", "<question type=\"cloze\">", 
                "<name>", paste0("<text>", info$quizname, 
                  i, "</text>"), "</name>", "<questiontext format=\"html\">", 
                paste0("<text><![CDATA[", info$qtxt, "]]></text>"), 
                "</questiontext>", "<generalfeedback format=\"html\">", 
                paste0("<text><![CDATA[", info$atxt, "]]></text>"), 
                "</generalfeedback>", "<penalty>0.0000000</penalty>", 
                "<hidden>0</hidden>", "<hint format=\"html\">", 
                paste0("<text><![CDATA[", info$htxt, "]]></text>"), 
                "</hint>", "</question>")
            outfile <- c(outfile, "", lns)
        }
    }
    outfile <- c(outfile, "</quiz>")
    write(outfile, paste0(folder, "/newquiz.xml"))
}

moodle.table = function (x, DoRowNames = FALSE, DoBorder = FALSE, ncols = 10) 
{
    if (is.vector(x)) {
        n <- length(x)
        xlengths <- rep(max(nchar(as.character(x))), ncols)
        k <- n%%ncols
        if (k == 0) 
            x <- matrix(x, ncol = ncols)
        else {
            add <- rep("&nbsp;", floor(n/ncols) * ncols + 
                ncols - n)
            x <- matrix(c(x, add), ncol = ncols, byrow = TRUE)
        }
        padding <- 2
        DoColnames <- FALSE
    }
    else {
        padding <- 8
        DoColnames <- TRUE
        xlengths <- rep(0, dim(x)[2])
        for (i in 1:length(xlengths)) xlengths[i] <- max(nchar(colnames(x)[i]), 
            nchar(as.character(x[, i])))
    }
    n <- dim(x)[1]
    m <- dim(x)[2]
    lns <- ifelse(DoBorder, "<TABLE BORDER>", "<TABLE>")
    nxtline <- "<tr>"
    for (i in 1:m) nxtline <- paste0(nxtline, "<td>", paste0(rep("-", 
        xlengths[i] + padding), collapse = ""), "</td>")
    nxtline <- paste0(nxtline, "</tr>", collapse = "")
    lns <- c(lns, nxtline)
    if (DoColnames) {
        nxtline <- "<TR>"
        if (DoRowNames) 
            nxtline <- paste0(nxtline, "<TD> &nbsp;</TD>")
        for (i in 1:m) nxtline <- paste0(nxtline, paste("<TH>", 
            colnames(x)[i], "</TH>"), collapse = "")
        nxtline <- paste0(nxtline, "</TR>")
        lns <- c(lns, nxtline)
    }
    for (j in 1:n) {
        nxtline <- "<TR>"
        if (DoRowNames) 
            nxtline <- paste0(nxtline, "<TH>", rownames(x)[j], 
                "</TH>")
        for (i in 1:m) nxtline <- paste0(nxtline, "<TD>", 
            x[j, i], "</TD>")
        nxtline <- paste0(nxtline, "</TR>", collapse = "")
        lns <- c(lns, nxtline)
    }
    lns <- c(lns, "</Table>")
    paste0(lns, collapse = "")
}