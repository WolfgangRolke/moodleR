library(shiny)
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

mcchoices=c("yes , no", 
            "lower ,not equal to , higher , can not say", 
            "lower , not equal to , higher", 
            "is statistically significant , is not statistically significant", 
            "is statistically significant , is not statistically significant , can not say", 
            "is , is not", 
            "Male , Female",
            "true , false",
            "has , does not have",
            "!= , < , >",
            "&mu; , &pi; , &sigma; , &lambda; , &rho; , other")
names(mcchoices)=mcchoices
names(mcchoices)[11]="\u03BC / \u03C0 / \u03C3 / \u03BB / \u03C1 / other"

ui <- fluidPage(
    tags$head(
        tags$style(HTML("
          table, th, td {
              text-align:right;
          }
          
          th, td {
              padding: 10px;
          }    
       "))
    ),
    titlePanel("MoodleR"),
    radioButtons("dtl", "Detailed Explanations", choices=c("No", "Yes"), inline = TRUE),
    fluidRow(
        column(3, textInput("quizname","Name of Quiz / File ",  placeholder = " Quiz 1")),
        column(3, textInput("category","Category", placeholder=" top /  middle / bottom")),
        column(2, textInput("numquiz","Quizzes", value="25", width="50%")),
        column(4, textInput("folder","Folder for Files", placeholder="Working Directory"))
    ),
    conditionalPanel( condition = "input.dtl == 'Yes'",
                      HTML("<h5>Choose the name of the quiz as it will appear in the questions bank. This will also be the name of the .R file</h5>"),                    
                      HTML("<h5>Choose the Category / Subcategory where the quizzes will be stored</h5>"),
                      HTML("<h5>Choose how many quizzes to generate</h5>"),
                      HTML("<h5>Choose where the .R file and the newquiz.xml file are saved</h5>")
    ),
    fluidRow(
        column(3, textInput("n", "Sample Size", placeholder="50,100,1", width="50%"))
    ),
    conditionalPanel( condition = "input.dtl == 'Yes'",
                      HTML("<h5>Choose the desired sample size</h5>"),                    
                      HTML("<h5>To get a randomly chosen sample size enter three number separated by a comma</h5>"),
                      HTML("<h5>From to To in steps of Step</h5>")
    ),  
    fluidRow(
        column(3, selectInput("distribution", "Type of Data", 
                              choices = c("Normal", "Set of Numbers", "Uniform", "Beta", "Gamma", 
                                          "Categorical Variable", "Bivariate Normal", "R Code"))),
        conditionalPanel( condition = "input.distribution == 'Set of Numbers'",     
                          column(2, textInput("Dfrom", "From", placeholder="1,10,1")),
                          column(2, textInput("Dto", "To", placeholder="10,20,1"))
        ),
        conditionalPanel( condition = "input.distribution == 'Uniform'",     
                          column(2, textInput("Ufrom", "From", placeholder="0,1,0.1", width = "100%")),
                          column(2, textInput("Uto", "To", placeholder="1,2,0.1", width = "100%"))
        ),
        conditionalPanel( condition = "input.distribution == 'Normal'",     
                          column(2, textInput("Nmean", "Mean", placeholder="90,110,1", width = "100%")),
                          column(2, textInput("Nstd", "Std", placeholder="9,11,0.1", width = "100%"))
        ),
        conditionalPanel( condition = "input.distribution == 'Beta'",     
                          column(2, textInput("Balpha", "alpha", placeholder="1, 2, 0.1", width = "100%")),
                          column(2, textInput("Bbeta", "beta", placeholder="1, 2, 0.1", width = "100%"))
        ),
        conditionalPanel( condition = "input.distribution == 'Gamma'",     
                          column(2, textInput("Galpha", "alpha",placeholder="1, 2, 0.1", width = "100%")),
                          column(2, textInput("Gbeta", "beta",  placeholder="1, 2, 0.1", width = "100%"))
        ),
        conditionalPanel( condition = "input.distribution == 'Categorical Variable'",     
                          column(2, textInput("Cval", "Values", placeholder="A,B,C")),
                          column(2, textInput("Cp", "Relative Frequencies", placeholder="1, 3, 1"))
        ),
        conditionalPanel( condition = "input.distribution == 'R Code'",     
                          column(12, textAreaInput("RCode", "Code", placeholder="Write your R code, data should be called x", height="200px", width="600px"))
        ),
        conditionalPanel( condition = "input.distribution == 'Bivariate Normal'",        
                          column(2, textInput("BNnames", "Variable Names", placeholder="Age,Height")),
                          column(2, textInput("BNmeans", "Means", placeholder="100, 100", width = "50%")),
                          column(2, textInput("BNstds", "Stds", placeholder="10, 10", width = "50%")),
                          column(2, textInput("BNcor", "Correlation", placeholder="0.5", width = "50%"))
        )
    ),
    conditionalPanel( condition = "input.dtl == 'Yes'",
                      HTML("<h5>Choose the desired parameters</h5>"),                    
                      HTML("<h5>To get randomly chosen values enter three number separated by a comma</h5>")
    ),   
    fluidRow(        
        column(3, selectInput("ndigit", "Round data to ", choices=c(-3:3), selected="0")),
        column(3, selectInput("srt", "Sort data?", choices=c("Yes","No"), selected="No"))
    ),
    conditionalPanel( condition = "input.dtl == 'Yes'",
                      HTML("<h5>Choose the type  and distribution for the data</h5>"),                    
                      HTML("<h5>The option R Code allows entering the code directly, with</h5>"),
                      HTML("<h5>different lines separated by a semicolon</h5>")
    ),  
    radioButtons("numquestions", "Number of Questions ", choices=1:5,inline=TRUE),
    conditionalPanel( condition = "input.dtl == 'Yes'",
                      HTML("<h5>How many questions will the quiz have?</h5>"),                    
    ),  
    HTML("<hr>"),  
    conditionalPanel( condition = "input.numquestions>1", 
                      HTML("<h4>First Question</h4>")
    ),  
    textAreaInput("qtxt1", "", placeholder="Enter question text here. Answerbox will appear at the end, or at @ symbol if entered", width="100%"),
    radioButtons("answertype1", "Type of Answer", choices=c("Numeric Answer", "Multiple Choice"), inline = TRUE),
    conditionalPanel( condition = "input.answertype1 == 'Numeric Answer'",
                      column(3, textInput("numpoints1", "Point(s)", value="100", width = "50%")),
                      column(3, textInput("numprecision1", "Precision(s)", value="0", width = "50%"))
    ),
    conditionalPanel( condition = "input.answertype1 == 'Multiple Choice'",
                      column(3, textInput("mcoptions1", "Choices", placeholder="Yes,No,Maybe")),
                      column(3, selectInput("mcoptions1a", "Choices (List)", choices = mcchoices))
    ),
    conditionalPanel( condition = "input.dtl == 'Yes'",
                      HTML("<h5>Say the R calculation yields 5.1232574, but the students are supposed to round this to 5.12</h5>"),                    
                      HTML("<h5>The unrounded answer should receive 75% of the total points</h5>"),
                      HTML("<h5>Enter 100,75 in Point(s) and 0,0.01 in Precision</h5>")
    ),  
    textAreaInput("calculate.answer1", "Enter Calculation of Answer", placeholder=""),   
    conditionalPanel( condition = "input.dtl == 'Yes'",
                      HTML("<h5>Enter the R code that calculates the answer, for example mean(x)</hr>"),
                      HTML("<h5>Data is always called x</hr>"),
                      HTML("<h5>In the case of a multiple choice question the result of the calculation</hr>"),
                      HTML("<hr>should be the place of the correct option</hr>")
    ),                  
    textAreaInput("atxt1", "", placeholder="Enter answer text here. Answer will appear at the end, or at @ symbol if entered", width="100%"),
    conditionalPanel( condition = "input.numquestions > 1", 
                      HTML("<hr>"),
                      HTML("<h4>Second Question</h4>"),
                      textAreaInput("qtxt2", "", placeholder="Enter question text here. Answerbox will appear at the end, or at @ symbol if entered", width="100%"),
                      radioButtons("answertype2", "Type of Answer", choices=c("Numeric Answer", "Multiple Choice"), inline = TRUE),
                      conditionalPanel( condition = "input.answertype2 == 'Numeric Answer'",
                                        column(3, textInput("numpoints2", "Point(s)", value="100", width = "50%")),
                                        column(3, textInput("numprecision2", "Precision(s)", value="0", width = "50%"))
                      ),
                      conditionalPanel( condition = "input.answertype2 == 'Multiple Choice'",
                                        column(3, textInput("mcoptions2", "Choices", placeholder="Yes,No,Maybe")),
                                        column(3, selectInput("mcoptions2a", "Choices (List)", choices = mcchoices))
                      ),  
                      textAreaInput("calculate.answer2", "Enter Calculation of Answer", placeholder=""),    
                      textAreaInput("atxt2", "", placeholder="Enter answer text here. Answer will appear at the end, or at @ symbol if entered", width="100%"),
    ), 
    conditionalPanel( condition = "input.numquestions > 2", 
                      HTML("<hr>"),
                      HTML("<h4>Third Question</h4>"),
                      textAreaInput("qtxt3", "", placeholder="Enter question text here. Answerbox will appear at the end, or at @ symbol if entered", width="100%"),
                      radioButtons("answertype3", "Type of Answer", choices=c("Numeric Answer", "Multiple Choice"), inline = TRUE),
                      conditionalPanel( condition = "input.answertype3 == 'Numeric Answer'",
                                        column(3, textInput("numpoints3", "Point(s)", value="100", width = "50%")),
                                        column(3, textInput("numprecision3", "Precision(s)", value="0", width = "50%"))
                      ),
                      conditionalPanel( condition = "input.answertype3 == 'Multiple Choice'",
                                        column(3, textInput("mcoptions3", "Choices", placeholder="Yes,No,Maybe")),
                                        column(3, selectInput("mcoptions3a", "Choices (List)", choices = mcchoices))
                      ),  
                      textAreaInput("calculate.answer3", "Enter Calculation of Answer", placeholder=""),    
                      textAreaInput("atxt3", "", placeholder="Enter answer text here. Answer will appear at the end, or at @ symbol if entered", width="100%"),
    ),    
    conditionalPanel( condition = "input.numquestions > 3", 
                      HTML("<hr>"),
                      HTML("<h4>Fourth Question</h4>"),
                      textAreaInput("qtxt4", "", placeholder="Enter question text here. Answerbox will appear at the end, or at @ symbol if entered", width="100%"),
                      radioButtons("answertype4", "Type of Answer", choices=c("Numeric Answer", "Multiple Choice"), inline = TRUE),
                      conditionalPanel( condition = "input.answertype4 == 'Numeric Answer'",
                                        column(3, textInput("numpoints4", "Point(s)", value="100", width = "50%")),
                                        column(3, textInput("numprecision4", "Precision(s)", value="0", width = "50%"))
                      ),
                      conditionalPanel( condition = "input.answertype4 == 'Multiple Choice'",
                                        column(3, textInput("mcoptions4", "Choices", placeholder="Yes,No,Maybe")),
                                        column(3, selectInput("mcoptions4a", "Choices (List)", choices = mcchoices))                
                      ),  
                      textAreaInput("calculate.answer4", "Enter Calculation of Answer", placeholder=""),    
                      textAreaInput("atxt4", "", placeholder="Enter answer text here. Answer will appear at the end, or at @ symbol if entered", width="100%"),
    ),   
    conditionalPanel( condition = "input.numquestions > 4", 
                      HTML("<hr>"),
                      HTML("<h4>Fifth Question</h4>"),
                      textAreaInput("qtxt5", "", placeholder="Enter question text here. Answerbox will appear at the end, or at @ symbol if entered", width="100%"),
                      radioButtons("answertype5", "Type of Answer", choices=c("Numeric Answer", "Multiple Choice"), inline = TRUE),
                      conditionalPanel( condition = "input.answertype5 == 'Numeric Answer'",
                                        column(3, textInput("numpoints5", "Point(s)", value="100", width = "50%")),
                                        column(3, textInput("numprecision5", "Precision(s)", value="0", width = "50%"))
                      ),
                      conditionalPanel( condition = "input.answertype5 == 'Multiple Choice'",
                                        HTML("<h4>separate answers by a comma</h4>"),
                                        column(3, textInput("mcoptions5", "Choices", placeholder="Yes,No,Maybe"))
                      ),  
                      textAreaInput("calculate.answer5", "Enter Calculation of Answer", placeholder=""),    
                      textAreaInput("atxt5", "", placeholder="Enter answer text here. Answer will appear at the end, or at @ symbol if entered", width="100%"),
    ),
    fluidRow(
        column(3, radioButtons("doquiz", "Generate xml file", choices = c("Yes", "No"), inline = TRUE)),
        column(3, actionButton("xmlbutton",HTML("<font color=\"red\">Execute!<font color=\"black\">")))
    ),
    fluidRow(verbatimTextOutput("text"))
)


server <- function(input, output,session) {
    
    get.info=reactive({
        input.values <- reactiveValuesToList(input)
        out=as.list(1:input$numquestions)
        for(i in 1:input$numquestions) {
            if(input.values[[paste0("mcoptions", i)]]=="")
                options=strsplit(input.values[[paste0("mcoptions", i,"a")]],",")[[1]]
            else
                options=strsplit(input.values[[paste0("mcoptions", i)]],",")[[1]]
            out[[i]]=list(qtxt=strsplit(input.values[[paste0("qtxt", i)]],"@")[[1]],
                          atxt=strsplit(input.values[[paste0("atxt", i)]],"@")[[1]],
                          calc.answer=input.values[[paste0("calculate.answer", i)]],
                          answertype=input.values[[paste0("answertype", i)]],
                          options=options,
                          numpoints=strsplit(input.values[[paste0("numpoints", i)]], ",")[[1]],
                          numprecision=strsplit(input.values[[paste0("numprecision", i)]], ",")[[1]]
            )  
            names(out[[i]])=c("qtxt", "atxt", "calc.answers", "answertype",
                              "options", "numpoints", "numprecision")
        }                    
        out                 
    })
    
    par.code=function(a, nme) {
        a = as.numeric(strsplit(a, ",")[[1]])
        if(length(a)==1) ln = paste(nme, "=", a)
        else
            ln = paste0(nme, "=", a[1], "+", a[3], "*", "sample(0:", ((a[2]-a[1])/a[3]), ", 1)") 
        ln
    }
    
    folder = reactive({
        if(input$folder!="") return(input$folder)
        folder=strsplit(getwd(), "/")[[1]]
        paste0(folder[1:(length(folder)-1)],collapse="/")
    })           
    
    gen.R = eventReactive(input$xmlbutton,{ 
        txt = paste0(input$quizname,"=function() {")
        txt[length(txt)+1] = paste0("category=\"",input$category,"\"")
        txt[length(txt)+1] = "quizname=\"problem -\""
        txt[length(txt)+1] = par.code(input$n, "n")
        txt = c(txt, gen.data.commands())
        if(input$srt=="Yes" & input$distribution !="Bivariate Normal") txt[length(txt)+1]="x=sort(x)"      
        if(input$distribution != "Categorical Variable") 
            txt[length(txt)+1]=paste0("x=round(x, ", as.numeric(input$ndigit),")")        
        if(input$numquestions==1)
            txt[length(txt)+1] = paste0("res= ", get.info()[[1]]$calc.answer)    
        else {
            txt[length(txt)+1] = paste0("res=rep(0, ", input$numquestions, ")")  
            for(i in 1:input$numquestions) 
                txt[length(txt)+1] = paste0("res[", i, "]= ", get.info()[[i]]$calc.answer)    
        }     
        txt[length(txt)+1] = paste("qtxt = ", qtxt())
        txt[length(txt)+1] = paste("atxt = ", atxt())        
        txt[length(txt)+1] = "list(qtxt = paste0(\"<h5>\", qtxt, \"</h5>\","
        txt[length(txt)+1] = "    moodle.table(x)),"   
        txt[length(txt)+1] = "    htxt = \"\"," 
        txt[length(txt)+1] = "    atxt = paste0(\"<h5>\", atxt, \"</h5>\")," 
        txt[length(txt)+1] = "    category = category, quizname = quizname)"
        txt[length(txt)+1] = "}"  
        txt[2:(length(txt)-1)]=paste0("   ",txt[2:(length(txt)-1)]) 
        txt              
    })
    
    
    
    gen.data.commands = reactive({
        
        if(input$distribution=="Set of Numbers") {
            command=c(par.code(input$Dfrom, "from"), par.code(input$Dto, "to"), "x=seq(from , to, length=n)")
        }          
        if(input$distribution=="Uniform") {       
            command=c(par.code(input$Ufrom, "from"), par.code(input$Uto, "to"), "x=runif(n, from, to)")
        }    
        if(input$distribution=="Normal") {
            command=c(par.code(input$Nmean, "m"), par.code(input$Nstd, "s"), "x=rnorm(n, m, s)")
        }    
        if(input$distribution=="Beta") {
            command=c(par.code(input$Balpha, "alpha"), par.code(input$Bbeta, "beta"), "x=rbeta(n, alpha, beta)")
        }    
        if(input$distribution=="Gamma") {
            command=c(par.code(input$Galpha, "alpha"), par.code(input$Gbeta, "beta"), "x=rgamma(n, alpha, beta)")
        }    
        if(input$distribution=="Categorical Variable") {
            vals=strsplit(input$Cval,",")[[1]]
            probs=as.numeric(strsplit(input$Cp,",")[[1]])
            command=paste0("x=sample(",vals, ", size=n, replace=TRUE, prob=", probs,")")
        }     
        if(input$distribution=="R Code") {
            command=input$RCode
        }
        if(input$distribution=="Bivariate Normal") {
            library(mvtnorm)
            nms=strsplit(input$BNnames,",")[[1]]
            mu=as.numeric(strsplit(input$BNmeans,",")[[1]])
            s=as.numeric(strsplit(input$BNstds,",")[[1]])
            cr=as.numeric(input$BNcor)   
            ln="library(mvtnorm)"
            ln[2] = paste0("mu=c(", mu[1], ", ", mu[2], ")")
            ln[3] = paste0("sigma=matrix(c(", s[1]^2, ", ", s[1]*s[2]*cr, ", ", s[1]*s[2]*cr, ", ", s[2]^2,"), 2, 2)")
            ln[4] = "x=rmvnorm(n, mu, sigma)"
            ln[5] = paste0("colnames(x) = c(\"", nms[1], "\",\"", nms[2], "\")")
            command=ln
        } 
        command
    })
    
    
    qtxt = eventReactive(input$xmlbutton,{
        starttext=rep("", input$numquestions)
        endtext=rep("", input$numquestions)
        question=rep("", input$numquestions)
        for(i in 1:input$numquestions) {
            starttext[i]= get.info()[[i]]$qtxt[1]
            if(length(get.info()[[i]]$qtxt)>1) endtext[i]=get.info()[[i]]$qtxt[2]
            if(get.info()[[i]]$answertype=="Numeric Answer")   {
                w=get.info()[[i]]$numpoints
                if(length(w)>1) w=paste0("c(", paste0(w, collapse=","),")")
                eps=get.info()[[i]]$numprecision
                if(length(eps)>1) eps=paste0("c(", paste0(eps, collapse=","),")")
                question[i] = paste0("nm(res[", i, "], w = ", w , ", eps = ", eps, ")")
            } 
            if(get.info()[[i]]$answertype=="Multiple Choice")   { 
                m=length(get.info()[[i]]$options)
                options=paste0("\"", get.info()[[i]]$options,"\"")
                options=paste0("c(", paste0(options, collapse=","),")") 
                question[i] = paste0("mc(", options,  ", ifelse(1:", m, "== res[", i, "], 100, 0))$qmc")
            }
        }
        code = paste0(starttext, " \",", question, ",\" ", endtext, collapse="</p><p>")
        code = paste0("\"<p>", code, "</p>\"")
        code = paste("paste0(", code, ")")
    })
    
    atxt = eventReactive(input$xmlbutton,{
        
        starttext=rep("", input$numquestions)
        endtext=rep("", input$numquestions)
        answer=rep("", input$numquestions)
        for(i in 1:input$numquestions) {
            starttext[i]= get.info()[[i]]$atxt[1]  
            if(length(get.info()[[i]]$atxt)>1) endtext[i]=get.info()[[i]]$atxt[2]
            if(get.info()[[i]]$answertype=="Numeric Answer")   {     
                w=strsplit(get.info()[[i]]$numpoints,",")[[1]]
                eps= strsplit(get.info()[[i]]$numprecision,",")[[1]]
                answer[i] = paste0(" res[", i, "]")
            }
            if(get.info()[[i]]$answertype=="Multiple Choice")   { 
                m=length(get.info()[[i]]$options)
                options=paste0("\"", get.info()[[i]]$options,"\"")
                options=paste0("c(", paste0(options, collapse=","),")") 
                answer[i] = paste0( options, "[res[", i, "]]")
            }   
        }
        code = paste0(starttext, " \",", answer, ",\" ", endtext, collapse="</p><p>")
        code = paste0("\"<p>", code, "</p>\"")
        code = paste("paste0(", code, ")")
    })
    
    output$text <- renderPrint({
        txt = gen.R()
        write(txt, paste0(folder(), "/", input$quizname,".R"))
        source(paste0(folder(), "/", input$quizname, ".R"))
        fun=get(input$quizname)
        if(input$doquiz=="Yes")
            genquiz(as.numeric(input$numquiz), fun, folder = folder())     
        fun
    })
}   


# Run the application 
shinyApp(ui = ui, server = server)
