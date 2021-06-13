source("R/routines.R")
library(shinyMatrix)

shinyServer(function(input, output, session) {
  
   save.inputs=eventReactive(input$xmlbutton,{
      input.values <- reactiveValuesToList(input)
      dump("input.values", paste0(folder(),"/",input$quizname,".dta"))
      
   })
   
   read.inputs=reactive({
        files=dir(folder())[endsWith(dir(folder()),".dta")]
        if( !(paste0(input$quizname,".dta") %in% files) ) return(NULL)
        source(paste0(folder(),"/",input$quizname,".dta"))
        for(i in 1:length(input.values))
          updateTextInput(session, names(input.values)[i],  value = input.values[[i]])
   })
   

   
   get.info=reactive({
     input.values <- reactiveValuesToList(input)
     input.values$question1newline="No"
     out=as.list(1:input$numquestions)
     for(i in 1:input$numquestions) {
        if(input.values[[paste0("mcoptions", i)]]=="")
           options=strsplit(input.values[[paste0("mcoptions", i,"a")]],",")[[1]]
        else
           options=strsplit(input.values[[paste0("mcoptions", i)]],",")[[1]]
        if(input.values[[paste0("atxt",i)]]=="")    
           input.values[[paste0("atxt",i)]]=input.values[[paste0("qtxt",i)]]
        out[[i]]=list(qtxt=strsplit(input.values[[paste0("qtxt", i)]],"@")[[1]],
                   atxt=strsplit(input.values[[paste0("atxt", i)]],"@")[[1]],
                   calc.answer=input.values[[paste0("calculate.answer", i)]],
                   answertype=input.values[[paste0("answertype", i)]],
                   questionnewline=input.values[[paste0("question", i,"newline")]],
                   options=options,
                   numpoints=strsplit(input.values[[paste0("numpoints", i)]], ",")[[1]],
                   numprecision=strsplit(input.values[[paste0("numprecision", i)]], ",")[[1]]
                 )  
         names(out[[i]])=c("qtxt", "atxt", "calc.answers", "answertype",  "questionnewline", 
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
        txt[length(txt)+1] = paste0("res=as.list(1:", input$numquestions, ")")  
        for(i in 1:input$numquestions) 
             txt[length(txt)+1] = paste0("res[[", i, "]]= ", get.info()[[i]]$calc.answer)         
        txt[length(txt)+1] = paste("qtxt = ", qtxt())
        txt[length(txt)+1] = paste("atxt = ", atxt())     
        txt[length(txt)+1] = paste("htxt = \"", input$htxt,"\"")        
        txt[length(txt)+1] = "list(qtxt = paste0(\"<h5>\", qtxt, \"</h5>\","
        txt[length(txt)+1] = "    moodle.table(x)),"   
        txt[length(txt)+1] = "    htxt = paste0(\"<h5>\", htxt, \"</h5>\")," 
        txt[length(txt)+1] = "    atxt = paste0(\"<h5>\", atxt, \"</h5>\")," 
        txt[length(txt)+1] = "    category = category, quizname = quizname)"
        txt[length(txt)+1] = "}"  
        txt[2:(length(txt)-1)]=paste0("   ",txt[2:(length(txt)-1)]) 
        txt              
   })
   

        
   gen.data.commands = reactive({
       
         
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
           cnames = strsplit(input$pc,",")[[1]]
           rnames = strsplit(input$pr,",")[[1]]
           if(length(rnames)<2) {
             vals=paste0("c(\"", paste0(cnames, collapse="\",\""), "\")")           
             probs=as.numeric(strsplit(input$catpvector,",")[[1]])
             probs=paste0("c(", paste0(probs, collapse=","), ")")
             command=paste0("x=sample(",vals, ", size=n, replace=TRUE, prob=", probs,")")
           }
           else {
             varnames=strsplit(input$cat2varnames,",")[[1]]
             ln1 = paste0("p=matrix(c(", paste0(input$catpmatrix,collapse=","), "),", length(rnames), ",", length(cnames),")")  
             cnames=paste0("c(\"", paste0(cnames, collapse="\",\""), "\")")           
             rnames=paste0("c(\"", paste0(rnames, collapse="\",\""), "\")")           
             ln2 = paste0("dimnames(p)=list(", rnames,",", cnames,")")
             ln3=paste0("x=rcategorical(n, p)")
             ln4=paste0("colnames(x)=", paste0("c(\"", paste0(varnames, collapse="\",\""), "\")") )
             command=c(ln1, ln2, ln3, ln4)
           }
           command  
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
            if(get.info()[[i]]$questionnewline=="Yes") endtext[i]=paste0(endtext[i],"</p><p>")
            if(get.info()[[i]]$answertype=="Numeric Answer")   {
                w=get.info()[[i]]$numpoints
                if(length(w)>1) w=paste0("c(", paste0(w, collapse=","),")")
                eps=get.info()[[i]]$numprecision
                if(length(eps)>1) eps=paste0("c(", paste0(eps, collapse=","),")")
                question[i] = paste0("nm(res[[", i, "]], w = ", w , ", eps = ", eps, ")")
            } 
            if(get.info()[[i]]$answertype=="Multiple Choice")   { 
              m=length(get.info()[[i]]$options)
              options=paste0("\"", get.info()[[i]]$options,"\"")
              options=paste0("c(", paste0(options, collapse=","),")") 
              question[i] = paste0("mc(", options,  ", ifelse(1:", m, "== res[[", i, "]], 100, 0))$qmc")
            }
            if(get.info()[[i]]$answertype=="Numeric Matrix") {
               question[i] = paste0("qamatrix(res[[", i, "]])$qtxt")
            }
            if(get.info()[[i]]$answertype=="Plain Text")   {     
              question[i] = paste0(" res[[", i, "]]")
            }                       
        }
        code = paste0(starttext, " \",", question, ",\" ", endtext, collapse="")
        code = paste0("\"<p>", code, "</p>\"")
        code = paste("paste0(", code, ")")   
        code
   })

   atxt = eventReactive(input$xmlbutton,{
        starttext=rep("", input$numquestions)
        endtext=rep("", input$numquestions)
        answer=rep("", input$numquestions)
        for(i in 1:input$numquestions) {
            starttext[i]= get.info()[[i]]$atxt[1]  
            if(length(get.info()[[i]]$atxt)>1) endtext[i]=get.info()[[i]]$atxt[2]
            if(get.info()[[i]]$questionnewline=="Yes") endtext[i]=paste0(endtext[i],"</p><p>")
            if(get.info()[[i]]$answertype=="Numeric Answer")   
                answer[i] = paste0(" res[[", i, "]]")
           if(get.info()[[i]]$answertype=="Numeric Matrix") 
                answer[i] = paste0("qamatrix(res[[", i, "]])$atxt")
           if(get.info()[[i]]$answertype %in% c("Multiple Choice", "Plain Text") ) { 
                m=length(get.info()[[i]]$options)
                options=paste0("\"", get.info()[[i]]$options,"\"")
                answer[i] = paste0( options, "[res[[", i, "]]]")
            }   
        }
        code = paste0(starttext, " \",", answer, ",\" ", endtext, collapse="")
        code = paste0("\"<p>", code, "</p>\"")
        code = paste("paste0(", code, ")")
        code
   })
   
   output$catvarinfo=renderUI({
     if(input$distribution != 'Categorical Variable') return(NULL)
     cnames = strsplit(input$pc,",")[[1]]
     rnames = strsplit(input$pr,",")[[1]]
     if(length(rnames)<2) {
        out=textInput("catpvector", "Probabilities", placeholder="1,2,3")
     }  
     else {    
       A=matrix(0, length(rnames), length(cnames))
       dimnames(A)=list(rnames,cnames)
       out=matrixInput("catpmatrix", "Categorical Variables and Probabilities", A)
     }
     out  
   })   
    
   output$text <- renderPrint({
        read.inputs()
        txt = gen.R()
        save.inputs()
        write(txt, paste0(folder(), "/", input$quizname,".R"))
        source(paste0(folder(), "/", input$quizname, ".R"))
        fun=get(input$quizname)
        if(input$doquiz=="Yes")
           genquiz(as.numeric(input$numquiz), fun, folder = folder())     
        fun
   })
})     
