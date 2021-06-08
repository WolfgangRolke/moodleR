source("R/routines.R")

shinyServer(function(input, output,session) {
  
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
          command=strsplit(input$C,";")[[1]]
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
})     
