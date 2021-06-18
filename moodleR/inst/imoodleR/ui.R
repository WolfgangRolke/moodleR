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
            "<,>", "+,-", 
            "&mu; , &pi; , &sigma; , &lambda; , &rho; , other")
names(mcchoices)=mcchoices
names(mcchoices)[11]="\u03BC / \u03C0 / \u03C3 / \u03BB / \u03C1 / other"
Distributions=c("Normal", "Uniform", "Beta", "Gamma", "Categorical Variable", "Bivariate Normal", "R Code", "No Data")

shinyUI(fluidPage(
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
  textAreaInput("comments", "Comments (Optional)"),
  conditionalPanel( condition = "input.dtl == 'Yes'",
    HTML("<h5>Choose the name of the quiz as it will appear in the questions bank. This will also be the name of the .R file</h5>"),                    
    HTML("<h5>Choose the Category / Subcategory where the quizzes will be stored</h5>"),
    HTML("<h5>Choose how many quizzes to generate</h5>"),
    HTML("<h5>Choose where the .R file and the newquiz.xml file are saved</h5>")
  ),
  fluidRow(
    column(3, textInput("n", "Sample Size", value="50,100,1", width="50%")),
    column(5, selectInput("distribution", "Type of Data", choices = Distributions))
  ),
  conditionalPanel( condition = "input.dtl == 'Yes'",
     HTML("<h5>Choose the desired sample size</h5>"),                    
     HTML("<h5>To get a randomly chosen sample size enter three number separated by a comma</h5>"),
     HTML("<h5>From to To in steps of Step</h5>")
  ),  
  fluidRow(
     conditionalPanel( condition = "input.distribution == 'Uniform'",     
        column(2, textInput("Ufrom", "From", value="1", width = "100%")),
        column(2, textInput("Uto", "To", value="10", width = "100%"))
     ),
     conditionalPanel( condition = "input.distribution == 'Normal'",     
        column(2, textInput("Nmean", "Mean", value="90,110,1", width = "100%")),
        column(2, textInput("Nstd", "Std", value="1", width = "100%"))
     ),
     conditionalPanel( condition = "input.distribution == 'Beta'",     
        column(2, textInput("Balpha", "alpha", value="1", width = "100%")),
        column(2, textInput("Bbeta", "beta", value="1", width = "100%"))
     ),
     conditionalPanel( condition = "input.distribution == 'Gamma'",     
        column(2, textInput("Galpha", "alpha", value="1", width = "100%")),
        column(2, textInput("Gbeta", "beta", value="1", width = "100%"))
     ),
     conditionalPanel( condition = "input.distribution == 'Categorical Variable'",
        column(4, textInput("pc","Values First Variable", value = "Male, Female")),
        column(4, textInput("pr","Values Second Variable (Optional)", placeholder = "Young, Middle Age, Old")),
        conditionalPanel( condition = "input.pr!=''",
            column(4, textInput("cat2varnames", "Names of Variables", placeholder="X,Y"))
       )                  
     ),
     conditionalPanel( condition = "input.distribution == 'R Code'",     
        column(12, textAreaInput("RCode", "Code", placeholder="Write your R code, data should be called x", height="200px", width="600px"))
     ),
     conditionalPanel( condition = "input.distribution == 'Bivariate Normal'",    
        column(2, textInput("BNmeans", "Means", value="0, 0", width = "50%")),
        column(2, textInput("BNstds", "Stds", value="1, 1", width = "50%")),
        column(2, textInput("BNcor", "Correlation", value="0.5", width = "50%")),
        column(4, textInput("BNnames", "Variable Names", value="X,Y"))
     ),
     column(6,uiOutput("catvarinfo"))
   ),
   conditionalPanel( condition = "input.dtl == 'Yes'",
     HTML("<h5>Choose the desired parameters</h5>"),                    
     HTML("<h5>To get randomly chosen values enter three number separated by a comma</h5>")
   ),   
   conditionalPanel( condition = "input.distribution != 'Categorical Variable'", 
     conditionalPanel( condition = "input.distribution != 'Bivariate Normal'",           
       conditionalPanel( condition = "input.distribution != 'R Code'",           
         fluidRow(          
           column(5, selectInput("ndigit", "Round data to ... digits behind decimal", choices=c(-3:3), selected="-1")),
           column(3, selectInput("srt", "Sort data?", choices=c("Yes","No"), selected="No"))
         )  
       )  
     )
  ),
  conditionalPanel( condition = "input.dtl == 'Yes'",
     HTML("<h5>Choose the type  and distribution for the data</h5>"),                    
     HTML("<h5>The option R Code allows entering the code directly, with</h5>"),
     HTML("<h5>different lines separated by a semicolon</h5>")
  ),  
  HTML("<hr>"),
  radioButtons("numquestions", "Number of Questions ", choices=1:7,inline=TRUE),
  conditionalPanel( condition = "input.dtl == 'Yes'",
        HTML("<h5>How many questions will the quiz have?</h5>"),                    
  ),  
      HTML("<hr>"),  
      conditionalPanel( condition = "input.numquestions>1", 
        HTML("<h4>First Question</h4>")
      ),  
      textAreaInput("qtxt1", "", placeholder="Enter question text here. Answerbox will appear at the end, or at @ symbol if entered", width="100%"),
      radioButtons("answertype1", "Type of Answer", choices=c("Numeric Answer", "Multiple Choice", "Numeric Matrix", "Verbatim"), inline = TRUE),
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
  textAreaInput("atxt1", "", placeholder="Enter answer text here", width="100%"),
  conditionalPanel( condition = "input.numquestions > 1", 
      HTML("<hr>"),
      HTML("<h4>Second Question</h4>"),
      radioButtons("question2newline", "Start on new Line", choices=c("Yes", "No"), inline=TRUE),
      textAreaInput("qtxt2", "", placeholder="Enter question text here. Answerbox will appear at the end, or at @ symbol if entered", width="100%"),
      radioButtons("answertype2", "Type of Answer", choices=c("Numeric Answer", "Multiple Choice", "Numeric Matrix", "Verbatim"), inline = TRUE),
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
      radioButtons("question3newline", "Start on new Line", choices=c("Yes", "No"), inline=TRUE),
      textAreaInput("qtxt3", "", placeholder=" ", width="100%"),
      radioButtons("answertype3", "Type of Answer", choices=c("Numeric Answer", "Multiple Choice", "Numeric Matrix", "Verbatim"), inline = TRUE),
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
      radioButtons("question4newline", "Start on new Line", choices=c("Yes", "No"), inline=TRUE),
      textAreaInput("qtxt4", "", placeholder=" ", width="100%"),
      radioButtons("answertype4", "Type of Answer", choices=c("Numeric Answer", "Multiple Choice", "Numeric Matrix", "Verbatim"), inline = TRUE),
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
        radioButtons("question5newline", "Start on new Line", choices=c("Yes", "No"), inline=TRUE),
        textAreaInput("qtxt5", "", placeholder=" ", width="100%"),
        radioButtons("answertype5", "Type of Answer", choices=c("Numeric Answer", "Multiple Choice", "Numeric Matrix", "Verbatim"), inline = TRUE),
        conditionalPanel( condition = "input.answertype5 == 'Numeric Answer'",
          column(3, textInput("numpoints5", "Point(s)", value="100", width = "50%")),
          column(3, textInput("numprecision5", "Precision(s)", value="0", width = "50%"))
        ),
        conditionalPanel( condition = "input.answertype5 == 'Multiple Choice'",
          column(3, textInput("mcoptions5", "Choices", placeholder="Yes,No,Maybe")),
          column(3, selectInput("mcoptions5a", "Choices (List)", choices = mcchoices))
        ),  
        textAreaInput("calculate.answer5", "Enter Calculation of Answer", placeholder=""),    
        textAreaInput("atxt5", "", placeholder="Enter answer text here. Answer will appear at the end, or at @ symbol if entered", width="100%"),
  ),
  conditionalPanel( condition = "input.numquestions > 5", 
        HTML("<hr>"),
        HTML("<h4>Sixth Question</h4>"),
        radioButtons("question6newline", "Start on new Line", choices=c("Yes", "No"), inline=TRUE),
        textAreaInput("qtxt6", "", placeholder=" ", width="100%"),
        radioButtons("answertype6", "Type of Answer", choices=c("Numeric Answer", "Multiple Choice", "Numeric Matrix", "Verbatim"), inline = TRUE),
        conditionalPanel( condition = "input.answertype6 == 'Numeric Answer'",
          column(3, textInput("numpoints6", "Point(s)", value="100", width = "50%")),
          column(3, textInput("numprecision6", "Precision(s)", value="0", width = "50%"))
        ),
        conditionalPanel( condition = "input.answertype6 == 'Multiple Choice'",
          column(3, textInput("mcoptions6", "Choices", placeholder="Yes,No,Maybe")),
          column(3, selectInput("mcoptions6a", "Choices (List)", choices = mcchoices))
        ),  
        textAreaInput("calculate.answer6", "Enter Calculation of Answer", placeholder=""),    
        textAreaInput("atxt6", "", placeholder=" ", width="100%"),
  ), 
  conditionalPanel( condition = "input.numquestions > 6", 
        HTML("<hr>"),
        HTML("<h4>Seventh Question</h4>"),
        radioButtons("question7newline", "Start on new Line", choices=c("Yes", "No"), inline=TRUE),
        textAreaInput("qtxt7", "", placeholder=" ", width="100%"),
        radioButtons("answertype7", "Type of Answer", choices=c("Numeric Answer", "Multiple Choice", "Numeric Matrix", "Verbatim"), inline = TRUE),
        conditionalPanel( condition = "input.answertype7 == 'Numeric Answer'",
          column(3, textInput("numpoints7", "Point(s)", value="100", width = "50%")),
          column(3, textInput("numprecision7", "Precision(s)", value="0", width = "50%"))
        ),
        conditionalPanel( condition = "input.answertype7 == 'Multiple Choice'",
          column(3, textInput("mcoptions7", "Choices", placeholder="Yes,No,Maybe")),
          column(3, selectInput("mcoptions7a", "Choices (List)", choices = mcchoices))
        ),  
        textAreaInput("calculate.answer7", "Enter Calculation of Answer", placeholder=""),    
        textAreaInput("atxt7", "", placeholder=" ", width="100%"),
  ),   
  HTML("<hr>"),
  radioButtons("addgraph", "Add a Graph?", choices=c("No", "Yes"), inline=TRUE),
  conditionalPanel( condition = "input.addgraph=='Yes'",
    textAreaInput("graphcommand", "Graph Commands", value="plt=")
  ),
  textAreaInput("htxt", "Any hints after first try?", placeholder="Did you forget to round?"),
  fluidRow(
   column(3, radioButtons("doquiz", "Generate xml file", choices = c("Yes", "No"), inline = TRUE)),
   column(3, actionButton("xmlbutton",HTML("<font color=\"red\">Execute!<font color=\"black\">")))
  ),
  fluidRow(verbatimTextOutput("text"))
))