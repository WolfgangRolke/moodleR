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
))