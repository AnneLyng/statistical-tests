### Author: Anne Lyngholm Sørensen
### Description: The server part of the t-test shiny app

server <- function(input, output, session){
 output$dataLoad <- renderText({
   "Please insert data in either .txt or .csv file format.<br>Notice that the upload may give you an error, 
if the the separator is incorrect.
   Try either looking at your data in e.g. Notepad or an equivalent software before uploading the data
   and choose the correct separator for your data."
 })
 
 output$steps <- renderText({
   "To compute a t-test using this plug-in, you need to do the following steps. 
    <br><ol><li>Upload your data in the box below</li>
   <li>Ensure that the structure of the data is correct using the Visualization of a subset of data-box</li>
   <li>Choose the t-test that suits the hypothesis you wish to test</li>
   <li>Agree to the assumptions</li>
   <li>Compute the t-test</li></ol>"
 })
 
 
 output$testExamples <- renderText({
   "We will present examples of the three types of t-test"
 })
 
 output$dataLoadText <- renderText({
   "Use the visulization of your data to ensure that your data is read correctly. <br>
     The data should be divided into columns. If not try to select another separator in the 'Choose separator' dropdown."
 })

 
 output$contents <- renderTable({
   
   # input$file1 will be NULL initially. After the user selects
   # and uploads a file, head of that data file by default,
   # or all rows if selected, will be shown.
   
   if(exists("ttest_data")){
     rm(ttest_data, pos=".GlobalEnv")
   }
   req(input$file1)
   
   # when reading semicolon separated files,
   # having a comma separator causes `read.csv` to error
   tryCatch(
     {
       ttest_data <<- read.csv(input$file1$datapath,
                      header = input$header,
                      sep = input$sep)
     },
     error = function(e) {
       # return a safeError if a parsing error occurs
       stop(safeError(e))
     }
   )
   
   return(head(ttest_data))
   
 })
 
 output$testSelector <- renderText({
   "Choose the t-test for your hypothesis, which you wish to test.
   If you are in doubt about which t-test to choose, the examples in the examples tab
   in the left may provide the information that you need."
 })
 
 v <- reactiveValues(doTest = FALSE)
 dataCheck <<- 0
 testMethod <<- "oneSample"
 
 observeEvent(input$oneSample, 
              {output$assumptions <- renderText({paste("<h4>You have choosen the <font color='#0080ff'>one sample</font>
                                                      t-test.</h4> &emsp;To compute the one sample t-test, a numerical
variable is required, besides this, the test comes with the following assumptions:<br><br>
&emsp;&emsp;1. The sample is approximately normally distributed<br>
&emsp;&emsp;2. Observations are independent<br><br>
                                                       To do the one sample t-test press the following button:")})
              testMethod <<- "oneSample"})
 
 observeEvent(input$twoSample, 
              {output$assumptions <- renderText({"<h4>You have choosen the <font color='#0080ff'>two sample</font> 
                t-test.</h4>  &emsp;To compute the two sample t-test, two numerical
variables are required, besides this, the test comes with the following assumptions:<br><br>
&emsp;&emsp;1. The two samples are mutually independent<br>
&emsp;&emsp;2. Observations from each sample are independent<br>
&emsp;&emsp;3. Both variables are normally distributed<br><br>
                To do the two sample t-test press the following button:"})
              testMethod <<- "twoSample"})
 
 observeEvent(input$paired, 
              {output$assumptions <- renderText({"<h4>You have choosen the <font color='#0080ff'>paired</font> t-test.</h4>
                &emsp;To compute the two sample t-test, two numerical
variables are required, besides this, the test comes with the following assumptions:<br><br>
&emsp;&emsp;1. We have a sample of paired observations<br>
&emsp;&emsp;2. Observation pairs (e.g. subjects) are mutually independent<br>
&emsp;&emsp;3. The difference between the two observations are approximately normally distributed<br><br>
                To do the paired t-test press the following button:"})
              testMethod <<- "paired"})
 
 # the understand and assumptions fulfilled button
 observe({
   shinyjs::hide("understand")
   
   if(input$oneSample | input$twoSample | input$paired){
     shinyjs::show("understand")
   }
 })
 
 
 if(testMethod == "twoSample"){
 observeEvent(input$paired, {
   v$doTest <- FALSE
 })
   observeEvent(input$oneSample, {
     v$doTest <- FALSE
   })}
 
 if(testMethod == "oneSample"){
   observeEvent(input$paired, {
     v$doTest <- FALSE
   })
   observeEvent(input$twoSample, {
     v$doTest <- FALSE
   })}
 
 if(testMethod == "paired"){
   observeEvent(input$twoSample, {
     v$doTest <- FALSE
   })
   observeEvent(input$oneSample, {
     v$doTest <- FALSE
   })}
 
 observeEvent(input$understand,
              {if(exists("ttest_data")){
                  dataCheck <<- 1
                  output$class <- renderText({if(v$doTest == FALSE) return()
                                              isolate({"Please supply the needed information below:"})})
                  if(testMethod=="oneSample"){
                    output$headArg <- renderText({if(v$doTest == FALSE) return()
                      isolate({"<h4>Arguments in the <font color='#0080ff'>one sample</font> t-test:</h4>"})})
                    output$arguments1 <- renderUI({if(v$doTest == FALSE) return()
                      isolate({selectInput("varInterest1", "Sample of interest:", colnames(ttest_data),
                                                              selected = NULL, multiple = FALSE,
                                                              selectize = TRUE, width = 200, size = NULL)})})
                    output$arguments2 <- renderUI({if(v$doTest == FALSE) return()
                      isolate({numericInput("varInterest2", "Test value:", 0, width=200)})
                      })
                    output$result <- renderPrint({if(v$doTest == FALSE) return()})
                  } else if(testMethod=="twoSample"){
                    output$headArg <- renderText({if(v$doTest == FALSE) return()
                      isolate({"<h4>Arguments in the <font color='#0080ff'>two sample</font> t-test:</h4>"})})
                    output$arguments1 <- renderUI({if(v$doTest == FALSE) return()
                      isolate({selectInput("varInterest1", "Sample 1 of interest:", colnames(ttest_data),
                                  selected = NULL, multiple = FALSE,
                                  selectize = TRUE, width = 200, size = NULL)})})
                    output$arguments2 <- renderUI({if(v$doTest == FALSE) return()
                      isolate({selectInput("varInterest2", "Sample 2 of interest:", colnames(ttest_data),
                                  selected = NULL, multiple = FALSE,
                                  selectize = TRUE, width = 200, size = NULL)})})
                    output$result <- renderPrint({if(v$doTest == FALSE) return()})
                  } else if(testMethod=="paired"){
                    output$headArg <- renderText({if(v$doTest == FALSE) return()
                      isolate({"<h4>Arguments in the <font color='#0080ff'>paired</font> t-test:</h4>"})})
                    output$arguments1 <- renderUI({if(v$doTest == FALSE) return()
                      isolate({selectInput("varInterest1", "Sample 1 of interest:", colnames(ttest_data),
                                  selected = NULL, multiple = FALSE,
                                  selectize = TRUE, width = 200, size = NULL)})})
                    output$arguments2 <- renderUI({if(v$doTest == FALSE) return()
                      isolate({selectInput("varInterest2", "Sample 2 of interest:", colnames(ttest_data),
                                  selected = NULL, multiple = FALSE,
                                  selectize = TRUE, width = 200, size = NULL)})})
                    output$result <- renderText({if(v$doTest == FALSE) return()})
                    }
              } else {
                dataCheck <<- 0
                  output$class <- renderText({if(v$doTest == FALSE) return()
                                              isolate({"Data is nessecary to compute a t-test. Load the data in the 'Upload data' box"})})
                  output$result <- renderText({if(v$doTest == FALSE) return()})
              }
                session$sendCustomMessage(type = "scrollCallback", 1)}
                )
 
 observeEvent(input$understand, {
   v$doTest <- input$understand})
 
 observe({
   if(v$doTest == FALSE){
     shinyjs::hide("startTTest")}
   
   if(input$understand & v$doTest){
       isolate({shinyjs::show("startTTest")})
   }
 })
 
 
 
 testReact1 <- reactive({
   input$varInterest1
 })
 
 testReact2 <- reactive({
   input$varInterest2
 })
 
 
outputText <- "P-value of: "
 observeEvent(input$startTTest,
              {
                if(testMethod == "oneSample"){
                  myval1 <- testReact1()
                  myval2 <- testReact2()
                  vec <- eval(parse(text=paste("ttest_data$",myval1,sep="")))
                  pval <- t.test(x=vec,mu=myval2)$p.value
                  if(pval < 0.0001) {pval <- "< 0.0001"
                  } else {
                    pval <- round(pval,4)
                  }
                  intConf <- t.test(x=vec,mu=myval2)$conf.int
                  output$result <- renderText({if(v$doTest == FALSE) return()
                    isolate({paste("<br><br><h4>Results</h4><hr>",
                                   outputText, pval, ". And a 95% confidence interval of ", round(intConf[1],4), "; ", 
                                   round(intConf[2],4), ".", sep="")})})
                } else if(testMethod=="twoSample"){
                  myval1 <- testReact1()
                  myval2 <- testReact2()
                  vec1 <- eval(parse(text=paste("ttest_data$",myval1,sep="")))
                  vec2 <- eval(parse(text=paste("ttest_data$",myval2,sep="")))
                  pval <- t.test(x=vec1,y=vec2)$p.value
                  if(pval < 0.0001) {pval <- "< 0.0001"
                  } else {
                    pval <- round(pval,4)
                  }
                  intConf <- t.test(x=vec1,y=vec2)$conf.int
                  output$result <- renderText({if(v$doTest == FALSE) return()
                    isolate({paste("<br><br><h4>Results</h4><hr>",outputText, round(pval,4), ". And a 95% confidence interval of ",
                                   round(intConf[1],4), 
                                   "; ", round(intConf[2],4), ".", sep="")})})
                } else {
                  myval1 <- testReact1()
                  myval2 <- testReact2()
                  vec1 <- eval(parse(text=paste("ttest_data$",myval1,sep="")))
                  vec2 <- eval(parse(text=paste("ttest_data$",myval2,sep="")))
                  pval <- t.test(x=vec1,y=vec2)$p.value
                  if(pval < 0.0001) {pval <- "< 0.0001"
                  } else {
                    pval <- round(pval,4)
                  }
                  intConf <- t.test(x=vec1,y=vec2)$conf.int
                  output$result <- renderText({if(v$doTest == FALSE) return()
                    isolate({paste("<br><br><h4>Results</h4><hr>",outputText, round(pval,4), ". And a 95% confidence interval of  ",
                                                      round(intConf[1],4),
                                   "; ", round(intConf[2],4), ".", sep="")})})
                }
              })
 
 
}
