### Author: Anne Lyngholm Sørensen
### Description: The user interface for the t-test shiny app

# libraries
library(shiny)
library(shinydashboard)
library(shinyjs)


# the ui
dashboardPage(
  dashboardHeader(title="statistical tests"),
  dashboardSidebar(sidebarMenu(
    menuItem("tests", tabName = "tests"),
    menuItem("examples", tabName = "examples"),
    menuItem("theory", tabName="theory")
  )
  ),
  dashboardBody(
    tabItems(
      tabItem("tests",
              tags$head(
                # Some css to style the div to make it more easily visible
                tags$style(
                  '#outDiv{
        height:150px;
        overflow-y:scroll;
        border: 1px solid black;
        border-radius:15px;
        padding:15px;
      }
      '
                ),
                # Custom shiny to javascript binding
                # scrolls "outDiv" to bottom once called
                tags$script(
                  '
      Shiny.addCustomMessageHandler("scrollCallback",
        function(color) {
          var objDiv = document.getElementById("outDiv");
          objDiv.scrollTop = objDiv.scrollHeight;
        }
      );'
                )
              ),
              fluidRow(
                column(width=12,
                       box(width = NULL, solidHeader = TRUE,
                           status = "warning",
                           title="Steps to do a statistical test using this app",
                           htmlOutput("steps"),
                           actionButton("selectButton", "Select existing data set"),
                           actionButton("uploadButton", "Use own data set"))
                )),
              # The select data-box
              fluidRow(
                column(width=5,
                       box(id = "selectData", width = NULL, #height = 350, 
                           status = "info",
                           h4("Choose data"),
                           hr(),
                           htmlOutput("dataChooseIntro"),
                           br(),
                           selectInput(inputId = "dataset",
                                       label = "Choose a dataset:",
                                       choices = c("differences_between_differences", "compare_train_group",
                                                   "compare_pre_post_interven"),
                                       width=300))
                ),
                column(width=7,
                       box(id="showSelectData", width=NULL, #height = 350,
                           status = "info",
                           h4("Visualization of a subset of data (existing)"),
                           hr(),
                           htmlOutput("dataChoose"),
                           br(),
                           tableOutput("contentsData")))),
              # The upload data box
              fluidRow(
                column(width=5,
                       box(id= "uploadData", width=NULL, #height = 425,
                           status = "info",
                           h4("Upload data"),
                           hr(),
                           htmlOutput("dataLoad"),
                           br(),
                           fileInput("file1", "Choose a .csv or .txt file:",
                                     multiple = FALSE,
                                     accept = c("text/csv",
                                                "text/comma-separated-values,text/plain",
                                                ".csv")),
                           selectInput("sep", "Choose separator:", c(comma=",", semicolon=";", tab = "\t", whitespace =" "),
                                       selected = NULL, multiple = FALSE,
                                       selectize = TRUE, width = 200, size = NULL),
                           checkboxInput("header", "Does the data have named columns (header)?", TRUE))),
                # The data visualization box
                column(width=7, 
                       box(id="showUploadData",
                           width=NULL, #height = 425,
                           status = "info",
                           h4("Visualization of a subset of data (upload)"),
                           tags$hr(),
                           htmlOutput("dataLoadText"),
                           br(),
                           tableOutput("contents")))),
              tabsetPanel(type = "tabs",
                          tabPanel("t-tests",
                                   # The t-test box
                                   useShinyjs(),
                                   fluidRow(
                                     column(width=12,
                                            box(width = NULL, 
                                                h4("Choice of t-test and assumptions"),
                                                hr(),
                                                textOutput("testSelector"),
                                                br(),
                                                actionButton("oneSample", "One Sample"),
                                                actionButton("twoSample", "Two Sample"),
                                                actionButton("paired", "Paired Sample"),
                                                htmlOutput("assumptions"),
                                                actionButton("understand", "I understand the assumptions and the assumptions are fulfilled"),
                                                br(),
                                                br(),
                                                textOutput("class"),
                                                br(),
                                                br(),
                                                htmlOutput("headArg"),
                                                hr(),
                                                fluidRow(width=12,
                                                         column(width=2,
                                                                uiOutput("arguments1")),
                                                         column(width=2,
                                                                uiOutput("arguments2")),
                                                         column(width=2,
                                                                br(),
                                                                actionButton("startTTest", "Compute t-test"))),
                                                htmlOutput("result")
                                            ))
                                   )),
                          tabPanel("Wilcox", textOutput("introWilcox")),
                          tabPanel("Linear", tableOutput("table"))
              )),
      tabItem("examples",
              titlePanel("Examples of app's three t-tests"),
              textOutput("testExamples"))
    )
  )
)
