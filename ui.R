### Author: Anne Lyngholm Sørensen
### Description: The user interface for the t-test shiny app

# libraries
library(shiny)
library(shinydashboard)
library(shinyjs)


# the ui
dashboardPage(
  dashboardHeader(title="t-test plug-in"),
  dashboardSidebar(sidebarMenu(
    menuItem("t-test", tabName = "t-test"),
    menuItem("examples", tabName = "examples"),
    menuItem("theory", tabName="theory")
  )
  ),
  dashboardBody(
    tabItems(
      tabItem("t-test",
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
              # The info box aka how-to
              fluidRow(
                column(width=12,
                       box(width = NULL, solidHeader = TRUE,
                           status = "warning",
                           title="Steps to do a t-test using this app",
                           htmlOutput("steps"))
                       )),
              # The upload data box
              fluidRow(
                column(width=5,
                       box(width=NULL, height = 425,
                           h4("Upload data"),
                           tags$hr(),
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
                       box(width=NULL, height = 425,
                           h4("Visualization of a subset of data"),
                           tags$hr(),
                           htmlOutput("dataLoadText"),
                           br(),
                           tableOutput("contents")))),
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
      tabItem("examples",
              titlePanel("Examples of app's three t-tests"),
              textOutput("testExamples"))
    )
  )
)
