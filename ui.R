library(shiny)
library(shinythemes)
library(shiny.fluent)
library(shinymanager)
library(slickR)
library(data.table)
library(stringr)
library(tidyverse)
library(googlesheets4)
library(dplyr)
library(bslib)
library(tibble)



welcomeOurApp <- "This is Our App, the hub for everything we could ever want. Feel free to look around and explore!"

ui <- secure_app(head_auth = tags$script(inactivity),
                 
                 fluidPage(
                     
                     theme = bs_theme(version = 4, bootswatch = "lux"),
                     
                     # Application title
                     titlePanel("Our App"),
                     
                     navbarPage("Menu",
                                tabPanel("Home", fluid = TRUE,
                                         tabsetPanel(
                                             tabPanel("Welcome to Our App", fluid = TRUE,
                                                Card(title = "Welcome",
                                                    Text(welcomeOurApp)
                                                )
                                             ),
                                             tabPanel("Smile Center", fluid = TRUE,
                                                sidebarLayout(
                                                     sidebarPanel(
                                                         Card(title = "Happiness Countdowns",
                                                              a("Halloween", href = 'https://www.timeanddate.com/countdown/halloween?p0=64&msg=Halloween&font=hand', target = '_blank'),
                                                              a("Thanksgiving Break", href = 'https://www.timeanddate.com/countdown/vacation?iso=20211123T1445&p0=64&msg=Thanksgiving+Break&font=hand', target = '_blank'),
                                                              a("Winter Break", href = 'https://www.timeanddate.com/countdown/vacation?iso=20211222T1445&p0=64&msg=Christmas+Break&font=hand', target = '_blank'),
                                                              a("Christmas", href = 'https://www.timeanddate.com/countdown/christmas?p0=64&msg=Christmas&font=hand', target = '_blank')
                                                         )
                                                     ),
                                                     mainPanel(
                                                         Card(title = "Smiley Faces",
                                                             imageOutput("cutepic")
                                                         ),
                                                     )
                                                 )
                                             ),
                                             tabPanel("Productivity Station", fluid = TRUE,
                                                      sidebarLayout(
                                                        sidebarPanel(
                                                          selectizeInput("prname", "Who are you?", choices = c("Person1", "Person2"), selected = NULL, width = "500px"),
                                                          Text("Tasks 2-5 are optional*", variant = "medium"),
                                                          textInput("task", "Enter task"),
                                                          textInput("task2", "Enter task"),
                                                          textInput("task3", "Enter task"),
                                                          textInput("task4", "Enter task"),
                                                          textInput("task5", "Enter task"),
                                                          actionButton("addTask", "Add Tasks"),
                                                          textOutput("uploadedTask")
                                                        ),
                                                        mainPanel(
                                                          Card(title =  "Show List?",
                                                               Toggle.shinyInput("showTasks", "Show List?")
                                                          ),
                                                          Card(title =  "Show Completed Tasks?",
                                                               Toggle.shinyInput("showComplete", "Show Completed Tasks?")
                                                          ),
                                                          Card(title = "To-Do",
                                                            uiOutput("todolist")
                                                          ),
                                                          Card(title = "Finishing Tasks",
                                                               uiOutput("todoListDone"),
                                                               actionButton("done", "Mark as completed"),
                                                               textOutput("markedFinished")
                                                          ),
                                                          Card(title = "Completed Tasks",
                                                               uiOutput("taskComplete")
                                                          )
                                                        )
                                                      )
                                               
                                             )
                                         )
                                ),
                                #STRESS TRACKER
                                tabPanel("Stress Tracker", fluid = TRUE,
                                         tabsetPanel(
                                             tabPanel( "Welcome", fluid = TRUE,
                                                       mainPanel(
                                                           verbatimTextOutput("welcome"),
                                                       )
                                             ),
                                             tabPanel("Get Advice", fluid = TRUE,
                                                      sidebarLayout(
                                                          sidebarPanel(
                                                              selectizeInput("name", "Who are you?", choices = c("Person1", "Person2"), selected = NULL, width = "500px"),
                                                              
                                                              uiOutput("stressor"),
                                                              
                                                              actionButton("habits", "Healthy Habits")
                                                          ),
                                                          
                                                          # Show a plot of the generated distribution 
                                                          mainPanel(
                                                              textOutput("intro"),
                                                              textOutput("text"),
                                                              verbatimTextOutput("coping")
                                                          )
                                                      )
                                             ),
                                             tabPanel("Give Advice", fluid = TRUE,
                                                      sidebarLayout(
                                                          sidebarPanel(
                                                              selectizeInput("name2", "Who are you giving advice to?", choices = c("Person1", "Person2"), selected = NULL, width = "500px"),
                                                              
                                                              uiOutput("stressor2"),
                                                          ),
                                                          
                                                          mainPanel(
                                                              textOutput("elaboration"),
                                                              textOutput("extraInfo"),
                                                              textInput("advice", "Give Advice: ", width = "400px"),
                                                              actionButton("upload", "Submit Advice"),
                                                              textOutput("uploadedAdvice")
                                                          )
                                                      )
                                             ),
                                             tabPanel("Submit Stressor", fluid = TRUE,
                                                      mainPanel(
                                                          selectizeInput("name3", "Who are you?", choices = c("Person1", "Person2"), selected = NULL, width = "500px"),
                                                          textInput("stressor1", "So, what is stressing you out?"),
                                                          textInput("elaboration", "Wanna Elaborate?"),
                                                          actionButton("upload2", "Upload Stressor"),
                                                          textOutput("uploadedStressor")
                                                      )
                                             )
                                         )
                                ),
                                tabPanel("Bucket List", fluid = TRUE,
                                         tabsetPanel(
                                             tabPanel("Our List", fluid = TRUE,
                                                  sidebarLayout(
                                                    sidebarPanel(
                                                        textInput("itemName", "What should we do?"),
                                                        selectizeInput("itemType", "What type of activity?", choices = c("Food", "Activity", "Shopping", "Day Trip"), selected = NULL, width = "500px"),
                                                        actionButton("uploadItem", "Add Item"),
                                                        textOutput("uploadedItem"),
                                                    ),
                                                    mainPanel(
                                                      Card(title =  "Show Incomplete Items?",
                                                           Toggle.shinyInput("showLeft", "Show Incomplete Items?")
                                                      ),
                                                      Card(title =  "Show Completed Items?",
                                                        Toggle.shinyInput("showDone", "Show Completed Items?")
                                                      ),
                                                      uiOutput("bucketList"),
                                                      uiOutput("bucketListDone"),
                                                      actionButton("done", "Mark as done"),
                                                      textOutput("markedDone"),
                                                      uiOutput("finishedList")
                                                    )
                                                  )
                                             ),
                                             tabPanel("Day Planner", fluid = TRUE,
                                                      sidebarLayout(
                                                        sidebarPanel(
                                                          Card(title = "Choose what type of day it'll be:",
                                                               checkboxGroupInput(
                                                                 inputId = "inclusions", label = "Select activitiy types:",
                                                                 choices = c("Food", "Activity", "Shopping",
                                                                             "Day Trip"),
                                                                 selected = NULL, width = "400px"
                                                               ),
                                                               actionButton("generate", "Generate Combo")
                                                          )
                                                        ),
                                                        mainPanel(
                                                          uiOutput("randomizedFood"),
                                                          uiOutput("randomizedActivity"),
                                                          uiOutput("randomizedShopping"),
                                                          uiOutput("randomizedTrip"),
                                                          Card(title = "Food Finder",
                                                              textInput("city", "Find food near..."),
                                                              uiOutput("searchFood")
                                                          ),
                                                          Card(title = "Seasonal Bucket List Item Finder",
                                                              uiOutput("seasonSelect"),
                                                             uiOutput("searchIdeas")
                                                          )
                                                        )
                                                      )   
                                             )
                                         )
                                         
                                ),
                                tabPanel("Movie Night", fluid = TRUE,
                                         tabsetPanel(
                                             tabPanel("Movies to Watch", fluid = TRUE,
                                                      sidebarLayout(
                                                        sidebarPanel(
                                                          textInput("movieName", "What should we watch?"),
                                                          selectizeInput("category", "What type of movie?", choices = c("Classic", "Random", "Thriller", "Comedy"), selected = NULL, width = "500px"),
                                                          actionButton("uploadMovie", "Add Movie"),
                                                          textOutput("uploadedMovie")
                                                        ),
                                                        mainPanel(
                                                          Card(title =  "Show Movies Left To Watch?",
                                                               Toggle.shinyInput("showUnwatched", "Show Unwatched Movies?")
                                                          ),
                                                          Card(title =  "Show Completed Movies?",
                                                            Toggle.shinyInput("showWatched", "Show Completed Movies?")
                                                          ),
                                                          uiOutput("movieList"),
                                                          uiOutput("movieListDone"),
                                                          actionButton("watched", "Mark as watched"),
                                                          textOutput("markedWatched"),
                                                          uiOutput("watchedList")
                                                        )
                                                      )
                                             ),
                                             tabPanel("Movie Night", fluid = TRUE,
                                                sidebarLayout(
                                                  sidebarPanel(
                                                    Card(title = "Movie Selector",
                                                      uiOutput("genre"),
                                                      uiOutput("title")
                                                    )
                                                  ),
                                                  mainPanel(
                                                      Card(title = "Movie Time",
                                                          uiOutput("watch"),
                                                          uiOutput("selected")
                                                      ),
                                                      Card(title = "Extras",
                                                           uiOutput("snack")
                                                      ),
                                                      Card(title = "If you want to go to the theaters...",
                                                           uiOutput("theater")
                                                      )
                                                      
                                                  )
                                                )
                                             )
                                         )
                                )
                     )
                 )
)
