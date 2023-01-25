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

server <- function(input, output, session) {
    
    #LOGIN
    result_auth <- secure_server(check_credentials = check_credentials(credentials))
    
    output$res_auth <- renderPrint({
        reactiveValuesToList(result_auth)
    })
    
    #HOME
    output$cutepic <- renderImage({
        filename <- normalizePath("data//doggo.jpg")
        
        list(src = filename, width = 300, height = 400)
    }, deleteFile = FALSE)
    
    uploadTask <- function() {
        
        prData <- add_row(prData, "Name" = input$prname, "List" = input$task, "Completed" = FALSE)
        
        if (!is.null(input$task2)) prData <- add_row(prData, "Name" = input$prname, "List" = input$task2, "Completed" = FALSE)
        
        if (!is.null(input$task3)) prData <- add_row(prData, "Name" = input$prname, "List" = input$task3, "Completed" = FALSE)
        
        if (!is.null(input$task4)) prData <- add_row(prData, "Name" = input$prname, "List" = input$task4, "Completed" = FALSE)
        
        if (!is.null(input$task5)) prData <- add_row(prData, "Name" = input$prname, "List" = input$task5, "Completed" = FALSE)
        
        range_write(pr,  prData)
        
        output$uploadedTask <- renderText("Uploaded")
        
    }
    
    observeEvent(input$addTask, uploadTask())
    
    output$todolist <- renderUI({
        if(input$prname == "Person1") namedList <- filteredTasks[filteredTasks$Name == "Person1",]
        if(input$prname == "Person2") namedList <- filteredTasks[filteredTasks$Name == "Person2",]
        if(input$showTasks) DetailsList(items = namedList, checkboxVisibility = 2)
    })
    
    output$taskComplete <- renderUI({
        if(input$prname == "Person1") namedList <- finishedTasks[finishedTasks$Name == "Person1",]
        if(input$prname == "Person2") namedList <- finishedTasks[finishedTasks$Name == "Person2",]
        if(input$showTasks) DetailsList(items = namedList, checkboxVisibility = 2)
    })
    
    output$todoListDone <- renderUI({
        if(input$prname == "Person1") namedList <- filteredTasks[filteredTasks$Name == "Person1",]
        if(input$prname == "Person2") namedList <- filteredTasks[filteredTasks$Name == "Person2",]
        selectizeInput("taskDone", "Congrats! What did you finish?", choices = namedList$List)
    })
    
    removeTask <- function(doneTask) {
        
        changeIndex <- NULL
        
        for (x in 1:NROW(prData)) {
            if (prData$List[x] == doneTask) changeIndex <- x
        }
        
        prData[changeIndex, 3] = TRUE
        
        range_write(pr, prData)
        
        output$markedDone <- renderText("Marked Done")
    }
    
    observeEvent(input$done, removeTask(input$taskDone))
    
    #STRESS TRACKER
    
    showtext <- function() {
        output$coping <- renderText(
            "Remember to do the following:
            \n
            \n1) Drink water and eat food
            \n2) Distract yourself:
            \n\t- Watch Netlix
            \n\t- Talk to Person1/Person2
            \n\t- Take a nap
            \n\t- Go shopping
            \n\t- Listen to music
            \n3) Focus on the positives
            \n Remember... everything will be okay"
        )
    }
    
    output$stressor <- renderUI({
        
        newData <- stData[(pull(stData, 1) == input$name),]    
        
        selectizeInput("stressorChoice", "Stressor:", choices = c(pull(newData, 2)), selected = NULL)
        
    })
    
    output$stressor2 <- renderUI({
        
        newData <- stData[(pull(stData, 1) == input$name2),]    
        
        selectizeInput("stressorChoice2", "Stressor:", choices = c(pull(newData, 2)), selected = NULL)
        
    })
    
    textOutput = function() {
        
        newData <- stData[(pull(stData, 1) == input$name),]
        
        selection <- newData[(pull(newData, 2) == input$stressorChoice),]
        
        print(pull(selection, 4))
    }
    
    textOutput2 = function() {
        
        newData <-  stData[(pull( stData, 1) == input$name2),]
        
        selection <- newData[(pull(newData, 2) == input$stressorChoice2),]
        
        print(pull(selection, 3))
    }
    
    upload <- function() {
        
        x <- NULL
        
        for(y in 1:NROW(stData)) {
            if( stData[y, 2] == input$stressorChoice2 &  stData[y, 1] == input$name2) x <- y
        }
        
        stData[x, 4] = input$advice
        
        range_write(st,  stData)
        
        output$uploadedAdvice <- renderText("Uploaded")
        
    }
    
    upload2 <- function() {
        
        stData <- add_row( stData, "Who are you?" = input$name3, "So, what is stressing you out?" = input$stressor1, "Wanna elaborate?" = input$elaboration)
        
        range_write(st,  stData)
        
        output$uploadedStressor <- renderText("Uploaded")
        
    }

    observeEvent(input$habits, showtext())
    
    observeEvent(input$upload, upload())
    
    observeEvent(input$upload2, upload2())
    
    output$welcome <- renderText(
        "Welcome to Stress Tracker Beta! This is a place where we can communicate about things that stress us out!
        For a quick summary, there are three things you can do here:
        \n1) First off, you can upload a stressor by clicking the tab which will
        allow you to fill out what's stressing you out and an explanation about it.
        \n2) The next thing you can do is give advice on the other person's stressors by 
        clicking the tab and choosing one of their stressors to give advice for.
        \n3) And lastly, you can view the advice given to you for various stressors that you have submitted when you click the first tab.
        \n(On the first tab you can also click the Healthy Habits button to be reminded of things to do if you're stressed out)"
    )
    
    output$text <- renderText(
        print(textOutput())
    )
    
    output$extraInfo <- renderText(
        print(textOutput2())
    )
    
    output$intro <- renderText(
        "Advice:"
    )
    
    output$elaboration <- renderText(
        "Extra Info:"
    )
    
    #BUCKET LIST
    
    uploadItem <- function() {
        
        blData <- add_row( blData, "Item" = input$itemName, "Type" = input$itemType, "IsDone" = FALSE)
        
        range_write(bl,  blData)
        
        output$uploadedItem <- renderText("Uploaded")
    }
    
    observeEvent(input$uploadItem, uploadItem())
    
    output$bucketList <- renderUI(
        if(input$showLeft) DetailsList(items = filteredItems, checkboxVisibility = 2)
    )

    output$bucketListDone <- renderUI(
        selectizeInput("itemDone", "What should we cross off?", choices = filteredItems$Item)
    )
    
    output$finishedList <- renderUI(
        if(input$showDone) DetailsList(items = finishedItems, checkboxVisibility = 2)
    )
    
    removeItem <- function(item) {
        
        changeIndex <- NULL
        
        for (x in 1:NROW(blData)) {
            if (blData$Item[x] == item) changeIndex <- x
        }
        
        blData[changeIndex, 3] = TRUE
        
        range_write(bl, blData)
        
        output$markedDone <- renderText("Marked Done")
    }
    
    observeEvent(input$done, removeItem(input$itemDone))
    
    reRandomizeFood <- function() {
        food <- filteredItems[filteredItems$Type == "Food",]
        index <- floor(runif(1, min = 1, max = NROW(food) + 1))
        updateSelectizeInput(session = getDefaultReactiveDomain(), "foodSelection", "Selected Food Option", choices = food$Item, selected = food[index, 1])
    }
    
    reRandomizeActivity <- function() {
        activity <- filteredItems[filteredItems$Type == "Activity",]
        indexa <- floor(runif(1, min = 1, max = NROW(activity) + 1))
        updateSelectizeInput(session = getDefaultReactiveDomain(), "activitySelection", "Selected Activity Option", choices = activity$Item, selected = activity[indexa, 1])
    }
    
    reRandomizeShopping <- function() {
        shopping <- filteredItems[filteredItems$Type == "Shopping",]
        indexs <- floor(runif(1, min = 1, max = NROW(shopping) + 1))
        updateSelectizeInput(session = getDefaultReactiveDomain(), "shoppingSelection", "Selected Shopping Option", choices = shopping$Item, selected = shopping[indexs, 1])
    }
    
    reRandomizeTrip <- function() {
        trip <- filteredItems[filteredItems$Type == "Day Trip",]
        indext <- floor(runif(1, min = 1, max = NROW(trip) + 1))
        updateSelectizeInput(session = getDefaultReactiveDomain(), "tripSelection", "Selected Day Trip Option", choices = trip$Item, selected = trip[indext, 1])
    }
    
    output$randomizedFood <- renderUI({
        if("Food" %in% input$inclusions) {
            food <- filteredItems[filteredItems$Type == "Food",]
            indexf <- floor(runif(1, min = 1, max = NROW(food) + 1))
            selectizeInput("foodSelection", "Selected Food Option", choices = food$Item, selected = food[indexf, 1])
        }
    })
    
    output$randomizedActivity <- renderUI({
        if("Activity" %in% input$inclusions){
            activity <- filteredItems[filteredItems$Type == "Activity",]
            indexa <- floor(runif(1, min = 1, max = NROW(activity) + 1))
            selectizeInput("activitySelection", "Selected Activity Option", choices = activity$Item, selected = activity[indexa, 1])
        }
    })
    
    output$randomizedShopping <- renderUI({
        if("Shopping" %in% input$inclusions) {
            shopping <- filteredItems[filteredItems$Type == "Shopping",]
            indexs <- floor(runif(1, min = 1, max = NROW(shopping) + 1))
            selectizeInput("shoppingSelection", "Selected Shopping Option", choices = shopping$Item, selected = shopping[indexs, 1])
        }
    })
    
    output$randomizedTrip <- renderUI({
        if("Day Trip" %in% input$inclusions) {
            trip <- filteredItems[filteredItems$Type == "Day Trip",]
            indext <- floor(runif(1, min = 1, max = NROW(trip) + 1))
            selectizeInput("tripSelection", "Selected Day Trip Option", choices = trip$Item, selected = trip[indext, 1])
        }
    })
    
    randomize <- function() {
        if("Food" %in% input$inclusions) reRandomizeFood()
        if("Activity" %in% input$inclusions) reRandomizeActivity()
        if("Shopping" %in% input$inclusions) reRandomizeShopping()
        if("Day Trip" %in% input$inclusions) reRandomizeTrip()
    }
    
    output$seasonSelect <- renderUI({
        selectInput("season", "Choose a season:", choices = c("Winter", "Spring", "Summer", "Fall"))
    })
    
    output$searchFood <- renderUI({
        url <- str_interp("https://www.google.com/maps/search/food+near+${input$city}")
        
        a("Find Food!", href = url, target = '_blank')
    })
    
    output$searchIdeas <- renderUI({
        url <- str_interp("https://www.google.com/search?q=${input$season}+bucket+list+ideas")
        
        a("Find Ideas!", href = url, target = '_blank')
    })
    
    observeEvent(input$generate, randomize())
    
    observeEvent(input$searchFood, searchFood())
    
    observeEvent(input$searchItems, searchIdeas())
    
    #MOVIE NIGHT
    
    uploadMovie <- function() {
        
        mvData <- add_row(mvData, "MovieName" = input$movieName, "Category" = input$category, "isWatched" = FALSE)
        
        range_write(mv,  mvData)
        
        output$uploadedMovie <- renderText("Uploaded")
        
    }
    
    observeEvent(input$uploadMovie, uploadMovie())
    
    output$movieList <- renderUI(
        if(input$showUnwatched) DetailsList(items = filteredMovies, checkboxVisibility = 2)
    )
    
    output$watchedList <- renderUI(
        if(input$showWatched) DetailsList(items = finishedMovies, checkboxVisibility = 2)
    )
    
    output$movieListDone <- renderUI(
        selectizeInput("movieDone", "What should we cross off?", choices = filteredMovies$MovieName)
    )
    
    removeMovie <- function(movie) {
        
        changeIndexMv <- NULL
        
        for (y in 1:NROW(mvData)) {
            if (mvData$MovieName[y] == movie) changeIndexMv <- y
        }
        
        mvData[changeIndexMv, 3] = TRUE
        
        range_write(mv, mvData)
        
        output$markedDone <- renderText("Marked Watched")
    }
    
    observeEvent(input$watched, removeItem(input$movieDone))
    
    output$genre <- renderUI({
        selectizeInput("genre", "What type of movie?", choices = c("Classic", "Random", "Thriller", "Comedy"), selected = NULL, width = "500px")
    })
    
    output$title <- renderUI({
        
        genreSpec <- mvData[mvData$Category == input$genre,]
        
        selectizeInput("title", "Which one?", choices = pull(genreSpec, 1))
    })
    
    output$watch <- renderUI({
        Text(input$title, variant = "large")
    })
    
    output$selected <- renderUI({
        title <- gsub(" ", "-", input$title)
        url <- str_interp("https://www.justwatch.com/us/movie/${title}")
        a("Watch Options", href = url, target = "_blank")
    })
    
    output$snack <- renderUI({
        a("Snack Ideas", href = 'https://www.bonappetit.com/gallery/movie-night-snacks', target = '_blank')
    })
    
    output$theater <- renderUI({
        a("Movies Out Now", href = 'https://www.fandango.com/', target = '_blank')
    })
    
}

