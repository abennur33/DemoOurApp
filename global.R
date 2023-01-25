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
library(shinyjs)

inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"

credentials <- data.frame(
  user = c("Test"),
  password = c("Test"),
  stringsAsFactors = FALSE
)

gs4_auth(cache = ".secrets")
gs4_auth(cache = ".secrets", email = TRUE, use_oob = TRUE)

st <- gs4_get("https://docs.google.com/spreadsheets/d/1iqpE5scjGBrgExtJ-1Mm7E85dEy-s5UVunnpF075JEk/edit#gid=324233014")
stData <- read_sheet(st)

bl <- gs4_get("https://docs.google.com/spreadsheets/d/1MrjVR_-53h7ZqsINv9Gvp-fMECvrpiUzJuVXGqoNS64/edit#gid=1949185410")
blData <- read_sheet(bl)

mv <- gs4_get("https://docs.google.com/spreadsheets/d/1RAiSHO7eHZw5NsC-ZmWRfUkluGSi79iJg3xEA6DQVcE/edit#gid=2012257961")
mvData <- read_sheet(mv)

pr <- gs4_get("https://docs.google.com/spreadsheets/d/1qafTLhO8f9qoEQD3KlpDIUsPndguLJD0iZC4oTcJ600/edit#gid=634186678")
prData <- read_sheet(pr)


Card <- function(..., title = NULL) {
  Stack(
    class = "ms-depth-8",
    tokens = list(padding = 20, childrenGap = 10),
    if (!is.null(title)) Text(title, variant = "large"),
    ...
  )
}


columns <- tibble(
  fieldName <- c("Item", "Type"),
  name <- c("Item", "Type")
)

filteredItems <- blData[!blData$IsDone,]

filteredMovies <- mvData[!mvData$isWatched,]

finishedItems <- blData[blData$IsDone,]

finishedMovies <- mvData[mvData$isWatched,]

filteredTasks <- prData[!prData$Completed,]

finishedTasks <- prData [prData$Completed,]