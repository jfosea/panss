library("shiny")
library("shinydashboard")
library("dplyr")
library("dbplyr")
library("pool")
library("DBI")
library("rlang")
library("shinyjs")
library("tidyr")
library("shinyWidgets")
library("RSQLite")


source("modules/welcomePage.R")
source("modules/trainMe.R")
source("modules/viewPast.R")

db <- dbPool(RSQLite::SQLite(), dbname = "workshop.db")

header <- dashboardHeader(title="Actions")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Welcome",tabName = "welcome"),
    menuItem("Train Me", tabName = "trainMe"),
    menuItem("View Past", tabName = "viewPast")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "welcome", welcomePageUI("welcomePage-module")),
    tabItem(tabName = "trainMe", trainMeUI("trainMe-module")),
    tabItem(tabName = "viewPast", viewPastUI("viewPast-module"))
  )
)

server <- function(input, output, session) {
  callModule(welcomePage, "welcomePage-module")
  callModule(trainMe, "trainMe-module", db)
  callModule(viewPast, "viewPast-module", db)
}

ui <- dashboardPage(header, sidebar, body)
shinyApp(ui = ui, server = server)