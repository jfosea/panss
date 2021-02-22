source("modules/welcomePage.R")
source("modules/trainMePage.R")

db <- dbPool(RSQLite::SQLite(), dbname = "workshop.db")

header <- dashboardHeader(title="Actions")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Welcome",tabName = "welcome"),
    menuItem("Train Me", tabName = "trainMe")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "welcome", welcomePageUI("welcomePage-module")),
    tabItem(tabName = "trainMe", trainMePageUI("trainMePage-module"))
  )
)

server <- function(input, output, session) {
  callModule(welcomePage, "welcomePage-module")
  callModule(trainMePage, "trainMePage-module")
}

ui <- dashboardPage(header, sidebar, body)
shinyApp(ui = ui, server = server)