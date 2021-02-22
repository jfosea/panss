source("modules/welcomePage.R")
source("modules/trainMePage.R")

db <- dbPool(RSQLite::SQLite(), dbname = "workshop.db")

header <- dashboardHeader(title="Actions")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Welcome Page",tabName = "welcome")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "welcome", welcomePageUI("welcomePage-module"))
  )
)

server <- function(input, output, session) {
  callModule(welcomePage, "welcomePage-module")
}

ui <- dashboardPage(header, sidebar, body)
shinyApp(ui = ui, server = server)