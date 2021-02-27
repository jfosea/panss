welcomePageUI <- function(id) {
  ns <- NS(id)
  tagList(h2("Welcome!"),
          h4("This page is developed by Statistical Consulting Service
             Department of Mathematics and Statistics at the University of Calgary."),
          h4("We were appointed by the College of Physicians and Surgeons of Alberta
             to create an analysis system to train physicians on how to use the 
             Positive and Negative Syndrome Scale (PANSS) instrument. Each physician can use
             this app to answer the existence of each symptom of a sample patient. After completing
             the questions, each physician is able to see their results immediately. They
             can also choose to save their results in the database. Users of this app can 
             also view past results of previous users."))
}

welcomePage <- function(input, output, session) {
}