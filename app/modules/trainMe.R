trainMeUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Training Questions"),
    h4("Assess the existence of each symptom in the patient where 1 - Low and 7 - High"),
    box(title = "Your Info", width = 20, solidHeader = TRUE, status = "primary",
        numericInput(ns("rater"), "Rater ID", NULL),
        radioButtons(ns("lang"), "Language", inline=TRUE,
                     choices = list("English" = 1, "French" = 2, "Italian" = 3))
    ),
    box(title = "Positive", width = 20, solidHeader = TRUE, status = "primary",
        radioButtons(ns("q1"), "Delusions", inline=TRUE,
                     choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4,
                                    "5" = 5, "6" = 6, "7" = 7)),
        textOutput(ns("a1")),
        radioButtons(ns("q2"), "Conceptual disorganization", inline=TRUE,
                     choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4,
                                    "5" = 5, "6" = 6, "7" = 7)),
        textOutput(ns("a2")),
        radioButtons(ns("q3"), "Hallucinatory behavior", inline=TRUE,
                     choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4,
                                    "5" = 5, "6" = 6, "7" = 7)),
        textOutput(ns("a3")),
        radioButtons(ns("q4"), "Excitement", inline=TRUE,
                     choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4,
                                    "5" = 5, "6" = 6, "7" = 7)),
        textOutput(ns("a4")),
        radioButtons(ns("q5"), "Grandsiosity", inline=TRUE,
                     choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4,
                                    "5" = 5, "6" = 6, "7" = 7)),
        textOutput(ns("a5")),
        radioButtons(ns("q6"), "Suspiciousness/Persecution", inline=TRUE,
                     choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4,
                                    "5" = 5, "6" = 6, "7" = 7)),
        textOutput(ns("a6")),
        radioButtons(ns("q7"), "Hostility", inline=TRUE,
                     choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4,
                                    "5" = 5, "6" = 6, "7" = 7)),
        textOutput(ns("a7"))
    ),
    box(title = "Negative", width = 20, solidHeader = TRUE, status = "primary",
        radioButtons(ns("q8"), "Blunted affect", inline=TRUE,
                     choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4,
                                    "5" = 5, "6" = 6, "7" = 7)),
        textOutput(ns("a8")),
        radioButtons(ns("q9"), "Emotional withdrawal", inline=TRUE,
                     choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4,
                                    "5" = 5, "6" = 6, "7" = 7)),
        textOutput(ns("a9")),
        radioButtons(ns("q10"), "Poor rapport", inline=TRUE,
                     choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4,
                                    "5" = 5, "6" = 6, "7" = 7)),
        textOutput(ns("a10")),
        radioButtons(ns("q11"), "Passive/Apathetic social withdrawal", inline=TRUE,
                     choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4,
                                    "5" = 5, "6" = 6, "7" = 7)),
        textOutput(ns("a11")),
        radioButtons(ns("q12"), "Difficulty in abstract thinking", inline=TRUE,
                     choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4,
                                    "5" = 5, "6" = 6, "7" = 7)),
        textOutput(ns("a12")),
        radioButtons(ns("q13"), "Lack of spontaneity", inline=TRUE,
                     choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4,
                                    "5" = 5, "6" = 6, "7" = 7)),
        textOutput(ns("a13")),
        radioButtons(ns("q14"), "Stereotyped thinking", inline=TRUE,
                     choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4,
                                    "5" = 5, "6" = 6, "7" = 7)),
        textOutput(ns("a14"))
    ),
    box(title = "General", width = 20, solidHeader = TRUE, status = "primary",
        radioButtons(ns("q15"), "Somatic concern", inline=TRUE,
                     choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4,
                                    "5" = 5, "6" = 6, "7" = 7)),
        textOutput(ns("a15")),
        radioButtons(ns("q16"), "Anxiety", inline=TRUE,
                     choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4,
                                    "5" = 5, "6" = 6, "7" = 7)),
        textOutput(ns("a16")),
        radioButtons(ns("q17"), "Guilt feeling", inline=TRUE,
                     choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4,
                                    "5" = 5, "6" = 6, "7" = 7)),
        textOutput(ns("a17")),
        radioButtons(ns("q18"), "Tension", inline=TRUE,
                     choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4,
                                    "5" = 5, "6" = 6, "7" = 7)),
        textOutput(ns("a18")),
        radioButtons(ns("q19"), "Mannerisms and posturing", inline=TRUE,
                     choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4,
                                    "5" = 5, "6" = 6, "7" = 7)),
        textOutput(ns("a19")),
        radioButtons(ns("q20"), "Depression", inline=TRUE,
                     choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4,
                                    "5" = 5, "6" = 6, "7" = 7)),
        textOutput(ns("a20")),
        radioButtons(ns("q21"), "Motor retardation", inline=TRUE,
                     choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4,
                                    "5" = 5, "6" = 6, "7" = 7)),
        textOutput(ns("a21")),
        radioButtons(ns("q22"), "Uncooperativeness", inline=TRUE,
                     choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4,
                                    "5" = 5, "6" = 6, "7" = 7)),
        textOutput(ns("a22")),
        radioButtons(ns("q23"), "Unusual thought content", inline=TRUE,
                     choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4,
                                    "5" = 5, "6" = 6, "7" = 7)),
        textOutput(ns("a23")),
        radioButtons(ns("q24"), "Disorientation", inline=TRUE,
                     choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4,
                                    "5" = 5, "6" = 6, "7" = 7)),
        textOutput(ns("a24")),
        radioButtons(ns("q25"), "Poor attention", inline=TRUE,
                     choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4,
                                    "5" = 5, "6" = 6, "7" = 7)),
        textOutput(ns("a25")),
        radioButtons(ns("q26"), "Lack of judgement and insight", inline=TRUE,
                     choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4,
                                    "5" = 5, "6" = 6, "7" = 7)),
        textOutput(ns("a26")),
        radioButtons(ns("q27"), "Disturbance of volition", inline=TRUE,
                     choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4,
                                    "5" = 5, "6" = 6, "7" = 7)),
        textOutput(ns("a27")),
        radioButtons(ns("q28"), "Poor impulse control", inline=TRUE,
                     choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4,
                                    "5" = 5, "6" = 6, "7" = 7)),
        textOutput(ns("a28")),
        radioButtons(ns("q29"), "Preoccupation", inline=TRUE,
                     choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4,
                                    "5" = 5, "6" = 6, "7" = 7)),
        textOutput(ns("a29")),
        radioButtons(ns("q30"), "Active social avoidance", inline=TRUE,
                     choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4,
                                    "5" = 5, "6" = 6, "7" = 7)),
        textOutput(ns("a30"))
    ),
    actionButton(ns("submit"), label="Submit", class="pull-right btn-info"),
    actionButton(ns("show"), label="Show answers", class="pull-right btn-info"),
    actionButton(ns("save"), label="Save responses", class="pull-right btn-info")
  )
}

trainMe <- function(input, output, session, pool) {
  
  ncol <- pool %>% tbl("values") %>% as.data.frame %>% ncol()
  entryValues <- data.frame(matrix(NA, 1, ncol))
  colnames(entryValues) <- pool %>% tbl("values") %>% as.data.frame %>% colnames()
  
  outputAnswers <- function(bool, range) {
    if (bool) {
      return(renderText({"Correct!"}))
    } else {
      return(renderText({paste("Incorrect! Answer is: ", paste(range))}))
    }
  }
  
  observeEvent(input$submit, {
    
    if (is.na(input$rater)) {
      showModal(modalDialog(
        title = "Input Error: Empty",
        "Please enter a rater id",
        easyClose = TRUE, footer = NULL
      ))
    } else if (!is.integer(input$rater)){
      showModal(modalDialog(
        title = "Input Error: Incorrect Input Type",
        "Please enter rater id as integer",
        easyClose = TRUE, footer = NULL
      ))
    } else {
      
      nam <- paste0("q", seq(1,30))
      values <- as.numeric(unlist(reactiveValuesToList(input)[nam]))
      expert <- c(4, 2, 5, 1, 2, 4, 2, 5, 4, 4, 3, 3,
                  4, 2, 3, 4, 5, 3, 3, 3, 4, 3, 3, 1,
                  2, 2, 3, 1, 3, 1)
      
      marks <- rep(NA, length(values))
      bool <- rep(NA, length(values))
      ranges <- list()
      for (i in 1:length(values)) {
        if (expert[i] == 1) {
          rang <- seq(expert[i], expert[i]+1)
        } else if (expert[i] == 7) {
          rang <- seq(expert[i]-1, expert[i])
        } else {
          rang <- seq(expert[i]-1, expert[i]+1)
        }
        ranges <- append(ranges, list(rang))
        if (values[i] %in% rang) {
          marks[i] <- 1
          bool[i] <- TRUE
        } else {
          marks[i] <- 0
          bool[i] <- FALSE
        }
      }
      
      
      pos <- sum(marks[1:7])
      neg <- sum(marks[8:14])
      gen <- sum(marks[15:30])
      result <- "FAIL"
      if (pos >= 5 & neg >= 5 & gen >= 10) {
        result <- "PASS"
      }
      
      tryCatch(
        {
          showModal(modalDialog(
            title = paste("Result: ", result),
            paste("Positive: ", pos, " out of 7","\n", "Negative: ", neg, " out of 7\n",
                  "General", gen, " out of 16"),
            easyClose = TRUE, footer = NULL
          ))
        },
        error=function(e) {
          print(e)
          showModal(modalDialog(
            title = "UNIQUE contraint failed",
            "Given rater id already exists.",
            easyClose = TRUE, footer = NULL
          ))
        })
      # 
      # showModal(modalDialog(
      #   title = paste("Result: ", result),
      #   paste("Positive: ", pos, " out of 7","\n", "Negative: ", neg, " out of 7\n",
      #         "General", gen, " out of 16"),
      #   easyClose = TRUE, footer = NULL
      # ))
      # 
      
      
      observeEvent(input$show, {
        output$a1 <- outputAnswers(bool[1], ranges[1])
        output$a2 <- outputAnswers(bool[2], ranges[2])
        output$a3 <- outputAnswers(bool[3], ranges[3])
        output$a4 <- outputAnswers(bool[4], ranges[4])
        output$a5 <- outputAnswers(bool[5], ranges[5])
        output$a6 <- outputAnswers(bool[6], ranges[6])
        output$a7 <- outputAnswers(bool[7], ranges[7])
        output$a8 <- outputAnswers(bool[8], ranges[8])
        output$a9 <- outputAnswers(bool[9], ranges[9])
        output$a10 <- outputAnswers(bool[10], ranges[10])
        output$a11 <- outputAnswers(bool[11], ranges[11])
        output$a12 <- outputAnswers(bool[12], ranges[12])
        output$a13 <- outputAnswers(bool[13], ranges[13])
        output$a14 <- outputAnswers(bool[14], ranges[14])
        output$a15 <- outputAnswers(bool[15], ranges[15])
        output$a16 <- outputAnswers(bool[16], ranges[16])
        output$a17 <- outputAnswers(bool[17], ranges[17])
        output$a18 <- outputAnswers(bool[18], ranges[18])
        output$a19 <- outputAnswers(bool[19], ranges[19])
        output$a20 <- outputAnswers(bool[20], ranges[20])
        output$a21 <- outputAnswers(bool[21], ranges[21])
        output$a22 <- outputAnswers(bool[22], ranges[22])
        output$a23 <- outputAnswers(bool[23], ranges[23])
        output$a24 <- outputAnswers(bool[24], ranges[24])
        output$a25 <- outputAnswers(bool[25], ranges[25])
        output$a26 <- outputAnswers(bool[26], ranges[26])
        output$a27 <- outputAnswers(bool[27], ranges[27])
        output$a28 <- outputAnswers(bool[28], ranges[28])
        output$a29 <- outputAnswers(bool[29], ranges[29])
        output$a30 <- outputAnswers(bool[30], ranges[30])
      }) 
    }
    
  })
  
  
  findResult <- function() {
    nam <- paste0("q", seq(1,30))
    values <- as.numeric(unlist(reactiveValuesToList(input)[nam]))
    expert <- c(4, 2, 5, 1, 2, 4, 2, 5, 4, 4, 3, 3,
                4, 2, 3, 4, 5, 3, 3, 3, 4, 3, 3, 1,
                2, 2, 3, 1, 3, 1)
    
    marks <- rep(NA, length(values))
    bool <- rep(NA, length(values))
    ranges <- list()
    for (i in 1:length(values)) {
      if (expert[i] == 1) {
        rang <- seq(expert[i], expert[i]+1)
      } else if (expert[i] == 7) {
        rang <- seq(expert[i]-1, expert[i])
      } else {
        rang <- seq(expert[i]-1, expert[i]+1)
      }
      ranges <- append(ranges, list(rang))
      if (values[i] %in% rang) {
        marks[i] <- 1
        bool[i] <- TRUE
      } else {
        marks[i] <- 0
        bool[i] <- FALSE
      }
    }
    
    
    pos <- sum(marks[1:7])
    neg <- sum(marks[8:14])
    gen <- sum(marks[15:30])
    result <- "FAIL"
    if (pos >= 5 & neg >= 5 & gen >= 10) {
      result <- "PASS"
    }
    
    return(result)
  } 
  
  
  observeEvent(input$save, {
    tryCatch(
      {
        nam <- paste0("q", seq(1,30))
        values <- as.numeric(unlist(reactiveValuesToList(input)[nam]))
        expert <- c(4, 2, 5, 1, 2, 4, 2, 5, 4, 4, 3, 3,
                    4, 2, 3, 4, 5, 3, 3, 3, 4, 3, 3, 1,
                    2, 2, 3, 1, 3, 1)
        
        marks <- rep(NA, length(values))
        bool <- rep(NA, length(values))
        ranges <- list()
        for (i in 1:length(values)) {
          if (expert[i] == 1) {
            rang <- seq(expert[i], expert[i]+1)
          } else if (expert[i] == 7) {
            rang <- seq(expert[i]-1, expert[i])
          } else {
            rang <- seq(expert[i]-1, expert[i]+1)
          }
          ranges <- append(ranges, list(rang))
          if (values[i] %in% rang) {
            marks[i] <- 1
            bool[i] <- TRUE
          } else {
            marks[i] <- 0
            bool[i] <- FALSE
          }
        }
        
        
        pos <- sum(marks[1:7])
        neg <- sum(marks[8:14])
        gen <- sum(marks[15:30])
        result <- "FAIL"
        if (pos >= 5 & neg >= 5 & gen >= 10) {
          result <- "PASS"
        }
        
        entryValues[1] <- as.integer(input$rater)
        if (input$lang == "1") {
          entryValues[2] <- "E"
        } else if (input$lang == "2") {
          entryValues[2] <- "F"
        } else if (input$lang == "3") {
          entryValues[2] <- "I"
        }
        
        entryValues[3:length(entryValues)] <- values
        
        
        dbAppendTable(pool, "values", entryValues)
        
        showModal(modalDialog(
          title = "Success! Saved to the database!",
          easyClose = TRUE, footer = NULL
        ))
      },
      error=function(e) {
        showModal(modalDialog(
          title = "UNIQUE contraint failed",
          str(e),
          easyClose = TRUE, footer = NULL
        ))
      })
  })
  
}