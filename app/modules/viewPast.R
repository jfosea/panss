viewPastUI <- function(id) {
  ns <- NS(id)
  tagList(h2("Look at past results"),
          box(title = "Workshop Results", width = 20, solidHeader = TRUE, status = "primary",
              tableOutput(ns("res"))
          )
        )
}

viewPast <- function(input, output, session, pool) {
  
  output$res <- renderTable({
    a <- pool %>% tbl("values")
    fdata <- as.data.frame(a)
    for (i in 2:ncol(fdata)) {
      rownum <- which(fdata[,i] == 0)
      if (isTRUE(rownum > 0)) {
        fdata[rownum, i] <- NA
      }
    }
    
    expert <- c(0,0, 4, 2, 5, 1, 2, 4, 2, 5, 4, 4, 3, 3,
                4, 2, 3, 4, 5, 3, 3, 3, 4, 3, 3, 1,
                2, 2, 3, 1, 3, 1)
    
    scores <- matrix(0, nrow=nrow(fdata), ncol=ncol(fdata))
    for (i in 2:nrow(fdata)) {
      for (j in 3:ncol(fdata)) {
        range <- seq(expert[j]-1, expert[j]+1)
        if (fdata[i,j] %in% range) {
          scores[i,j] <- 1
        } 
      }
    }
    
    for (j in 3:ncol(fdata)) {
      range <- seq(expert[j]-1, expert[j]+1)
      if (fdata[i,j] %in% range) {
        scores[i,j] <- 1
      } 
    }
    
    result <- rep(NA, 1, nrow(fdata))
    for (i in 2:nrow(fdata)) {
      pos <- sum(scores[i,3:9])
      neg <- sum(scores[i,10:16])
      gen <- sum(scores[i, 17:32])
      if (isTRUE(pos >= 5 & neg >= 5 & gen >= 10)) {
        result[i] <- "PASS"
      } else {
        result[i] <- "FAIL"
      }
    }
    
    final <- data.frame(User=fdata["RATER"], Language=fdata["LANG"], Result=result)
    as_tibble(final)
 }) 
  
}