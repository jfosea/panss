appendUI <- function(id) {
  ns <- NS(id)
  tagList(h2("Append!"),
          actionButton(ns("append"), label="Save responses", class="pull-right btn-info"))
}

appendDB <- function(input, output, session, pool) {
  
  entryValues <- data.frame(matrix(100, 1, ncol))
  colnames(entryValues) <- pool %>% tbl("values") %>% as.data.frame %>% colnames()
  
  
  observeEvent(input$append, {dbAppendTable(pool, "values", entryValues)})
}