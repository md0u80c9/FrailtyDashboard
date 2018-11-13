
filterTabsInput <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    h3("Filter by"),
    uiOutput(ns("filter_boxes"))
  )
}

# Module server function
filterTabs <- function(input, output, session, source_data) {

  filter_box_columns <- function() {
    factors <- unlist(purrr::map(names(source_data),
          .f = function(x) is.factor(source_data[[x]])))
    return(names(source_data)[factors])
  }
    
  output$filter_boxes <- renderUI({
      purrr::map(filter_box_columns(),
      .f = function(x) box(title = x,
                           collapsible = TRUE,
                           collapsed = TRUE,
                           width = NULL,
        checkboxGroupInput(
          inputId = session$ns(x),
          label = "Show",
          choices = levels(
            source_data[[x]]),
          selected = levels(
            source_data[[x]])
        )))
  })

  filtered_source_data <- reactive({
    filters <- purrr::map(filter_box_columns(),
      .f = function(x) {
        enquo_x <- enquo(x)
        rlang::expr(.data[[x]] %in% input[[!!enquo_x]])
      })

    dplyr::filter(source_data, !!! filters)
  })

  return(filtered_source_data)
}
