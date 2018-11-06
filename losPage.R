# Length of stay Shiny frailty dashboard page

losPageInput <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    fluidRow(
      uiOutput(ns("title"))
    ),
    fluidRow(
      column(width = 9,
          box(title = "Length of stay", width = NULL,
            plotOutput(ns("chart"), height = 550)
          )
      ),
      column(width = 3,
       box(width = NULL,
            radioButtons(inputId = ns("lengthofstay_charttype"),
                         label = "Separate length of stay",
                         choices = c("Show all",
                                     "By Year",
                                     "By residence at admission"))
        ),
        box(width = NULL,
            uiOutput(ns("filter_controls"))
        ),
        box(width = NULL,
            uiOutput(ns("extra_controls"))
        )
      )
    )
  )
}

# Module server function
losPage <- function(input, output, session, source_data) {
  
  los_data <- reactive({
    los_data <- dplyr::filter(source_data,
                                 !is.na(.data[["LOS"]]))
    if (!is.null(input$lengthofstay_typefilter)) {
      # Now filter the data so only those with a valid LOS and whose data is
      # selected are shown
      
      los_data <- dplyr::filter(los_data,
        .data[["place_of_residence"]] %in% input$lengthofstay_typefilter)
    }
    los_data$place_of_residence <- forcats::fct_drop(los_data$place_of_residence)
    return(los_data)
  })

  output$title <- renderUI({
    titlePanel(glue::glue("Length of stay (from 
          {earliest} to {latest})",
      earliest = format(min(los_data()[['Date/Time of Referral']]),
                        format = '%d %b %Y'),
      latest = format(max(los_data()[['Date/Time of Referral']]),
                      format = '%d %b %Y'))
    )
  })

  output$filter_controls <- renderUI({
    checkboxGroupInput(inputId = session$ns("lengthofstay_typefilter"),
                       label = "Show residence type",
                       choices = levels(source_data$place_of_residence),
                       selected = levels(source_data$place_of_residence))
  })
  
  output$extra_controls <- renderUI({
    extra_options <- c("Show confidence interval bands",
                       "Exclude deaths during the admission")
    checkboxGroupInput(inputId = session$ns("lengthofstay_extra"),
                       label = "Additional display options",
                       choices = extra_options,
                       selected = "")
  })
  
  output$chart <- renderPlot({
    fit <- survfit(Surv(LOS, event) ~ place_of_residence, data = los_data())
    survminer::ggsurvplot(
      fit, 
      data = los_data(),
      size = 1,                 # change line size
      conf.int = "Show confidence interval bands" %in% input$lengthofstay_extra,
      xlim = c(0, 21),          # Limit at 21 days ('super stranded' patients)
      xlab = "Time from admission in days",   # customize X axis label.
      break.time.by = 1,     # break X axis in time intervals by 500.
      ylab = "Proportion of patients remaining in hospital",
      risk.table = TRUE,        # Add risk table
      risk.table.title = "Number of patients",
      risk.table.col = "strata",# Risk table color by groups
      legend.labs = levels(los_data()[["place_of_residence"]]),
      #          c("All"),    # Change legend labels
      risk.table.height = 0.25, # Useful to change when you have multiple groups
      ggtheme = theme_bw()      # Change ggplot2 theme
    )
  })
}
