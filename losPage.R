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
          ),
          box( width = NULL, title =
                "Association between delayed admission and length of stay",
              plotOutput(ns("admission_delay_on_los"), height = 550)
          )
      ),
      column(width = 3,
        box(width = NULL,
          uiOutput(ns("charttype"))
        ),
        tags$div(id = 'select_factors_to_plot'),
        uiOutput(ns("filter_controls")),
        box(width = NULL,
          uiOutput(ns("extra_controls"))
        )
      )
    )
  )
}

# Module server function
losPage <- function(input, output, session, source_data) {
  
  filtered_source_data <- dplyr::filter(source_data,
                                        !is.na(.data[["LOS"]]))
  filtered_source_data <- dplyr::select(filtered_source_data,
                                        "LOS",
                                        "event",
                                        "CCG",
                                        "place_of_residence",
                                        "mode_of_admission",
                                        "Date/Time of Referral",
                                        "source_of_admission",
                                        "discharge_destination",
                                        "arrival_to_admission_mins",
                                        "Hospital number")
  filtered_source_data <- dplyr::mutate(filtered_source_data,
    "year" = as.factor(format(.data[['Date/Time of Referral']],
                    format = '%Y')))
                                        
  los_data <- reactive({
#    if (input$charttype == names(charttype_fields)[1]) {
#      los_data <- dplyr::mutate(filtered_source_data,
#                                "chart_field" = TRUE)
#    } else 
    {
      los_data <- dplyr::mutate(filtered_source_data,
        "chart_field" =
          !!sym(charttype_fields[[input$charttype]]))
      if (!is.null(input$lengthofstay_typefilter)) {
        # Now filter the data so only those with a valid LOS and whose data is
        # selected are shown

        los_data <- dplyr::filter(los_data,
          .data[["chart_field"]] %in% input$lengthofstay_typefilter)
      }
    }
    los_data$chart_field <- forcats::fct_drop(los_data$chart_field)
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

  charttype_fields <- c(
#    "Show all" = "",
#    "By Year" = "referral_year",
    "By residence at admission" = "place_of_residence",
    "By discharge destination" = "discharge_destination",
    "By mode of admission" = "mode_of_admission",
    "By source of admission" = "source_of_admission",
    "By CCG" = "CCG",
    "By year" = "year"
  )
  
  output$charttype <- renderUI({
    radioButtons(inputId = session$ns("charttype"),
                 label = "Separate length of stay",
                 choices = names(charttype_fields))
  })

  output$filter_controls <- renderUI({
    if (!is.null(input$charttype)) {
      if (length(levels(filtered_source_data[[
        charttype_fields[input$charttype]]])) > 0) {
        box(width = NULL,
          checkboxGroupInput(
          inputId = session$ns("lengthofstay_typefilter"),
          label = "Show",
          choices = levels(
            filtered_source_data[[charttype_fields[input$charttype]]]),
          selected = levels(
            filtered_source_data[[charttype_fields[input$charttype]]]))
          )
        }
      }
  })

  output$extra_controls <- renderUI({
    extra_options <- c("Show confidence interval bands",
                       "Exclude deaths during the admission")
    checkboxGroupInput(inputId = session$ns("lengthofstay_extra"),
                       label = "Additional charting options",
                       choices = extra_options,
                       selected = "")
  })

  output$chart_box <- renderUI({
    box(title = "Length of stay", width = NULL,
        plotOutput(ns("chart"), height = 550)
    )
  })
  
  output$chart <- renderPlot({
    fit <- survfit(Surv(LOS, event) ~ chart_field,
                   data = los_data())

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
      legend.labs = levels(los_data()[["chart_field"]]),
      #          c("All"),    # Change legend labels
      risk.table.height = 0.25, # Useful to change when you have multiple groups
      ggtheme = theme_bw()      # Change ggplot2 theme
    )
  })
  
  output$admission_delay_on_los <- renderPlot({
    ggplot(filtered_source_data, aes(x=arrival_to_admission_mins, y=LOS)) + 
      geom_point() +
      scale_x_continuous(breaks = seq(0, 72,
                                      by = 6),
                         limits = c(0, 72)) +
      scale_y_continuous(breaks = seq(0, 21, by = 1),
                         limits = c(0, 21)) +
      xlab("Time from arrival to 1A admission (hours)") +
      ylab("Length of stay (days)") +
      geom_smooth(method=lm)
  })
}
