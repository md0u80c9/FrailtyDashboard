
source("filter_tabs.R")

admissionActivityInput <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 9,
        box(title = "Time of day for referrals",
          width = NULL,
          p(glue::glue(
            "The proportion of frailty patients arriving in the ED
            each hour are shown in red. The proportion of patients
            referred each hour are shown in blue. In an ideal system,
            it would be expected that the blue line would mimic the
            red line with a delay of a couple of hours. The purpose
            of this chart is to identify times of peak activity.")),
          plotOutput(ns("arrival_to_referral_plot"), height = 500)
        ),
        box(title = "Day of week of arrival and referrals",
          width = NULL,
          p(glue::glue(
            "Frailty patients should present (more or less
            uniformly across the working week: each bar would be
            expected to be uniform. Differences in the arrivals bar
            may represent issues in identifying frailty patients for
            referral; differences between blue bars may indicate
            referrals being made on a different day to the day of
            admission.")),
          plotOutput(ns("weekday_referrals_plot"), height = 300)
        ),
        box(title = "Daily referral activity heatmap",
          width = NULL,
          p(glue::glue(
            "The heatmaps give us an indication of changes over
            time; and may also be useful to identify patterns of
            activity which may help workforce planning - for
            example around weekends, bank holidays, and 
            seasonality.")),
          plotOutput(ns("daily_arrivals_plot"), height = 400),
          plotOutput(ns("daily_referrals_plot"), height = 400),
          plotOutput(ns("daily_admissions_plot"), height = 400)
        )
      ),
      column(width = 3,
        filterTabsInput(ns("filterTabs"))
      )
    )
  )
}

# Module server function
admissionActivity <- function(input, output, session, source_data) {

  source_data <- dplyr::select(source_data,
    "CCG",
    "mode_of_admission",
    "Date/Time of Referral",
    "ed_arrival_datetime",
    "arrival_to_referral_mins",
    "Date/Time of Admission to 1A")
  
  source_data <- dplyr::filter(source_data,
    !is.na(.data[["Date/Time of Referral"]]))
  
  source_data <- dplyr::mutate(source_data,
    "referral_hour" = as.factor(
      format(.data[['Date/Time of Referral']], format = '%H')),
    "ed_arrival_hour" = as.factor(
      format(.data[["ed_arrival_datetime"]], format = '%H')),
    "referral_weekday" = as.factor(
      format(.data[['Date/Time of Referral']], format = '%u')),
    "ed_arrival_weekday" = as.factor(
      format(.data[["ed_arrival_datetime"]], format = '%u'))
  )

  filtered_data <- callModule(filterTabs,
                              "filterTabs",
                              source_data)

  referral_data <- reactive({
    referral_data <- dplyr::transmute(filtered_data(),
      "hour" = .data[["referral_hour"]],
      "arrival_to_referral_mins" =
        .data[["arrival_to_referral_mins"]])
    referral_data <- dplyr::group_by(referral_data,
                                     hour)
    referral_data <- dplyr::summarise(referral_data,
      "n" = n(),
      "median_arrival_to_referral_mins" =
        median(.data[["arrival_to_referral_mins"]], na.rm = TRUE))
    referral_data$hour <- as.integer(referral_data$hour) * 100
    all_admissions <- sum(referral_data$n)
    referral_data$p <- referral_data$n / all_admissions * 100
    return(referral_data)
  })

  ed_arrival_data <- reactive({
    ed_arrival_data <- dplyr::transmute(filtered_data(),
                                 "hour" = .data[["ed_arrival_hour"]])
    ed_arrival_data <- dplyr::group_by(ed_arrival_data,
                                       hour)
    ed_arrival_data <- dplyr::summarise(ed_arrival_data,
                                       "n" = n())
    ed_arrival_data$hour <- as.integer(ed_arrival_data$hour) * 100
    all_admissions <- sum(ed_arrival_data$n)
    ed_arrival_data$p <- ed_arrival_data$n / all_admissions * 100
    return(ed_arrival_data)
  })

  admission_data <- reactive({
    referrals <- dplyr::mutate(referral_data(),
                               series = "Referral time")
    ed_arrival <- dplyr::mutate(ed_arrival_data(),
                               series = "ED arrival time")
    admission_data <- dplyr::bind_rows(referrals,
                                       ed_arrival)
  })
  
  weekday_data <- reactive({
    if (length(filtered_data()[[1]]) == 0) {
      return(NULL)
    }
    arrival_weekdays <- dplyr::transmute(filtered_data(),
      "weekday" = .data[["ed_arrival_weekday"]])
    referral_weekdays <- dplyr::transmute(filtered_data(),
      "weekday" = .data[["referral_weekday"]])
    
    arrivals <- dplyr::summarise(
      dplyr::group_by(arrival_weekdays, weekday),
      "n" = n(),
      "series" = "Arrival day")
    referrals <- dplyr::summarise(
     dplyr::group_by(referral_weekdays, weekday),
                     "series" = "Referral day",
                     "n" = n())
    # Make referrals negative so they plot below the axis
    referrals$n <- referrals$n * -1
    
    weekdays <- dplyr::bind_rows(arrivals, referrals)
    weekdays$weekday <- as.character(weekdays$weekday)
    weekdays <- dplyr::filter(weekdays,
                  !is.na(weekday))
    weekdays$weekday <- as.factor(weekdays$weekday)
    weekdays <- dplyr::mutate(weekdays,
      weekday = factor(.data[["weekday"]],
                       labels = c("Monday", "Tuesday", "Wednesday",
                                  "Thursday", "Friday", "Saturday",
                                  "Sunday"))
    )
    return(weekdays)
  })

  
  daily_arrivals <- reactive({
    daily_data <- dplyr::transmute(filtered_data(),
                                   "arrival_day" = as.Date(.data[["ed_arrival_datetime"]]))
    daily_data <- dplyr::group_by(daily_data, arrival_day)
    daily_data <- dplyr::summarise(daily_data,
                                   "n" = n())
    daily_data <- dplyr::filter(daily_data,
                                arrival_day >= as.Date("01/01/2016", "%d/%m/%Y"))
    return(daily_data)
  })
  
  
  daily_referrals <- reactive({
    daily_data <- dplyr::transmute(filtered_data(),
      "referral_day" = as.Date(.data[['Date/Time of Referral']]))
    daily_data <- dplyr::group_by(daily_data, referral_day)
    daily_data <- dplyr::summarise(daily_data,
                     "n" = n())
    daily_data <- dplyr::filter(daily_data,
                    referral_day >= as.Date("01/01/2016", "%d/%m/%Y"))
    return(daily_data)
  })
  
  daily_admissions <- reactive({
    daily_data <- dplyr::transmute(filtered_data(),
      "admission_day" = as.Date(.data[["Date/Time of Admission to 1A"]]))
    daily_data <- dplyr::group_by(daily_data, admission_day)
    daily_data <- dplyr::summarise(daily_data,
                                   "n" = n())
    daily_data <- dplyr::filter(daily_data,
                                admission_day >= as.Date("01/01/2016", "%d/%m/%Y"))
    return(daily_data)
  })
  
  output$arrival_to_referral_plot <- renderPlot({
    admission <- admission_data()
    frequency <- ggplot2::ggplot(data = admission,
                    aes(x = .data[["hour"]],
                        y = .data[["p"]],
                        group = .data[["series"]],
                        colour = .data[["series"]])) +
      geom_line() +
      xlab("Hour of the day") +
      ylab("%age of referrals") +
      ggtitle("Presenting time of day for referrals") +
      scale_x_continuous(breaks = seq(0, 2400, by = 300)) +
      theme_bw() +
      theme(legend.position = "top")
    
    # Now drop the admission time series and just use the referral
    # time series which has a median_arrival_to_referral_mins set.
    admission <- dplyr::filter(admission,
                               .data[["series"]] == "Referral time")
    
    referral_time <- ggplot2::ggplot(data = admission,
      aes(x = .data[["hour"]],
          y = .data[["median_arrival_to_referral_mins"]],
          group = .data[["series"]],
          colour = .data[["series"]])) +
      geom_line() +
      xlab("Hour of the day") +
      ylab("Median hours from arrival to referral") +
      ggtitle(glue::glue(
        "Median waiting time from arrival to referral in hours, ",
        "determined by hour of arrival")) +
      scale_x_continuous(breaks = seq(0, 2400, by = 300)) +
      scale_y_continuous(breaks = seq(0, 48, by = 4)) +
      theme_bw() +
      theme(legend.position = "top")
    ggpubr::ggarrange(frequency, referral_time,
                      ncol = 1, nrow = 2)
  })
  
  output$weekday_referrals_plot <- renderPlot({
    weekday <- weekday_data()
    if (length(weekday$n) > 0) {
      ggplot2::ggplot(data = weekday,
        aes(x = .data[["weekday"]],
          y = .data[["n"]],
#          group = .data[["series"]],
          fill = .data[["series"]])) +
        geom_bar(stat = "identity", position = "identity") +
        xlab("Day of the week") +
        ylab("Number of patients") +
        ggtitle("Comparison of day of week arrival versus day of week referral of frailty cohort") +
        theme_bw() +
        theme(legend.position = "top")
    }
    else {
      geom_blank()
    }
  })
  
  output$daily_referrals_plot <- renderPlot({
    referrals <- daily_referrals()
    if (length(referrals$referral_day > 0)) {
      calendarHeat(referrals$referral_day,
                 referrals$n, color = "bluehue",
                 varname = "number of frailty referrals per day")
    }
    else {
      geom_blank()
    }
  })
  
  output$daily_arrivals_plot <- renderPlot({
    arrivals <- daily_arrivals()
    if (length(arrivals$arrival_day > 0)) {
      calendarHeat(arrivals$arrival_day,
                 arrivals$n, color = "redhue",
                 varname = "frailty patients arriving in ED per day")
    }
    else {
      geom_blank()
    }
  })
  
  output$daily_admissions_plot <- renderPlot({
    admissions <- daily_admissions()
    if (length(admissions$admission_day > 0)) {
      calendarHeat(admissions$admission_day,
                 admissions$n, color = "greenhue",
                 varname = "Frailty unit admissions per day")
  }
    else {
      geom_blank()
    }
  })
}
