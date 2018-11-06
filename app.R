#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyBS)
library(dplyr)
library(ggplot2)
library(tibbletime)
library(shinydashboard)
library(survival)
library(survminer)

source("read_frailty_data.R")
source("losPage.R")

header <-  dashboardHeader(title = "STHK Frailty Dashboard")

## Sidebar content

ui <- dashboardPage(
    dashboardHeader(title = "Frailty Dashboard"),
    dashboardSidebar(
      sidebarMenu(id = "mainsidebar",
        menuItem(text = "Admission", tabName = "admission", icon = NULL),
        menuItem(text = "Length of stay", tabName = "los_page", icon = NULL)
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "admission",
        # Boxes need to be put in a row (or column)
        fluidRow(
          titlePanel(glue::glue("Frailty service activity (from 
            {format(min(frailty_data[['Date/Time of Referral']]), format = '%d %b %Y')} to 
            {format(max(frailty_data[['Date/Time of Referral']]), format = '%d %b %Y')})"))
        ),
        fluidRow(
            box(title = "Type of residence",
              width = 10,
              plotOutput("residence", height = 250)),
            box(width = 2,
              radioButtons(inputId = "residenceRadios",
                           label = "Y axis shows",
                           choices = c("Number of referrals",
                                       "Proportion of referrals")))
        ),
        fluidRow(
          box(title = "Admission mode", width = 10,
              plotOutput("admission_mode", #height = 250
                         ))
        )
        ),
        tabItem(tabName = "los_page",
          losPageInput("los_page")
        )
      )
    )
)

server <- function(input, output) {
  
    frailty_data <-
      read_frailty_data("../FrailtyTest/FinalTableOutput.csv")
    
    frailty_data <- dplyr::group_by(frailty_data,
                                    .data[["ReferralPeriod"]],
                                    .data[["Place of Residence"]])

    residence_type <- dplyr::summarise(frailty_data,
                                       "Patients" = n())
    frailty_data <- dplyr::ungroup(frailty_data)
    frailty_data <- dplyr::group_by(frailty_data,
                                    .data[["ReferralPeriod"]])
    patients_per_month <- dplyr::summarise(frailty_data,
                                           "Total" = n())
    frailty_data <- dplyr::ungroup(frailty_data)
    
    residence_type = dplyr::left_join(residence_type,
                                      patients_per_month,
                                      by = "ReferralPeriod")
    
    residence_type <- dplyr::mutate(residence_type,
      "proportionPatients" = .data[["Patients"]] /
        .data[["Total"]] * 100)

    output$residence <- renderPlot({
      if (input$residenceRadios == "Number of referrals") {
        y_axis = "Patients"
      } else {
        y_axis = "proportionPatients"
      }
      ggplot2::ggplot(residence_type,
             ggplot2::aes(x = .data[["ReferralPeriod"]],
                 y = .data[[y_axis]],
                 fill = .data[["Place of Residence"]])) + 
        ggplot2::geom_area(alpha = 0.4 , size = 0.2, colour = "black") +
        ggplot2::labs(xlab = "Month of admission",
                    ylab = input$residenceRadios)
    })

# Do the same with the referral source (NB needs to be modularised later)
    
    frailty_data <- dplyr::group_by(frailty_data,
                                    .data[["ReferralPeriod"]],
                                    .data[["Mode of admission"]])
    
    admission_mode <- dplyr::summarise(frailty_data,
                                       "Patients" = n())
    frailty_data <- dplyr::ungroup(frailty_data)

    admission_mode = dplyr::left_join(admission_mode,
                                      patients_per_month,
                                      by = "ReferralPeriod")
    
    admission_mode <- dplyr::mutate(admission_mode,
                                    "proportionPatients" = .data[["Patients"]] /
                                      .data[["Total"]] * 100)
    
    output$admission_mode <- renderPlot({
      if (input$residenceRadios == "Number of referrals") {
        y_axis = "Patients"
      } else {
        y_axis = "proportionPatients"
      }
      ggplot2::ggplot(admission_mode,
                      ggplot2::aes(x = .data[["ReferralPeriod"]],
                                   y = .data[[y_axis]],
                                   fill = .data[["Mode of admission"]])) + 
        ggplot2::geom_area(alpha = 0.4 , size = 0.2, colour = "black") +
        ggplot2::labs(xlab = "Month of admission",
                      ylab = input$residenceRadios)
    })
    
    
    los_page_module <- callModule(losPage, "los_page", frailty_data)
}

shinyApp(ui, server)
