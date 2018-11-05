
read_frailty_data <- function(filename) {
  frailty_data <- readr::read_csv(
    file = filename,
    col_types = readr::cols(
      .default = readr::col_character(),
      ID = readr::col_double(),
      "Hospital number" = readr::col_character(),
      "Place of Residence" = readr::col_factor(
        levels = c("Own home",
                   "Nursing home",
                   "Sheltered accomodation",
                   "Residential",
                   "Respite",
                   "Supported Living",
                   "Unknown"),
        include_na = TRUE),
      "Mode of admission" = readr::col_factor(
        levels = c("Case finding",
                   "Case Finding",
                   "Direct Admission",
                   "Direct admission",
                   "E-Handover",
                   "Referral",
                   "Transfer",
                   "Unknown"),
        include_na = TRUE),
        
      #            "Source of admission" = readr::col_factor(),
      #            "Mode of admission" = readr::col_factor(),
      "Date/Time of Referral" = readr::col_datetime(
        format = "%d/%m/%Y %H:%M:%S"),
      "Date/Time of Admission to 1A" = readr::col_datetime(
        format = "%d/%m/%Y %H:%M:%S"),
      Fall = readr::col_logical(),
      Cognition = readr::col_logical(),
      Continence = readr::col_logical(),
      CMS = readr::col_logical(),
      Nutrition = readr::col_logical(),
      MCM = readr::col_logical(),
      "Bed Manager Override" = readr::col_logical(),
      "CFS" = readr::col_integer(),
      "DNA CPR" = readr::col_logical(),
      "ACP" = readr::col_logical(),
      "ACP Info" = readr::col_logical(),
      "Arrival to Discharge (Days)" = readr::col_number()
    ))
  
  frailty_data <- dplyr::mutate(frailty_data,
    "Date/Time of Referral" = 
       dplyr::if_else(is.na(.data[["Date/Time of Referral"]]),
                      .data[["Date/Time of Admission to 1A"]],
                      .data[["Date/Time of Referral"]]),
  )
  frailty_data <- dplyr::rename(frailty_data, "LOS" = "Arrival to Discharge (Days)")
  
  frailty_data[["Mode of admission"]][frailty_data[["Mode of admission"]] == "Case Finding"] <- "Case finding"
  frailty_data[["Mode of admission"]][frailty_data[["Mode of admission"]] == "Direct Admission"] <- "Direct admission"
  frailty_data[["Mode of admission"]][is.na(frailty_data[["Mode of admission"]])] <- "Unknown"
  forcats::fct_drop(frailty_data[["Mode of admission"]])
  
  frailty_data <- dplyr::filter(frailty_data,
                                !is.na(.data[["Date/Time of Referral"]]))
  frailty_data <- tibbletime::as_tbl_time(frailty_data,
                                          index = "Date/Time of Referral")
  
  frailty_data <- dplyr::mutate(
    frailty_data,
    ReferralPeriod = tibbletime::collapse_index(
      as.Date(.data[["Date/Time of Referral"]]),
      period = "monthly",
      side = "start",
      clean = TRUE))
}