
read_frailty_data <- function(filename) {
  frailty_data <- readr::read_csv(
    file = filename,
    col_types = readr::cols(
      .default = readr::col_character(),
      ID = readr::col_double(),
      "Hospital number" = readr::col_character(),
      "Place of Residence" = readr::col_factor(levels = NULL,
        include_na = TRUE),
      "CCG" = readr::col_factor(levels = NULL, include_na = TRUE),
      "Mode of admission" = readr::col_factor(levels = NULL,
                                              include_na = TRUE),
      "Source of admission" = readr::col_factor(levels = NULL,
                                              include_na = TRUE),
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
#      "Bed Manager Override" = readr::col_logical(),
#      "CFS" = readr::col_integer(),
#      "DNA CPR" = readr::col_logical(),
#      "ACP" = readr::col_logical(),
#      "ACP Info" = readr::col_logical(),
      "Arrival to Discharge (Days)" = readr::col_number(),
      "Discharge destination" = readr::col_factor(levels = NULL,
        include_na = TRUE),
      "Arrival in A&E" = readr::col_datetime(
        format = "%d/%m/%Y %H:%M:%S")
    ))
  
  frailty_data <- dplyr::mutate(frailty_data,
    "Date/Time of Referral" = 
       dplyr::if_else(is.na(.data[["Date/Time of Referral"]]),
                      .data[["Date/Time of Admission to 1A"]],
                      .data[["Date/Time of Referral"]]),
    "event" = TRUE) # This is used by the Kaplan-Meier curves and should always be TRUE
  
  frailty_data <- dplyr::rename(frailty_data,
    "mode_of_admission" = .data[["Mode of admission"]],
    "place_of_residence" = .data[["Place of Residence"]],
    "discharge_destination" = .data[["Discharge destination"]],
    "source_of_admission" = .data[["Source of admission"]],
    "LOS" = .data[["Arrival to Discharge (Days)"]],
    "ed_arrival_datetime" = .data[["Arrival in A&E"]])
  
  frailty_data[["mode_of_admission"]][frailty_data[["mode_of_admission"]] == "Case Finding"] <- "Case finding"
  frailty_data[["mode_of_admission"]][frailty_data[["mode_of_admission"]] == "Direct Admission"] <- "Direct admission"
  forcats::fct_drop(frailty_data[["mode_of_admission"]])
  
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
      clean = TRUE),
    "arrival_to_referral_mins" =
      as.integer(difftime(.data[["Date/Time of Referral"]],
                          .data[["ed_arrival_datetime"]],
                          units = "hours"))
  )
}
