#' Calculate insulin basal rates
#'
#' @param dat_profile 
#' @param dat_treats 
#'
#' @return
#' @export
#'
#' @examples
calc_basal <- function(dat_profile, dat_treats) {
  
  dat_basal <- dat_profile[
    parameter == "basal", .(basal_insulin = sum(value_hr)), 
    by = .(startDate)][
      , endDate := data.table::shift(startDate, fill = Sys.time(), type = "lead")]
  
  ## Determine base basal --------------------------------------------------------
  dat_b1 = merge(
    unique(dat_treats$`Meal Bolus`[, .(k = 1, date)])
    , merge(dat_profile[parameter == "basal", c(k = 1, .SD)]
            , dat_basal[, startDate, endDate]
            , by = "startDate")
    , by = "k", allow.cartesian = TRUE)
  
  dat_b1[, `:=`(k, NULL)]
  
  dat_b2 <- dat_b1[
    date >= as.Date(startDate) & date < as.Date(endDate)
    , .(
      date, datetime = as.POSIXct(paste(as.character(date), time), format = "%Y-%m-%d %H:%M"), 
      time_start, absolute = value, base = value)]
  
  
  ## Determine temporary basal ---------------------------------------------------
  if("Temp Basal" %in% names(dat_treats)) {
    
    # Add extra columns if missing
    dat_tb0 <- data.table(
      eventType = character(),
      date = as.Date(character()),
      datetime = as.POSIXct(character()),
      pumpId = numeric(),
      enteredBy = character(),
      duration = integer(),
      absolute = numeric(),
      rate = numeric()
    )
    
    # And drop those not needed.
    dat_tb <- rbindlist(list(
      dat_tb0, dat_treats$`Temp Basal`)
      , fill = TRUE)[, .(datetime, date, eventType, duration, absolute, rate)][order(datetime)]
    
    dat_tb[, time_elapse := (shift(datetime, n = 1L, type = "lead") - datetime)/60]
    dat_tb[, time_start := as.numeric(lubridate::force_tz(datetime, tzone = "UTC"))%%(60*60*24)]
    
    dat_tb2 <- rbindlist(list(
      base = dat_b2,
      temp = dat_tb[, .(date, datetime, time_start, absolute)]), idcol = "type", 
      fill = TRUE)[order(datetime)]
    
  } else {
    dat_tb2 <- dat_b2[order(datetime)]
  } 
  
  ## Tidy dataset
  dat_tb2[, base := nafill(base, "locf")]  
  dat_tb2[is.na(absolute), `:=` (
    type = "temp_end",
    absolute = base)]
  
  ## Drop changes in base where they occur in the middle of a temporary basal
  dat_tb2[, absolute := tidytable::ifelse.(
    type == "base" & shift(type, type = "lag") %in% "temp",
    shift(absolute, type = "lag"), absolute)]
  
  dat_tb2[, time_end := shift(time_start, n = 1L, fill = 24L*60L*60L, type = "lead"),
          by = .(date)]
  
  
  dat_tb2[, `:=` (
    diff_secs = time_end - time_start,
    diff_mins  = (time_end - time_start) / 60,
    diff_hrs  = (time_end - time_start) / (60 *60),
    temp_pos  = pmax(absolute - base, 0),
    temp_neg  = pmin(absolute - base, 0)
  )]
  
  
  dat_tb2[, `:=` (
    basal_base     = base * diff_hrs,
    basal_pos_temp = temp_pos * diff_hrs,
    basal_neg_temp = temp_neg * diff_hrs,
    basal_total    = absolute * diff_hrs
  )]
  
  dat_basal_wide <- dat_tb2[
    , lapply(.SD, sum), by = .(date),
    .SDcols = c("basal_base", "basal_pos_temp", "basal_neg_temp", "basal_total")
  ]
  
  
  dat_basal_long <- data.table::melt(
    dat_basal_wide, id.vars = c("date"), 
    variable.name = "eventType", value.name = "insulin") 
  
  dat_basal_long[, eventType := gsub("_", " ", eventType)]
  
  return(list(long = dat_basal_long[], wide = dat_basal_wide[]))
  
  
}

# calc_basal(dat_profiles, dat_treats)

#' Calculate insulin boluses
#'
#' @param dat_treats 
#'
#' @return
#' @export
#'
#' @examples
calc_bolus <- function(dat_treats)  {
  
  if(any(grepl("Combo", names(dat_treats)))) {
    setnames(dat_treats$`Combo Bolus`, "enteredinsulin", "insulin")
    dat_treats$`Combo Bolus`[, isSMB := FALSE]
  }
  
  vct_bolus <- names(dat_treats)[grepl("Bolus", names(dat_treats))]
  
  dat_bolus <- rbindlist(lapply(vct_bolus, function(event)
    
    dat_treats[[event]][
      !is.na(insulin)
      , .(datetime, date, 
          eventType, insulin, isSMB)]
    
  ))[order(datetime)]
  
  
  dat_bolus_long <- data.table::groupingsets(
    dat_bolus[
      , .(insulin = sum(insulin))
      , by = .(date, eventType = ifelse(
        isSMB %in% TRUE, paste0(eventType, " - SMB"), eventType))],
    j = list(insulin = sum(insulin)), by = c("date", "eventType"),
    sets = list(c("date", "eventType"), c("date")))[
      order(date)
    ]
  
  dat_bolus_long[is.na(eventType), eventType := "Bolus total"]
  
  dat_bolus_wide <- dcast(
    dat_bolus_long, date ~ eventType, value.var = "insulin")
  
  setnames(dat_bolus_wide, tolower(gsub(" ", "_", names(dat_bolus_wide))))

  return(list(
    long = dat_bolus_long,
    wide = dat_bolus_wide)
  )
  
}
  
# calc_bolus(dat_treats)


#' Combine insulin basal and bolus
#'
#' @param basal_wide 
#' @param bolus_wide 
#'
#' @return
#' @export
#'
#' @examples
bind_insulin <- function(basal_wide, bolus_wide) {
  
  dat_insulin_wide <- merge(
    basal_wide, bolus_wide, by = c("date")
  )
  
  dat_insulin_wide[, daily_total := basal_total + bolus_total]
  
  dat_insulin_long <- data.table::melt(
    dat_insulin_wide, id.vars = "date", variable.name = "eventType", value.name = "insulin"
  )[, eventType := factor(
    eventType, 
    levels = c(
      "meal_bolus", "bolus_wizard", "bolus_total",
      "basal_base", "basal_pos_temp", "basal_neg_temp", "basal_total",
      "daily_total"
    ), labels = c(
      "Meal Bolus", "Bolus Wizard", "Bolus total",
      "Basal base", "Basal pos temp", "Basal neg temp", "Basal total",
      
      "Daily total"
    ))][order(date, eventType)]
  
  return(list(long = dat_insulin_long, wide = dat_insulin_wide))
  
}

# bind_insulin(dat_bolus$wide, dat_basal$wide)

#' Extract data from Nightscout and prepare summary of insulin use
#'
#' @param url 
#' @param start_date 
#' @param end_date 
#' @param treat_count 
#' @param profile_count 
#'
#' @return
#' @export
#'
#' @examples
calc_insulin <- function(
  url = NULL,
  start_date    = NULL,
  end_date      = NULL,
  treat_count   = 100,
  profile_count = 50
) {
  
  ls_calls <- api_call(url, start_date, end_date, treat_count, profile_count)
  ls_json  <- lapply(ls_calls, api_run)
  dat_treats   <- bind_treats(ls_json$treatments$content)
  dat_profiles <- bind_profiles(ls_json$profile$content)
  dat_basal    <- calc_basal(dat_profiles, dat_treats)
  dat_bolus    <- calc_bolus(dat_treats)
  dat_insulin  <- bind_insulin(dat_basal$wide, dat_bolus$wide)
  
  return(dat_insulin)
  
}
  
  
