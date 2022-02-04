## User functions --------------------------------------------------------------
# prefix <- function(data, prefix) {
#   if(ncol(data) > 0)
#     setNames(data, paste(prefix, names(data), sep = "."))
# }

vcts <- vector("list")
vcts$profile <- c("dia", "carbs_hr", "delay", "timezone", "startDate")
vcts$parm    <- c("carbratio", "sens", "basal", "target_high", "target_low")


#' Tidy single nightscout treatments json object to data.table
#'
#' @param dat 
#'
#' @return
#' @export
#'
#' @examples
tidy_treats <- function(x) {
  
  # If event is a 'Bolus Wizard' then object has nested boluscalc information.
  if("boluscalc" %in% names(x)) {
    d1 <- data.table::as.data.table(x[!names(x) %in% "boluscalc"])
    d2a <- as.data.table(x[["boluscalc"]])
    d2b <- tidytable::nest_by.(d2a, .key = "boluscalc")
    d1$boluscalc <- d2b
  } else {
    d1 <- data.table::as.data.table(x)
  }
  
  return(d1)
  
  }


#' Tidy and combine a list of treatment json objects
#'
#' @param ls_treats 
#'
#' @return
#' @export
#'
#' @examples
bind_treats <- function(ls_treats) {
  
  dat_treat <- data.table::rbindlist(
    lapply(ls_treats, tidy_treats)
    , fill = TRUE, use.names = TRUE)
  
  ## Remove duplicate entries
  dat_treat[, c("_id", "NSCLIENT_ID") := NULL]
  dat_treat <- dat_treat[
    , head(.SD, 1), by = .(
      eventType, date, created_at, pumpId)]
  
  ## Tidy column types 
  
  cols_dates <- c("created_at")
  
  for(col in cols_dates)
    set(dat_treat, j = col, value = lubridate::with_tz(lubridate::fast_strptime(
      dat_treat[[col]], format = "%Y-%m-%dT%H:%M:%SZ", lt = FALSE)))
  
  cols_mills <- c("date", "mills")
  
  for(col in cols_mills)
    set(dat_treat, j = col, value = as.POSIXct(
      dat_treat[[col]]/1000, origin = "1970-01-01"))
  
  dat_treat[, date := as.Date(created_at, tz = "Pacific/Auckland")]
  setnames(dat_treat, old = "created_at", new = "datetime")
  
  
  ## Split treatments into list of data.tables by treatment type -----------------
  dat_treats <- split(dat_treat, by = "eventType")
  dat_treats <- lapply(dat_treats, function(dat) {
    dat[, which(colSums(!is.na(dat)) == 0) := NULL]
  })
  
  for(dat in names(dat_treats[!names(dat_treats) == "Bolus Wizard"]))
    dat_treats[[dat]][, boluscalc := NULL]
  
  return(dat_treats[])
  
}

# bind_treats(ls_json$treatments$content)


#' Tidy raw profile json data
#'
#' @param dat 
#'
#' @return
#' @export
#'
#' @examples
tidy_profiles <- function(dat) {
  
  ## Drop empty levels
  do.call(
    cbind, Filter(nrow, list(
      
      ## All first level headings except store settings
      tibble::as_tibble(dat[!names(dat) %in% "store"])
      
      ## Store
      , merge(
        
        ## All second level heading within store
        rbindlist(
          lapply(dat$store, function(x) x[intersect(names(x), vcts$profile)])
          , idcol = "profile", fill = TRUE, use.names = TRUE)
        
        ## All parameters (carbratio, sens, basal, target_high, target_low)
        , rbindlist(
          sapply(dat$store, function(profile) {
            
            ## Parameter settings
            rbindlist(
              sapply(vcts$parm, function(parm)
                rbindlist(profile[[parm]], idcol = "id_time", fill = TRUE)
                , simplify = FALSE)
              , idcol = "parameter", use.names = TRUE) %>% 
              tidytable::nest.(parm_set = one_of("id_time", "time", "value", "timeAsSeconds"))
            
          }, simplify = FALSE)
          , idcol = "profile", fill = TRUE, use.names = TRUE) %>% 
          tidytable::nest.(parameters = one_of("parameter", "parm_set"))
        
        , by = "profile"
      ) %>% 
        tidytable::nest.(store = one_of(
          "profile", "dia", "carbs_hr", "delay", "timezone", "startDate", "parameters")
        )
    ))
  )
  
}


#' Tidy and combine a list of profiles json objects
#'
#' @param ls_profiles 
#'
#' @return
#' @export
#'
#' @examples
bind_profiles <- function(ls_profiles) {
  
  ## Determine base basal insulin for each profile -----------------------------------
  dat_prof <- suppressWarnings(
    data.table::rbindlist(
      lapply(ls_profiles, tidy_profiles)
      , fill = TRUE, use.names = TRUE)
  )
  
  dat_p1 <- tidytable::unnest.(dat_prof)[defaultProfile == profile]
  dat_p2 <- tidytable::unnest.(dat_p1, parameters)
  dat_p3 <- data.table::dcast(dat_p2, ... ~ parameter, value.var = "parm_set")
  
  dat_parms <- rbindlist(
    sapply(
      c("basal", "carbratio", "sens", "target_high", "target_low"), function(x) 
        tidytable::unnest.(dat_p3[, .(startDate, .SD), .SDcols = c(x)]),
      simplify = FALSE) 
    , idcol = "parameter")
  
  dat_parms[, `:=` (
    startDate  = lubridate::with_tz(
      as.POSIXct(startDate, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"), tzone = "NZ"),
    value      = as.numeric(value),
    time_start = as.integer(timeAsSeconds)
  )]
  
  dat_parms[
    , time_end := shift(time_start, n = 1L, fill = 24L*60L*60L, type = "lead")
    , by = .(parameter, startDate)]
  
  dat_parms[parameter == "basal", `:=` (
    diff_secs = time_end - time_start,
    diff_hrs  = (time_end - time_start) / (60 *60),
    value_hr  = value * (time_end - time_start) / (60 *60),
    basal_hour = as.integer(substr(time, 1, 2))
  )]
  
  return(dat_parms[])

}

# bind_profiles(ls_profiles = ls_json$profile$content)
