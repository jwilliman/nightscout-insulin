#' @title Convenience function for creating request query
#'
#' @param x An orderable vector, i.e., those with relevant methods for `<=`, usually a date field.
#' @param lower Lower range bound of length 1. Will be coerced to character if not already.
#' @param upper Upper range bound of length 1. Will be coerced to character if not already.
#' @param inc.bounds `TRUE` (default) means inclusive bounds, i.e., [lower,upper]. `FALSE` means exclusive bounds, i.e., (lower,upper).
#'
#' @return  A list of maximum length 2, with expressions to use in later functions like `make_request`.
#' @export
#'
find_between <- function(x = "dateString", lower = NULL, upper = NULL, inc.bounds = TRUE) {
  
  if(inc.bounds == TRUE)
    bounds <- c("[$gte]", "[$lte]")
  else
    bounds <- c("[$gt]", "[$lt]")
  
  parms        <- list(as.character(lower), as.character(upper))
  names(parms) <- paste0("find[", x, "]", bounds)
  
  return(Filter(length, parms))
}



#' @title Create single API request to retrieve data from Nightscout
#'
#' @param url Base URL for accessing Nightscout of type `https://YOUR-SITE.com`. See https://nightscout-test.readthedocs.io/en/latest/Nightscout/EN/Technical%20info/api.html for further details. `ap/i/v1` is automatically added to the end of the base url when creating the request.
#' @param api_secret API secret or token as entered when accessing the base URL via a web browser. This API secret is hashed using Secure Hash Algorithm 1 prior to appending to header of API request.
#' @param schema Schema to be requested. For CGM data use `entries`, for Care Portal Treatments use `treatments`, for Treatment Profiles use `profiles`. and for server status and settings use `status`. 
#' @param spec Relevant for schema `entries` only. entry id, such as 55cf81bc436037528ec75fa5 or a type filter such as sgv, mbg, etc. Default is 'sgv' which returns sensor glucose values (SGV). 
#' @param find The query used to find and limit returned data. Supports nested query syntax, for example find[dateString][$gte]=2015-08-27. All find parameters are interpreted as strings. By default the `entries` and `treatments` schemas limit results to the the most recent 10 values from the last 2 days. You can get many more results, by using the date, dateString, and created_at parameters, depending on the type of data you’re looking for. See https://YOUR-SITE.com/api-docs.html for more options. Can use command `find_between` to help create correct query. Not valid and ignored for `profile` and `status` schemas.
#' @param count By default the `entries` and `treatments` schemas limit results to the the most recent 10 values from the last 2 days. You can get many more results by setting this to a higher number. Can set to `Inf` to get all results between two dates defined in `find`. Not valid and ignored for `profile` and `status` schemas.
#' 
#'
#' @return An HTTP response: an S3 list with class httr2_request.
#' @import httr2
#' @export
#'
#' @examples
#' make_request(url = "http://localhost.1337, schema = "entries", spec = "sgv", count = 100)
#' 
#' 
#' 
make_request <- function(url = NULL, api_secret = NULL, schema = "entries", spec = "sgv", find = NULL, count = NULL) {
  
  # Make base URL
  url_base <- file.path(url, "api", "v1", schema)
  if(schema == "entries" & !is.null(spec))
    url_base <- file.path(url_base, spec)
  
  # Add query options
  if(schema %in% c("profile", "status")) {
    if(!is.null(find) | !is.null(count))
      warning("Parameters 'find' and 'count' not valid for current schema, and will be ignored")
    req <- httr2::request(url_base)
  } else {
    req <- do.call(
      httr2::req_url_query, 
      c(list(.req = httr2::request(url_base)), find)) |>
      httr2::req_url_query(count = count)
    
  }
  
  # Add API secret as header
  if(!is.null(api_secret)) {
    api_encoded = openssl::sha1(api_secret)
    httr2::req_headers(
      req,
      "api-secret" = api_encoded,
      "Accept" = "application/json")
  }
  else
    httr2::req_headers(req, "Accept" = "application/json")
  
}


#' @title Create multiple API request to retrieve data from Nightscout
#'
#' @param url Base URL for accessing Nightscout of type `https://YOUR-SITE.com`. See https://nightscout-test.readthedocs.io/en/latest/Nightscout/EN/Technical%20info/api.html for further details. `ap/i/v1` is automatically added to the end of the base url when creating the request.
#' @param api_secret API secret or token as entered when accessing the base URL via a web browser. This API secret is hashed using Secure Hash Algorithm 1 prior to appending to header of API request.
#' @param schemas Vector of schema to be requested. For CGM data use `entries`, for Care Portal Treatments use `treatments`, for Treatment Profiles use `profiles`. and for server status and settings use `status`.
#' Relevant for schema `entries` only. entry id, such as 55cf81bc436037528ec75fa5 or a type filter such as sgv, mbg, etc. Default is 'sgv' which returns sensor glucose values (SGV). 
#' @param date_first First date for `entries` and `treatments` schemas. 
#' @param date_list Last date for `entries` and `treatments` schemas. 
#' @param count By default the `entries` and `treatments` schemas limit results to the the most recent 10 values from the last 2 days. You can get many more results by setting this to a higher number. Can set to `Inf` to get all results between two dates defined in `find`. Not valid and ignored for `profile` and `status` schemas.
#'
#' @return A list of HTTP responses: a list of S3 lists with class httr2_request.
#' @import httr2
#' @export
#'
#' @examples
#' \dontrun{
#' make_requests(
#'   url = "https://YOUR-SITE.com", 
#'   api_secret = "password",
#'   date_first = "2024-01-01", 
#'   date_last = Sys.Date())
#' }
#'  
#' 
make_requests <- function(
    url = NULL, 
    api_secret = NULL, 
    schemas = c("entries", "treatments", "profile"), spec = "sgv", 
    date_first = NULL, date_last = NULL, count = NULL) {
  
  schemas = setNames(schemas, schemas)
  
  requests <- suppressWarnings(
    lapply(schemas, function(schema) {

      if(schema == "entries")
        date_type = "dateString"
      else date_type = "created_at"
      
      make_request(
        url = url, api_secret = api_secret, schema = schema, 
        find = find_between(date_type, lower = date_first, upper = date_last), 
        count = count)
      
      }))
  
  return(requests)
  
}


#' @title Retrieve and save Nightscout data 
#'
#' @param path A folder location where retrieved data is to be saved. 
#' @param suffix By default files are saved according to their schema (e.g. entries.Rds), a suffix may be applied to the filename.
#' @param ... Additional arguments to pass to `make_requests`. Including base `url` and `api_secret`.
#'
#' @return
#' @export
#'
#' @examples
save_requests <- function(path = "", suffix = "", ...) {
  
  reqs <- make_requests(...)
  
  dats <- lapply(reqs, httr2::req_perform)
  
  for(schema in names(dats))
    saveRDS(dats[[schema]], file = file.path(path, paste0(schema, suffix, ".Rds")))
  
}
