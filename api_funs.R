#' Create API calls
#'
#' @param url The base url 
#' @param start_date The date of the first treatment to be retrieved
#' @param end_date The date of the last treatment to be retrieved
#' @param treat_count The maximum number of treatments to be retrieved
#' @param profile_count The maximum number of profiles to be retrieved
#'
#' @return
#' @export
#'
#' @examples
api_call <- function(
  url = NULL,
  start_date    = NULL,
  end_date      = NULL,
  treat_count   = 100,
  profile_count = 50) {
  
  params <- list(
    treatments = paste0(
      "?find[created_at][$gte]=", start_date, 
      "&find[created_at][$lte]=", end_date),
    profile = paste0(
      "?count=", profile_count))
  
  calls <- lapply(names(params), function(x) 
    file.path(url, "api/v1", paste0(x, ".json", params[[x]]))
    )
  
  return(calls)
}


#' Run API calls and retrieve data
#'
#' @param api the url/api call of the page to retrieve
#'
#' @return
#' @export
#'
#' @examples
api_run <- function(api) {
  
  resp <- httr::GET(api)
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  parsed <- jsonlite::fromJSON(
    httr::content(resp, "text"), simplifyVector = FALSE)
  
  if (httr::status_code(resp) != 200) {
    warning(
      sprintf(
        "GitHub API request failed [%s]\n%s\n<%s>", 
        status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }
  
  structure(
    list(
      content = parsed,
      url = url_name,
      response = resp
    ),
    class = "ns_api"
  )
}


## Retrieve treatment and profile data from Nightscout -------------------------
# ls_calls <- api_call()
# 
# ls_json <- lapply(ls_calls, api_run)
