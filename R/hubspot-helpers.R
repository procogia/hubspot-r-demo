# HubSpot API Helper Functions
# Base URL for HubSpot API
HUBSPOT_BASE_URL <- "https://api.hubapi.com"

#' Make authenticated requests to HubSpot API
#' @param endpoint API endpoint path
#' @param method HTTP method (GET or POST)
#' @param body Request body for POST requests
#' @return Parsed JSON response
hubspot_request <- function(endpoint, method = "GET", body = NULL) {
  hubspot_token <- Sys.getenv("HUBSPOT_ACCESS_TOKEN")
  
  if (hubspot_token == "") {
    stop("HubSpot access token not found. Please set HUBSPOT_ACCESS_TOKEN environment variable.")
  }
  
  url <- paste0(HUBSPOT_BASE_URL, endpoint)
  
  req <- httr2::request(url) |>
    httr2::req_headers(
      Authorization = paste("Bearer", hubspot_token),
      `Content-Type` = "application/json"
    )
  
  if (method == "POST" && !is.null(body)) {
    req <- req |> httr2::req_body_json(body)
  }
  
  if (method == "GET") {
    response <- req |> httr2::req_perform()
  } else if (method == "POST") {
    response <- req |> httr2::req_method("POST") |> httr2::req_perform()
  }
  
  # Parse JSON response
  content <- response |> httr2::resp_body_json()
  return(content)
}

#' Get all results with pagination
#' @param endpoint API endpoint path
#' @param limit Number of results per page
#' @return List of all results across all pages
hubspot_get_all <- function(endpoint, limit = 100) {
  all_results <- list()
  after <- NULL
  page <- 1
  
  repeat {
    # Build query parameters
    query_params <- list(limit = limit)
    if (!is.null(after)) {
      query_params$after <- after
    }
    
    # Construct URL with query parameters
    query_string <- paste(names(query_params), query_params, sep = "=", collapse = "&")
    full_endpoint <- paste0(endpoint, "?", query_string)
    
    cat("Fetching page", page, "...\n")
    
    response <- hubspot_request(full_endpoint)
    
    # Add results to our list
    if (!is.null(response$results)) {
      all_results <- c(all_results, response$results)
    }
    
    # Check if there are more pages
    if (is.null(response$paging) || is.null(response$paging$`next`)) {
      break
    }
    
    after <- response$paging$`next`$after
    page <- page + 1
  }
  
  return(all_results)
}

#' Get total count of deals without retrieving all data
#' @return Total number of deals or NULL if error
get_deals_count <- function() {
  tryCatch({
    # Use the search endpoint to get just the count
    search_endpoint <- "/crm/v3/objects/deals/search"
    
    search_body <- list(
      filterGroups = list(), # No filters = all deals
      sorts = list(),
      properties = list("hs_object_id"), # Minimal property to reduce response size
      limit = 1, # We only need the count, not the actual data
      after = 0
    )
    
    response <- hubspot_request(search_endpoint, method = "POST", body = search_body)
    
    # Return the total count
    return(response$total)
    
  }, error = function(e) {
    cat("Error getting deals count:", e$message, "\n")
    return(NULL)
  })
}

#' Safe version of hubspot_request with error handling
#' @param endpoint API endpoint path
#' @return API response or NULL if error
safe_hubspot_request <- function(endpoint) {
  tryCatch({
    hubspot_request(endpoint)
  }, error = function(e) {
    cat("API Error:", e$message, "\n")
    return(NULL)
  })
}

#' Process contacts list into a clean data frame
#' @param contacts_list List of contact objects from API
#' @return Tibble with contact data
process_contacts <- function(contacts_list) {
  contacts_list |>
    purrr::map_dfr(~ {
      tibble::tibble(
        id = .x$id,
        email = .x$properties$email %||% NA,
        firstname = .x$properties$firstname %||% NA,
        lastname = .x$properties$lastname %||% NA
      )
    })
}