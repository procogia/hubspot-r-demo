# HubSpot API Helper Functions
# Base URL for HubSpot API
HUBSPOT_BASE_URL <- "https://api.hubapi.com"

# ── Setup ───────────────────────────────────────────────────────────────────────
library(httr2)
library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)
library(tibble)

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
get_deals_count <- function() get_objects_count('deals') # Wrapper for deals (legacy)

#' Get total count of objects without retrieving all data
#' @return Total number of objects or NULL if error
get_objects_count <- function(obj = 'deals') {
  # deals, leads, # not tested: contacts, companies, tickets, leads
  tryCatch(
    {
      # Use the search endpoint to get just the count
      search_endpoint <- sprintf("/crm/v3/objects/%s/search", obj)

      search_body <- list(
        filterGroups = list(), 
        sorts = list(),
        properties = list("hs_object_id"), # Minimal property to reduce response size
        limit = 1, # We only need the count, not the actual data
        after = 0
      )

      response <- hubspot_request(
        search_endpoint,
        method = "POST",
        body = search_body
      )

      # Return the total count
      return(response$total)
    },
    error = function(e) {
      cat(sprintf("Error getting %s count: %s\n", obj, e$message))
      return(NULL)
    }
  )
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

#' Get random sample of deals with basic properties
#' @param n Number of deals to sample
#' @return List of deal objects
get_random_deals <- function(n = 25) {
  # First get total count
  total_count <- get_deals_count()
  
  if (is.null(total_count) || total_count < n) {
    cat("Warning: Requested", n, "deals but only", total_count, "available\n")
    n <- min(n, total_count)
  }
  
  # Get random offset
  max_offset <- max(0, total_count - n)
  random_offset <- sample(0:max_offset, 1)
  
  search_body <- list(
    filterGroups = list(),
    sorts = list(),
    properties = c("dealname", "amount", "dealstage", "pipeline", "createdate", "closedate"),
    limit = n,
    after = random_offset
  )
  
  response <- hubspot_request("/crm/v3/objects/deals/search", method = "POST", body = search_body)
  return(response$results)
}

#' Get company name for a deal
#' @param deal_id Deal ID
#' @return Company name or NA
get_deal_company <- function(deal_id) {
  tryCatch({
    # Get associations to companies
    endpoint <- paste0("/crm/v3/objects/deals/", deal_id, "/associations/companies")
    associations <- hubspot_request(endpoint)
    
    if (length(associations$results) > 0) {
      company_id <- associations$results[[1]]$id
      
      # Get company details
      company_endpoint <- paste0("/crm/v3/objects/companies/", company_id, "?properties=name")
      company <- hubspot_request(company_endpoint)
      
      return(company$properties$name %||% NA)
    }
    
    return(NA)
  }, error = function(e) {
    return(NA)
  })
}

#' Get all users from HubSpot
#' @return Tibble with user_id, user_name, user_email
get_users <- function() {
  tryCatch({
    user_response <- hubspot_request("/settings/v3/users")
    
    if (!is.null(user_response$results)) {
      user_map <- map_dfr(user_response$results, ~ tibble(
        user_id = .x$id |> as.integer(),
        user_name = paste(.x$firstName %||% "Unknown", .x$lastName %||% "User"),
        user_email = .x$email %||% NA_character_
      ))
      return(user_map)
    } else {
      return(tibble(
        user_id = integer(),
        user_name = character(),
        user_email = character()
      ))
    }
  }, error = function(e) {
    cat("Error getting users:", e$message, "\n")
    return(tibble(
      user_id = integer(),
      user_name = character(),
      user_email = character()
    ))
  })
}

#' Get deal stages from HubSpot pipeline
#' @param pipeline_id Pipeline ID (default is "default")
#' @return Tibble with stage_id, stage_label
get_stages <- function(pipeline_id = "default") {
  tryCatch({
    endpoint <- paste0("/crm/v3/pipelines/deals/", pipeline_id)
    stage_response <- hubspot_request(endpoint)
    
    if (!is.null(stage_response$stages)) {
      stage_map <- map_dfr(stage_response$stages, ~ tibble(
        stage_id = .x$id,
        stage_label = .x$label
      ))
      return(stage_map)
    } else {
      return(tibble(
        stage_id = character(),
        stage_label = character()
      ))
    }
  }, error = function(e) {
    cat("Error getting stages:", e$message, "\n")
    return(tibble(
      stage_id = character(),
      stage_label = character()
    ))
  })
}

#' Get deal history for multiple deals
#' @param d Vector of deal IDs
#' @return Tibble with deal history including deal_id column
get_deal_history <- function(d) {
  # Helper function to parse property history
  parse_property_history <- function(v, property_name) {
    tibble(
      property = property_name,
      ts = as_datetime(v$timestamp),
      new_value = if (is.null(v$value)) NA else as.character(v$value),
      source = if (is.null(v$sourceType)) NA_character_ else v$sourceType,
      sourceId = if (is.null(v$sourceId)) NA else v$updatedByUserId
    )
  }


  
  # Get users and stages for joins
  user_map <- get_users()
  stage_map <- get_stages()
  
  # Process each deal
  all_deal_history <- map_dfr(d, function(deal_id) {
    tryCatch(
      {
        # Get deal with property history
        deal_response <- request(glue::glue(
          "https://api.hubapi.com/crm/v3/objects/deals/{deal_id}"
        )) |>
          req_headers(Authorization = paste("Bearer", hubspot_token)) |>
          req_url_query(
            .multi = "explode",
            propertiesWithHistory = c("amount", "dealstage")
          ) |>
          req_perform() |>
          resp_body_json()


        # Extract property histories
        ph <- deal_response$propertiesWithHistory[c("amount", "dealstage")]

        if (is.null(ph) || (is.null(ph$amount) && is.null(ph$dealstage))) {
          return(tibble())
        }

        # Parse histories
        amount_hist <- if (!is.null(ph$amount)) {
          map_dfr(
            ph$amount,
            parse_property_history,
            property_name = "amount"
          ) |>
            arrange(ts)
        } else {
          tibble()
        }

        stage_hist <- if (!is.null(ph$dealstage)) {
          map_dfr(
            ph$dealstage,
            parse_property_history,
            property_name = "stage_id"
          ) |>
            arrange(ts)
        } else {
          tibble()
        }

        # Combine histories
        combined_hist <- bind_rows(amount_hist, stage_hist) |>
          pivot_wider(names_from = property, values_from = new_value) |>
          arrange(ts) |>
          fill(amount, stage_id, .direction = "down") |>
          mutate(deal_id = deal_id, .before = 1) # Add deal_id as first column

        # Join with stage and user information
        dealstage_hist <- combined_hist |>
          left_join(stage_map, by = "stage_id") |>
          left_join(user_map, by = c("sourceId" = "user_id")) |>
          select(deal_id, ts, amount, stage_id, stage_label, user_name)

        return(dealstage_hist)
      },
      error = function(e) {
        cat("Warning getting history for deal", deal_id, ":", e$message, "\n")
        return(tibble())
      }
    )
  })
  
  return(all_deal_history)
}

# TODO: Refactor to reduce code duplication with get_deal_history()

#' Get object history for multiple objects
#' @param d Vector of object IDs
#' @return Tibble with object history including object_id column
get_obj_history <- function(d, obj_type = 'leads', props = NULL) {
  
  if (obj_type == 'leads') {
    if (is.null(props)) props <- c("hs_pipeline_stage")
    stage_map <- get_lead_pipeline_stages() |> select(-pipeline_id)
  } else if (obj_type == 'deals') {
    if (is.null(props))  props <- c("amount", "dealstage")
    stage_map <- get_stages()
  } else {
    stop("Unsupported obj_type. Supported types are 'leads' and 'deals'.")
  }
  
  # Helper function to parse property history
  parse_property_history <- function(v, property_name) {
    tibble(
      property = property_name,
      ts = as_datetime(v$timestamp),
      new_value = if (is.null(v$value)) NA else as.character(v$value),
      source = if (is.null(v$sourceType)) NA_character_ else v$sourceType,
      sourceId = if (is.null(v$sourceId)) NA else v$updatedByUserId
    )
  }

  # Process each object
  all_obj_history <- map_dfr(d, function(obj_id) {
    tryCatch(
      {
        # Get object with property history
        obj_response <- request(glue::glue(
          "https://api.hubapi.com/crm/v3/objects/{obj_type}/{obj_id}"
        )) |>
          req_headers(Authorization = paste("Bearer", hubspot_token)) |>
          req_url_query(
            .multi = "explode",
            propertiesWithHistory = props
          ) |>
          req_perform() |>
          resp_body_json()

        # Extract property histories
        ph <- obj_response$propertiesWithHistory[props]

        if (is.null(ph))  {
          return(tibble())
        }

        # Parse histories  ## TODO this part is specific to leads and deals, needs to be generalized
        stage_hist <- if (!is.null(ph$hs_pipeline_stage)) {
          map_dfr(
            ph$hs_pipeline_stage,
            parse_property_history,
            property_name = "stage_id"
          ) |>
            arrange(ts)
        } else {
          tibble()
        }

        # # Combine histories: This needs to be generalized for multiple properties
        # combined_hist <- bind_rows(amount_hist, stage_hist) |>
        combined_hist <- stage_hist |>
          pivot_wider(names_from = property, values_from = new_value) |>
          arrange(ts) |>
        # fill(amount, stage_id, .direction = "down") |>
          mutate(id = obj_id, .before = 1) |>
          left_join(stage_map, by = "stage_id") |>
          select(-stage_id)

        return(combined_hist)
      },
      error = function(e) {
        cat("Warning getting history for object", obj_id, ":", e$message, "\n")
        return(tibble())
      }
    )
  })

  return(all_obj_history)
}


#' Get all deals with creation time and basic properties
#' @param limit Number of deals per API call (default 100, max 100)
#' @return Tibble with deal_id, deal_name, created_date, and other basic properties
get_all_deals_with_creation <- function(limit = 100) {
  cat("🔍 Fetching all deals with creation time from HubSpot...\n")

  all_deals <- list()
  after <- NULL
  page <- 1

  repeat {
    tryCatch(
      {
        cat("📄 Fetching page", page, "...\n")

        # Use the regular objects endpoint with pagination
        endpoint <- "/crm/v3/objects/deals"
        query_params <- list(
          limit = limit,
          properties = paste(
            c(
              "dealname",
              "amount",
              "dealstage",
              "createdate",
              "closedate",
              "hs_lastmodifieddate"
            ),
            collapse = ","
          )
        )

        # Add pagination if needed
        if (!is.null(after)) {
          query_params$after <- after
        }

        # Build query string
        query_string <- paste(
          names(query_params),
          query_params,
          sep = "=",
          collapse = "&"
        )
        full_endpoint <- paste0(endpoint, "?", query_string)

        response <- hubspot_request(full_endpoint)

        # Extract deals from this page
        if (!is.null(response$results) && length(response$results) > 0) {
          all_deals <- c(all_deals, response$results)

          cat(
            "   ✅",
            length(response$results),
            "deals retrieved (total so far:",
            length(all_deals),
            ")\n"
          )
        }

        # Check if there are more pages
        if (is.null(response$paging) || is.null(response$paging$`next`)) {
          break
        }

        after <- response$paging$`next`$after
        page <- page + 1

        # Small delay to respect rate limits
        Sys.sleep(0.1)
      },
      error = function(e) {
        cat("❌ Error on page", page, ":", e$message, "\n")
        break
      }
    )
  }

  cat("🎉 Total deals retrieved:", length(all_deals), "\n")

  # Convert to tibble
  deals_df <- map_dfr(all_deals, function(deal) {
    props <- deal$properties
    tibble(
      deal_id = deal$id,
      deal_name = props$dealname %||% "Unnamed Deal",
      amount = as.numeric(props$amount %||% 0),
      stage = props$dealstage %||% "Unknown",
      pipeline = props$pipeline %||% "default",
      created_date = as.Date(props$createdate),
      modified_date = as.Date(props$hs_lastmodifieddate),
      close_date = as.Date(props$closedate)
    )
  })

  return(deals_df)
}

#' Get all leads with creation time and basic properties
#' @param limit Number of leads per API call (default 100, max 100)
#' @return Tibble with lead_id, lead_name, created_date, and other basic properties
get_all_leads_with_creation <- function(page_limit = 100, max_pages = 0) {
  cat("🔍 Fetching all leads with creation time from HubSpot...\n")
  lead_stages <- get_lead_pipeline_stages() |> 
    select(-pipeline_id)

  all_leads <- list()
  after <- NULL
  page <- 1

  repeat {
  tryCatch(
    {
      cat("📄 Fetching page", page, "...\n")

      # Use the regular objects endpoint with pagination
      endpoint <- "/crm/v3/objects/leads"
      query_params <- list(
        limit = page_limit,
        properties = paste(
          c(
            "hs_lead_name",
            "hs_lead_type",
            "hs_lead_label",
            "createdate",
            "closedate",
            "hs_lastmodifieddate",
            "hs_pipeline_stage",
            "archived"
          ),
          collapse = ","
        )
      )

      # Add pagination if needed
      if (!is.null(after)) {
        query_params$after <- after
      }

      # Build query string
      query_string <- paste(
        names(query_params),
        query_params,
        sep = "=",
        collapse = "&"
      )
      full_endpoint <- paste0(endpoint, "?", query_string)

      response <- hubspot_request(full_endpoint)

      # Extract leads from this page
      if (!is.null(response$results) && length(response$results) > 0) {
        all_leads <- c(all_leads, response$results)

        cat(
          "   ✅",
          length(response$results),
          "leads retrieved (total so far:",
          length(all_leads),
          ")\n"
        )
      }

      # Check if there are more pages
      if (is.null(response$paging) || is.null(response$paging$`next`)) {
        break
      }

      # Limit to max_pages if specified
      if (max_pages > 0 && page >= max_pages) {
        cat("Reached max_pages limit of", max_pages, "\n")
        break
      }

      after <- response$paging$`next`$after
      page <- page + 1

      # Small delay to respect rate limits
      Sys.sleep(0.1)
    },
    error = function(e) {
      cat("❌ Error on page", page, ":", e$message, "\n")
      break
    }
  )
  }

  cat("🎉 Total leads retrieved:", length(all_leads), "\n")

  # Convert to tibble
  leads_df <- map_dfr(all_leads, function(lead) {
    props <- lead$properties
    tibble(
      lead_id = lead$id,
      lead_type = props$hs_lead_type %||% "Unknown",
      lead_name = props$hs_lead_name %||% "Unnamed lead",
      lead_label = props$hs_lead_label %||% "Unknown",
      created_date = as.Date(props$hs_createdate),
      modified_date = as.Date(props$hs_lastmodifieddate),
      archived = as.logical(props$archived %||% NA),
      stage_id = props$hs_pipeline_stage %||% "Unknown",
    ) |>
      left_join(lead_stages, by = "stage_id") |> 
      select(-stage_id)
  })

  return(leads_df)
}

get_leads_after <- function(date, search_type = 'hs_lastmodifieddate', page_limit = 100, max_pages = 0) {
  cat("🔍 Fetching leads of", search_type, "since", as.character(date), "from HubSpot...\n")
  
  lead_stages <- get_lead_pipeline_stages() |> 
    select(-pipeline_id)
  
  # Convert date to timestamp in milliseconds
  timestamp_ms <- as.numeric(as.POSIXct(date)) * 1000
  
  search_body <- list(
    filterGroups = list(
      list(
        filters = list(
          list(
            propertyName = search_type,
            operator = "GTE",
            value = timestamp_ms
          )
        )
      )
    ),
    properties = c(
      "hs_lead_name",
      "hs_lead_type", 
      "hs_lead_label",
      "hs_createdate",
      "closedate",
      "hs_lastmodifieddate",
      "hs_pipeline_stage",
      "archived"
    ),
    limit = page_limit
  )
  
  all_leads <- list()
  after <- NULL
  page <- 1
  
  repeat {
    tryCatch({
      cat("📄 Fetching page", page, "...\n")
      
      if (!is.null(after)) {
        search_body$after <- after
      }
      
      response <- hubspot_request("/crm/v3/objects/leads/search", method = "POST", body = search_body)
      
      if (!is.null(response$results) && length(response$results) > 0) {
        all_leads <- c(all_leads, response$results)
        
        cat("   ✅", length(response$results), "leads retrieved (total so far:", length(all_leads), ")\n")
      }
      
      if (is.null(response$paging) || is.null(response$paging$`next`)) {
        break
      }
      
      if (max_pages > 0 && page >= max_pages) {
        cat("Reached max_pages limit of", max_pages, "\n")
        break
      }
      
      after <- response$paging$`next`$after
      page <- page + 1
      
      Sys.sleep(0.1)
    },
    error = function(e) {
      cat("❌ Error on page", page, ":", e$message, "\n")
      break
    })
  }
  
  cat("🎉 Total leads retrieved:", length(all_leads), "\n")
  
  leads_df <- map_dfr(all_leads, function(lead) {
    props <- lead$properties
    tibble(
      lead_id = lead$id,
      lead_type = props$hs_lead_type %||% "Unknown",
      lead_name = props$hs_lead_name %||% "Unnamed lead",
      lead_label = props$hs_lead_label %||% "Unknown",
      created_date = as.Date(props$hs_createdate),
      modified_date = as.Date(props$hs_lastmodifieddate),
      archived = as.logical(props$archived %||% NA),
      stage_id = props$hs_pipeline_stage %||% "Unknown"
    ) |>
      left_join(lead_stages, by = "stage_id") |> 
      select(-stage_id)
  })
  
  return(leads_df)
}

get_lead_properties <- function() {
  cat("🔍 Fetching lead properties from HubSpot...\n")
  
  tryCatch({
    response <- hubspot_request("/crm/v3/properties/leads")
    
    if (!is.null(response$results)) {
      properties_df <- map_dfr(response$results, function(prop) {
        tibble(
          name = prop$name,
          label = prop$label %||% NA_character_,
          description = prop$description %||% NA_character_,
          type = prop$type %||% NA_character_,
          fieldType = prop$fieldType %||% NA_character_,
          groupName = prop$groupName %||% NA_character_,
          calculated = prop$calculated %||% FALSE,
          hidden = prop$hidden %||% FALSE
        )
      })
      
      cat("✅", nrow(properties_df), "lead properties retrieved\n")
      return(properties_df)
    } else {
      cat("⚠️ No properties found in response\n")
      return(tibble())
    }
  }, error = function(e) {
    cat("❌ Error getting lead properties:", e$message, "\n")
    return(tibble())
  })
}


get_lead_pipeline_stages <- function() {
  resp <- hubspot_request("/crm/v3/pipelines/leads")

  purrr::map_dfr(resp$results, function(p) {
    tibble::tibble(
      pipeline_id = p$id,
      stage_id    = purrr::map_chr(p$stages, "id"),
      stage_label = purrr::map_chr(p$stages, "label"),
      stage_order = purrr::map_int(p$stages, "displayOrder")
    )
  })
}
