# ── Setup ───────────────────────────────────────────────────────────────────────
library(httr2)
library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)
hs_token <- Sys.getenv("HUBSPOT_ACCESS_TOKEN")        # Private app token (Bearer)
deal_id  <- "39576186694"                      # your example



deal <- request(glue::glue(
  "https://api.hubapi.com/crm/v3/objects/deals/{deal_id}"
)) |>
  req_headers(Authorization = paste("Bearer", hs_token)) |>
  req_url_query(
    .multi = "explode",
    propertiesWithHistory = c("amount", "dealstage")
  ) |>
  req_perform() |>
  resp_body_json()

# :::

library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)
library(tibble)

# 1) Pull only the two properties you care about
ph <- deal$propertiesWithHistory[c("amount", "dealstage")]



library(purrr)
library(tibble)
library(dplyr)
library(lubridate)

# Take just the two histories
ph <- deal$propertiesWithHistory[c("amount","dealstage")]

parse_property_history <- function(v, property_name) {
    tibble(
      property = property_name,
      ts = as_datetime(v$timestamp),
      new_value = if (is.null(v$value)) NA else as.character(v$value),
      source = if (is.null(v$sourceType)) NA_character_ else v$sourceType,
      sourceId = if (is.null(v$sourceId)) NA else v$updatedByUserId
    ) 
}

amount_hist <- map_dfr(ph$amount, parse_property_history, property_name = "amount") |>
  arrange(ts)

stage_hist <- map_dfr(ph$dealstage, parse_property_history, property_name = "stage_id") |>
  arrange(ts) 

combined_hist <- bind_rows(amount_hist, stage_hist) |>
  pivot_wider(names_from = property, values_from = new_value) |>
  arrange(ts) |>
  fill(amount, stage_id, .direction = "down")

combined_hist

stage_pipe <- request("https://api.hubapi.com/crm/v3/pipelines/deals/default") |>
  req_headers(Authorization = paste("Bearer", hs_token)) |>
  req_perform() |>
  resp_body_json()

stage_map <- purrr::map_dfr(
  stage_pipe$stages,
  ~ tibble::tibble(
    stage_id = .x$id,
    stage_label = .x$label
  )
)

stage_map


user_ids <- combined_hist |>
  filter(source == "CRM_UI", !is.na(sourceId), nzchar(sourceId)) |>
  distinct(sourceId) |>
  pull()

get_user <- function(uid) {
  u <- request(glue::glue("https://api.hubapi.com/settings/v3/users/{uid}")) |>
    req_headers(Authorization = paste("Bearer", hs_token)) |>
    req_perform() |>
    resp_body_json()

  tibble(
    sourceId = uid,
    user_name = paste(u$firstName, u$lastName),
    user_email = u$email
  )
}

user_map <- if (length(user_ids)) {
  map_dfr(user_ids, get_user)
} else {
  tibble(
    sourceId = character(),
    user_name = character(),
    user_email = character()
  )
}

user_map



all_users <- request("https://api.hubapi.com/settings/v3/users") |>
  req_headers(Authorization = paste("Bearer", hs_token)) |>
  req_perform() |>
  resp_body_json() |>
  pluck("results") |>
  map_dfr(~ tibble(
    user_id = .x$id |> as.integer(),
    user_name = paste(.x$firstName, .x$lastName),
    user_email = .x$email
  ))

dealstage_hist <- combined_hist |>
  left_join(stage_map, by = "stage_id") |>
  left_join(all_users, by = c("sourceId" = "user_id")) |>
  select(-starts_with("source"), -user_email)
dealstage_hist

