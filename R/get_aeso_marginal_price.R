#' AESO Electricity Marginal Price API Connection
#' @description Pulls AESO Pool Price from API

library(httr2)
library(dplyr)

#' @param start_date Start date in YYYY-MM-DD str format.
#' @param end_date End date in YYYY-MM-DD str format.
#' @return A dataframe object of AESO marginal prices.

aeso_api_key <- "9982a9c6f4594a8bacc424eeea75e562"

get_aeso_marginal_price <- function(start_date, end_date) {

  # Create API call URL
  req <- httr2::request("https://apimgw.aeso.ca/public/systemmarginalprice-api/v1.1/price/systemMarginalPrice") %>%
    httr2::req_headers(`API-KEY` = aeso_api_key, `Cache-Control` = "no-cache") %>%
    httr2::req_url_query(startDate = start_date) %>%
    httr2::req_url_query(endDate = end_date)

  # Call API
  tryCatch({
    resp <- req %>% httr2::req_perform()

    # Extract list from JSON
    content <- resp %>% httr2::resp_body_json()
    report_data <- content$return$`System Marginal Price Report`

    # Clean and return as df
    dplyr::bind_rows(report_data) %>%
      dplyr::mutate(across(c(system_marginal_price, volume), as.numeric))

    # Print error code
  }, error = function(e) {
    message("Connection Error: ", e$message)
    return(NULL)
  })
}

# Test Usage
# aeso_marginal_price_result <- get_aeso_marginal_price(start_date, end_date)
print(aeso_marginal_price_result)
