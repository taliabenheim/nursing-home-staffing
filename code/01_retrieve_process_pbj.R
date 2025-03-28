library(httr)
library(jsonlite)
library(dplyr)

# Define base URLs
base_urls <- c(
  "https://data.cms.gov/data-api/v1/dataset/dcc467d8-5792-4e5d-95be-04bf9fc930a1/data",
  "https://data.cms.gov/data-api/v1/dataset/5b84bdf2-b246-4b3c-be1b-cf7c2bcb3391/data",
  "https://data.cms.gov/data-api/v1/dataset/ef20ab44-833c-4611-9600-2862e460072c/data",
  "https://data.cms.gov/data-api/v1/dataset/4fa21c39-1d12-4ed9-8d6b-b99d629217b2/data",
  "https://data.cms.gov/data-api/v1/dataset/73f63730-8230-41f8-a33f-b740c7e79dde/data"
)

# Initialize list to store all quarterly datasets
all_quarters_data <- list()

# Notify user that code can take a long time
print("Please note: This script takes several hours to run. Progress is logged with timestamps below.")

# Loop through base URLs
for (i in seq_along(base_urls)) {
  base_url <- base_urls[i]

  # Timestamp
  print(paste(Sys.time(), "Starting download", i, "of", length(base_urls), ":", base_url))
  
  # Set parameters
  limit <- 1000  # API only allows 1000 rows per request
  offset <- 0
  all_data <- list()
  
  # Loop to fetch all pages
  repeat {

    #Timestamp
    print(paste(Sys.time(), "Dataset", i, "of", length(base_urls), "- Fetching offset: ", offset))

    # API request with pagination parameters
    api_url <- paste0(base_url, "?limit=", limit, "&offset=", offset)
    
    # GET request
    response <- GET(api_url)
    
    # Break if unsuccessful
    if (status_code(response) != 200) {
      print(paste("Error: API request failed with status", status_code(response)))
      break
    }
    
    # Parse JSON response
    data_json <- content(response, "text", encoding = "UTF-8")
    
    # Check if response is empty before parsing
    if (data_json == "" || is.null(data_json)) {
      print("Warning: Empty API response received.")
      break
    }
    
    # Convert JSON to dataframe
    data_page <- fromJSON(data_json, flatten = TRUE)
    
    # Check that data_page is dataframe and has rows
    if (!is.data.frame(data_page) || nrow(data_page) == 0) {
      print("No more data to retrieve. Stopping pagination.")
      break
    }
    
    # Append page to list
    all_data[[length(all_data) + 1]] <- data_page
    
    # Update offset to get next page
    offset <- offset + limit
  }

  # Combine all pages into single dataframe
  df <- bind_rows(all_data)

  # Timestamp, print total rows retrieved
  print(paste(Sys.time(), "Rows retrieved: ", nrow(df)))

  # Convert data types
  df <- df %>%
    mutate(WorkDate = as.Date(WorkDate, format = "%Y%m%d"),
           year_month = format(WorkDate, "%Y-%m"),
           Hrs_RN = as.numeric(Hrs_RN),
           Hrs_RN_ctr = as.numeric(Hrs_RN_ctr),
           Hrs_LPN = as.numeric(Hrs_LPN),
           Hrs_LPN_ctr = as.numeric(Hrs_LPN_ctr),
           Hrs_CNA = as.numeric(Hrs_CNA),
           Hrs_CNA_ctr = as.numeric(Hrs_CNA_ctr),
           MDScensus = as.numeric(MDScensus)
           )
  
  # Select non-time varying facility characteristics
  facility_vars <- df %>%
    select(PROVNUM, PROVNAME, STATE, CITY, COUNTY_FIPS) %>%
    distinct(PROVNUM, .keep_all = TRUE)
  
  # Aggregate to facility-month level
  monthly_df <- df %>%
    group_by(PROVNUM, year_month) %>%
    summarise(
      monthly_hrs_rn = sum(Hrs_RN, na.rm = TRUE),
      monthly_hrs_rn_ctr = sum(Hrs_RN_ctr, na.rm = TRUE),
      monthly_hrs_lpn = sum(Hrs_LPN, na.rm = TRUE),
      monthly_hrs_lpn_ctr = sum(Hrs_LPN_ctr, na.rm = TRUE),
      monthly_hrs_cna = sum(Hrs_CNA, na.rm = TRUE),
      monthly_hrs_cna_ctr = sum(Hrs_CNA_ctr, na.rm = TRUE),
      mean_census = mean(MDScensus, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(
      share_hrs_all = (monthly_hrs_rn_ctr + monthly_hrs_lpn_ctr + monthly_hrs_cna_ctr) /
                      (monthly_hrs_rn + monthly_hrs_lpn + monthly_hrs_cna),
      share_hrs_rn = monthly_hrs_rn_ctr / monthly_hrs_rn,
      share_hrs_lpn = monthly_hrs_lpn_ctr / monthly_hrs_lpn,
      share_hrs_cna = monthly_hrs_cna_ctr / monthly_hrs_cna
    ) %>%
    left_join(facility_vars, by = "PROVNUM")
  
  # Append aggregated data to list
  all_quarters_data[[length(all_quarters_data) + 1]] <- monthly_df
}

# Combine all quarters into single dataframe
final_df <- bind_rows(all_quarters_data)

# Save CSV
write.csv(final_df, "data/intermediate/monthly_pbj", row.names = FALSE)