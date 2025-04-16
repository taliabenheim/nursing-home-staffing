library(tidyverse)
library(knitr)
library(kableExtra)

# Function to create a summary table for a given grouping var
create_summary_table <- function(df, grouping_var, caption, col_names = NULL) {
  grouping_var <- enquo(grouping_var)
  
  # Summarize facility characteristics by grouping var
  summary_facility <- df %>%
    group_by(!!grouping_var) %>%
    summarize(
      # Continuous vars: mean (SE)
      across(
        c(months_contract, na_hprd, lpn_hprd, rn_hprd, avg_share_hrs_all, n_cert_beds, 
          overall_rating, qm_rating, staffing_rating, health_inspection_rating, 
          nursing_staff_turnover, rn_turnover),
        ~ paste0(
            sprintf("%.2f", mean(.x, na.rm = TRUE)), 
            " (", sprintf("%.2f", sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))), ")"
        ),
        .names = "avg_{.col}"
      ),
      # Binary vars: percentage (SE)
      across(
        c(for_profit),
        ~ paste0(
            sprintf("%.2f", mean(.x, na.rm = TRUE) * 100), 
            " (", sprintf("%.2f", sd(.x, na.rm = TRUE) * 100 / sqrt(sum(!is.na(.x)))), ")"
        ),
        .names = "{.col}_percent"
      )
    )
  
  # Reshape summary table so group values become columns
  summary_facility <- summary_facility %>%
    pivot_longer(
      cols = -!!grouping_var,
      names_to = "Category",
      values_to = "Value"
    ) %>%
    pivot_wider(
      names_from = !!grouping_var,
      values_from = Value
    ) %>%
    # Remove NA columns
    select(-matches("^NA$"))
  
  # Label and reorder names
  summary_facility <- summary_facility %>%
    mutate(
      Category = recode(
        Category,
        avg_avg_share_hrs_all = "Share of Contract Hours",
        avg_months_contract = "Months of Contract Staffing (Max. 15)",
        avg_na_hprd = "NA Hours per Resident Day",
        avg_lpn_hprd = "LPN Hours per Resident Day",
        avg_rn_hprd = "RN Hours per Resident Day",
        avg_n_cert_beds = "Number of Certified Beds",
        avg_overall_rating = "Overall Star Rating (1-5)",
        avg_qm_rating = "Quality Measure Rating (1-5)",
        avg_staffing_rating = "Staffing Rating (1-5)",
        avg_health_inspection_rating = "Health Inspection Rating (1-5)",
        avg_nursing_staff_turnover = "% Nursing Staff Turnover",
        avg_rn_turnover = "% RN Turnover",
        for_profit_percent = "% For-Profit"
      ),
      Category = factor(
        Category,
        levels = c(
          "Share of Contract Hours",
          "Months of Contract Staffing (Max. 15)",
          "Number of Certified Beds",
          "Overall Star Rating (1-5)",
          "Quality Measure Rating (1-5)",
          "Staffing Rating (1-5)",
          "Health Inspection Rating (1-5)",
          "RN Hours per Resident Day",
          "LPN Hours per Resident Day",
          "NA Hours per Resident Day",
          "% Nursing Staff Turnover",
          "% RN Turnover",
          "% For-Profit"
        )
      )
    ) %>%
    arrange(Category)
  
  # Create table
  summary_table <- summary_facility %>%
    kable(
      format = "html",
      digits = 2,
      caption = caption,
      col.names = col_names
    ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive")
    )
  
  list(
    df = summary_facility,
    table = summary_table
  )
}