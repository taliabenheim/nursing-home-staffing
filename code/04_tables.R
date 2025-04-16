library(tidyverse)
library(knitr)
library(kableExtra)
library(ggplot2)

# Run function to create tables
source("code/helpers.R")

# Prep data for tables
df <- read_csv("data/intermediate/merged_nhc_pbj.csv", show_col_types = FALSE) %>%
  mutate(
    any_contract = if_else(avg_share_hrs_all > 0, "Any", "None") %>% 
      factor(levels = c("None", "Any")),
    share_hrs_all_quartile = ntile(avg_share_hrs_all, 4),
    months_contract_quartile = ntile(months_contract, 4),
    hospbase_num = if_else(hospbase == "Yes", 1, 0),
    for_profit = if_else(grepl("^For profit", ownership), 1, 0)
  )

# Table 1 (any contract staffing vs. none)
summary_table_binary <- create_summary_table(
  df,
  grouping_var = any_contract,
  caption = "Facility characteristics by contract staffing use, mean (SE)",
  col_names = c("", "None", "Any")
)
writeLines(summary_table_binary$table, "output/table1.md")

# Table 2 (quartiles of contract staffing hours)
summary_table_quartiles <- create_summary_table(
  df,
  grouping_var = share_hrs_all_quartile,
  caption = "Facility characteristics by quartile of share of contract staffing hours, mean (SE)",
  col_names = c("", "Q1", "Q2", "Q3", "Q4")
)
writeLines(summary_table_quartiles$table, "output/table2.md")

# Table 3 (quartiles of contract staffing duration)
summary_table_duration <- create_summary_table(
  df,
  grouping_var = months_contract_quartile,
  caption = "Facility characteristics by quartile of contract staffing months, mean (SE)",
  col_names = c("", "Q1", "Q2", "Q3", "Q4")
)
writeLines(summary_table_duration$table, "output/table3.md")

# Love plot corresponding to comparisons in Table 1
love_plot_df <- summary_table_binary$df %>%
  mutate(
    none_numeric = as.numeric(str_remove(None, " \\(.*")),
    any_numeric = as.numeric(str_remove(Any, " \\(.*")))

love_plot_quality <- ggplot(subset(love_plot_df, Category %in% c("Overall Star Rating (1-5)", 
    "Quality Measure Rating (1-5)", "Staffing Rating (1-5)", "Health Inspection Rating (1-5)")),
  aes(y = Category)) +
  geom_point(aes(x = none_numeric, color = "None"), size = 3) +
  geom_point(aes(x = any_numeric, color = "Any"), size = 3) +
  scale_color_manual(
    name = "Group", 
    values = c("None" = "dodgerblue", "Any" = "orange")
  ) +
  scale_x_continuous(limits = c(1, 5)) +
  xlab("Star Rating") +
  ylab("Category") +
  ggtitle("Quality Star Ratings by Contract Staffing Use") +
  theme_minimal(
  ) +
  theme(
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  )

ggsave("output/quality_comparison.png", plot = love_plot_quality, width = 8, height = 4, dpi = 300)

# Love plot corresponding to Table 2 (quartiles)
love_plot_df <- summary_table_quartiles$df %>%
  mutate(
    q1 = as.numeric(str_remove(`1`, " \\(.*")),
    q2 = as.numeric(str_remove(`2`, " \\(.*")),
    q3 = as.numeric(str_remove(`3`, " \\(.*")),
    q4 = as.numeric(str_remove(`4`, " \\(.*"))
  )

love_plot_quality_quartiles <- ggplot(subset(love_plot_df, Category %in% c("Overall Star Rating (1-5)", 
    "Quality Measure Rating (1-5)", "Staffing Rating (1-5)", "Health Inspection Rating (1-5)")),
  aes(y = Category)) +
  geom_point(aes(x = q1, color = "Q1"), size = 3) +
  geom_point(aes(x = q2, color = "Q2"), size = 3) +
  geom_point(aes(x = q3, color = "Q3"), size = 3) +
  geom_point(aes(x = q4, color = "Q4"), size = 3) +
  scale_color_manual(
    name = "Group", 
    values = c(Q1 = "#e2e287", Q2 = "#a1dab4", Q3 = "#41b6c4", Q4 = "#225ea8")
  ) +
  scale_x_continuous(limits = c(1, 5)) +
  xlab("Star Rating") +
  ylab("Category") +
  ggtitle("Quality Star Ratings by Contract Staffing Quartile") +
  theme_minimal(
  ) +
  theme(
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  )

ggsave("output/quality_comparison_quartiles.png", plot = love_plot_quality_quartiles, width = 8, height = 4, dpi = 300)