set.seed(123)

library(tidyverse)
library(patchwork)

# Read data
df_long <- read_csv("data/intermediate/monthly_pbj.csv", show_col_types = FALSE) %>%
    # Randomly select facilities
    filter(PROVNUM %in% sample(unique(PROVNUM), 60)) %>%
    # Pivot data to long format for plotting
    pivot_longer(
        cols = starts_with("share_hrs"),
        names_to = "staff_type",
        values_to = "share_hrs"
    ) %>%
    mutate(
        staff_type = factor(
            staff_type,
            levels = c("share_hrs_all", "share_hrs_rn", "share_hrs_lpn", "share_hrs_cna")
        )
    )

# Labels for staff types
staff_labels <- c(
    share_hrs_all = "All",
    share_hrs_rn = "RN",
    share_hrs_lpn = "LPN",
    share_hrs_cna = "CNA"
)

# Plot share of contract staffing hours by staff type
combined_plot <- ggplot(df_long, aes(x = year_month, y = share_hrs, group = PROVNUM, color = PROVNUM)) +
    geom_line() +
    facet_wrap(~staff_type, scales = "fixed", labeller = as_labeller(staff_labels)) +
    ylim(0, 1) +
    labs(
        x = "Month",
        y = "Share of Contract Staffing Hours",
        title = "Monthly Share of Contract Staffing Hours in 60 Randomly Selected Facilities, by Staff Type"
    ) +
    theme_minimal() +
    theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        strip.text = element_text(size = 10, face = "bold"),
        panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
    ) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

print(combined_plot)

# Save combined plot
ggsave("output/figure1.png", plot = combined_plot, width = 14, height = 10, units = "in", dpi = 300)