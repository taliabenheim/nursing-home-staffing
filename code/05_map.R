library(tidyverse)
library(remotes)
library(tigris)
library(sf)
library(viridis)
options(tigris_use_cache = TRUE)
options(tigris_protocol = "ftp")

# Read data
df <- read.csv("data/intermediate/monthly_pbj.csv")

# Prepare for mapping
df_county <- df %>%
  # Drop if outside contiguous U.S.
  filter(!STATE %in% c("AK", "HI")) %>%
  # Restrict to July 2023
  filter(year_month == "2023-07") %>%
  # Aggregate to county level
  group_by(year_month, STATE, COUNTY_FIPS) %>%
  summarize(
    county_avg_share_hrs_all = mean(share_hrs_all, na.rm = TRUE),
    county_avg_share_hrs_rn = mean(share_hrs_rn, na.rm = TRUE),
    county_avg_share_hrs_lpn = mean(share_hrs_lpn, na.rm = TRUE),
    county_avg_share_hrs_cna = mean(share_hrs_cna, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(state = STATE, county_fips = COUNTY_FIPS) %>%
  mutate(fips = sprintf("%03s", county_fips))

# Merge in state FIPS code
data("fips_codes")
df_county <- df_county %>%
  left_join(fips_codes, by = c("state" = "state", "fips" = "county_code"))

# Concatenate state and county code
df_county$fips_complete <- paste0(df_county$state_code, df_county$fips)

# Get county shapefile from tigris package
county_shp <- counties(cb = TRUE, year = 2021, class = "sf")

# Restrict to Northeast states
selected_counties <- county_shp %>%
  filter(STATEFP %in% c("09", "23", "25", "33", "44", "50"))

# Merge with shapefile
county_data <- selected_counties %>%
  left_join(df_county, by = c("GEOID" = "fips_complete"))

# Map of contract staffing by county
map_contract_staffing <- ggplot(county_data) +
  geom_sf(aes(fill = county_avg_share_hrs_all)) +
  scale_fill_viridis(
    name = "Contract Staffing",
    na.value = "grey80"
  ) +
  labs(
    title = "% Contract Staffing Hours in New England Counties, July 2023",
    subtitle = "(Grey = missing)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )

# Save map
ggsave("output/northeast_map.png", plot = map_contract_staffing, width = 12, height = 10, dpi = 300)