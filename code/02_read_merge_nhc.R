library(tidyverse)
library(dplyr)
library(stringr)

# Read NHC data from CSV
df_prov <- read.csv("data/raw/nh_providerinfo_nov2024.csv")

df_prov <- df_prov %>%
  select(
    PROVNUM = CMS.Certification.Number..CCN.,
    fac_name = Provider.Name,
    county = County.Parish,
    state = State,
    ownership = Ownership.Type,
    hospbase = Provider.Resides.in.Hospital,
    n_cert_beds = Number.of.Certified.Beds,
    qm_rating = QM.Rating,
    overall_rating = Overall.Rating,
    health_inspection_rating = Health.Inspection.Rating,
    staffing_rating = Staffing.Rating,
    longstay_qm_rating = Long.Stay.QM.Rating,
    nursing_staff_turnover = Total.nursing.staff.turnover,
    rn_turnover = Registered.Nurse.turnover,
    na_hprd = Reported.Nurse.Aide.Staffing.Hours.per.Resident.per.Day,
    lpn_hprd = Reported.LPN.Staffing.Hours.per.Resident.per.Day,
    rn_hprd = Reported.RN.Staffing.Hours.per.Resident.per.Day
  ) %>%
  mutate(
    PROVNUM = as.character(PROVNUM),
    PROVNUM = str_pad(PROVNUM, width = 6, side = "left", pad = "0")
  )

# Collapse aggregated PBJ to facility level (one row per facility)
df_pbj <- read.csv("data/intermediate/monthly_pbj.csv")
agg_pbj <- df_pbj %>%
    group_by(PROVNUM) %>%
    summarize(
        avg_share_hrs_all = mean(share_hrs_all, na.rm = TRUE),
        avg_share_hrs_rn = mean(share_hrs_rn, na.rm = TRUE),
        avg_share_hrs_lpn = mean(share_hrs_lpn, na.rm = TRUE),
        avg_share_hrs_cna = mean(share_hrs_cna, na.rm = TRUE),
        months_contract = sum(share_hrs_all > 0, na.rm = TRUE)
    )

# Merge NHC to PBJ 1:1 on provider ID
merged_nhc_pbj <- merge(agg_pbj, df_prov, by = "PROVNUM", all.y = TRUE)

# Save CSV
write.csv(merged_nhc_pbj, "data/intermediate/merged_nhc_pbj.csv", row.names = FALSE)