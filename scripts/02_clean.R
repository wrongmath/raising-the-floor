# ============================================================
# 02_clean.R
# Raising the Floor: Causal Evidence on Employment from the
# 2026 State Minimum Wage Increases
#
# Purpose: Build clean panel dataset from raw BLS pulls
#   - Parse dates, extract state from series ID
#   - Join leisure, retail, and unemployment into one panel
#   - Add treatment/control indicators
#   - Add minimum wage levels per state
# ============================================================

library(tidyverse)
library(lubridate)
library(here)
library(zoo)

# ============================================================
# LOAD RAW DATA
# ============================================================

leisure_raw <- read_csv(here("data/raw/leisure_raw.csv"), col_types = cols(.default = "c"))
retail_raw  <- read_csv(here("data/raw/retail_raw.csv"),  col_types = cols(.default = "c"))
laus_raw    <- read_csv(here("data/raw/laus_raw.csv"),    col_types = cols(.default = "c"))

# ============================================================
# STATE METADATA
# Treatment status and minimum wage levels
# ============================================================

state_meta <- tibble(
  state = c(
    # Treatment states
    "AZ","CA","CO","CT","HI","ME","MI","MN","MO","MT",
    "NE","NJ","NY","OH","RI","SD","VT","VA","WA",
    # Control states
    "AL","GA","ID","IN","KY","LA","MS","NC","SC","TN","TX","WY"
  ),
  fips = c(
    "04","06","08","09","15","23","26","27","29","30",
    "31","34","36","39","44","46","50","51","53",
    "01","13","16","18","21","22","28","37","45","47","48","56"
  ),
  treated = c(
    rep(1, 19),  # treatment states
    rep(0, 12)   # control states
  ),
  # Minimum wage as of Jan 2026 (post) and Dec 2025 (pre)
  # Source: EPI Minimum Wage Tracker
  min_wage_post = c(
    14.70, 16.50, 14.81, 16.35, 14.00, 14.65, 10.56, 11.13,
    13.75, 10.55, 13.50, 15.49, 15.50, 10.70, 15.00, 11.20,
    14.01, 12.41, 16.66,
    7.25, 7.25, 7.25, 7.25, 7.25, 7.25, 7.25, 7.25, 7.25, 7.25, 7.25, 7.25
  ),
  min_wage_pre = c(
    14.35, 16.00, 14.42, 15.69, 14.00, 14.15, 10.33, 10.85,
    12.30, 10.30, 13.50, 15.13, 15.00, 10.45, 14.00, 11.20,
    13.67, 12.00, 16.28,
    7.25, 7.25, 7.25, 7.25, 7.25, 7.25, 7.25, 7.25, 7.25, 7.25, 7.25, 7.25
  )
) %>%
  mutate(
    # Continuous treatment: dollar magnitude of wage increase
    wage_change = min_wage_post - min_wage_pre
  )

# ============================================================
# HELPER: PARSE BLS DATA
# Extracts state fips from series ID, converts value to numeric
# Removes annual average rows (period == "M13")
# ============================================================

parse_bls <- function(df, value_col) {
  df %>%
    filter(period != "M13") %>%
    mutate(
      fips = case_when(
        str_starts(seriesID, "SMU")   ~ str_sub(seriesID, 4, 5),
        str_starts(seriesID, "LAUST") ~ str_sub(seriesID, 6, 7),
        TRUE ~ NA_character_
      ),
      month      = as.integer(str_remove(period, "M")),
      year       = as.integer(year),
      date       = ymd(paste(year, month, "01", sep = "-")),
      !!value_col := as.numeric(value)
    ) %>%
    filter(!is.na(!!sym(value_col))) %>%
    select(fips, date, year, month, !!value_col)
}

# ============================================================
# PARSE EACH DATASET
# ============================================================

leisure_clean <- parse_bls(leisure_raw, "emp_leisure")
retail_clean  <- parse_bls(retail_raw,  "emp_retail")
laus_clean    <- parse_bls(laus_raw,    "unemp_rate")

# ============================================================
# JOIN INTO PANEL
# ============================================================

panel <- leisure_clean %>%
  full_join(retail_clean, by = c("fips", "date", "year", "month")) %>%
  full_join(laus_clean,   by = c("fips", "date", "year", "month")) %>%
  left_join(state_meta,   by = "fips") %>%
  arrange(state, date)

# ============================================================
# ADD DiD VARIABLES
# ============================================================

panel <- panel %>%
  mutate(
    # Post indicator: 1 if Jan 2026 or later
    post          = if_else(date >= ymd("2026-01-01"), 1L, 0L),
    # DiD interaction term
    treated_post  = treated * post,
    # Continuous DiD: wage change x post
    wage_change_post = wage_change * post,
    # Time index (months since Jan 2024, for event study)
    time_index    = (year - 2024) * 12 + month - 1,
    # Event time: months relative to treatment (Jan 2026 = 0)
    event_time    = time_index - 24
  )

# ============================================================
# QUICK CHECKS
# ============================================================

message("Panel dimensions: ", nrow(panel), " rows x ", ncol(panel), " cols")
message("States in panel: ", n_distinct(panel$state))
message("Date range: ", min(panel$date), " to ", max(panel$date))
message("Treatment/control split:")
panel %>%
  distinct(state, treated) %>%
  count(treated) %>%
  print()

message("Missing values per column:")
panel %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything()) %>%
  filter(value > 0) %>%
  print()

# ============================================================
# INTERPOLATE MISSING OCTOBER 2025 UNEMPLOYMENT
# BLS LAUS data for Oct 2025 missing across all states
# Fill with linear interpolation between Sep and Nov 2025
# ============================================================

panel <- panel %>%
  arrange(state, date) %>%
  group_by(state) %>%
  mutate(unemp_rate = zoo::na.approx(unemp_rate, na.rm = FALSE)) %>%
  ungroup()

# ============================================================
# SAVE CLEAN PANEL
# ============================================================

write_csv(panel, here("data/clean/panel.csv"))
message("Clean panel saved to data/clean/panel.csv")

