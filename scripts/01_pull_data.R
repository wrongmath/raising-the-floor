# ============================================================
# 01_pull_data.R
# Raising the Floor: Causal Evidence on Employment from the
# 2026 State Minimum Wage Increases
#
# Purpose: Pull BLS data via API
#   - CES: Monthly state employment, Leisure & Hospitality
#   - CES: Monthly state employment, Retail Trade
#   - LAUS: Monthly state unemployment rates
# Date range: January 2024 - February 2026
# ============================================================

library(tidyverse)
library(blsAPI)
library(rjson)
library(dotenv)
library(here)

# Load API key from .env
load_dot_env(here(".env"))
api_key <- Sys.getenv("BLS_API_KEY")

# ============================================================
# STATE FIPS CODES
# Treatment: states raising min wage Jan 1 2026
# Control: states frozen at federal $7.25
# ============================================================

state_fips <- list(
  # Treatment states
  AZ = "04", CA = "06", CO = "08", CT = "09",
  HI = "15", ME = "23", MI = "26", MN = "27",
  MO = "29", MT = "30", NE = "31", NJ = "34",
  NY = "36", OH = "39", RI = "44", SD = "46",
  VT = "50", VA = "51", WA = "53",
  # Control states (federal $7.25)
  AL = "01", GA = "13", ID = "16", IN = "18",
  KY = "21", LA = "22", MS = "28", NC = "37",
  SC = "45", TN = "47", TX = "48", WY = "56"
)

# ============================================================
# SERIES ID BUILDERS
# CES:  SMU + state_fips + 00000 + industry_code + 001
#   Leisure & Hospitality industry code: 7000000
#   Retail Trade industry code:          4200000
# LAUS: LAUST + state_fips + 0000000000003 = unemployment rate
# ============================================================

build_ces_series <- function(fips, industry_code) {
  paste0("SMU", fips, "00000", industry_code, "001")
}

build_laus_series <- function(fips) {
  paste0("LAUST", fips, "0000000000003")
}

leisure_series <- map_chr(state_fips, ~build_ces_series(.x, "7000000"))
retail_series  <- map_chr(state_fips, ~build_ces_series(.x, "4200000"))
laus_series    <- map_chr(state_fips, ~build_laus_series(.x))

# ============================================================
# BLS API PULL
# Max 50 series per request — batch into groups of 25
# ============================================================

pull_bls <- function(series_ids, start_year = "2024", end_year = "2026") {
  payload <- list(
    seriesid        = unname(series_ids),
    startyear       = start_year,
    endyear         = end_year,
    registrationkey = api_key
  )
  response <- blsAPI(payload, api_version = 2, return_data_frame = TRUE)
  return(response)
}

batch <- function(lst, size = 25) {
  split(lst, ceiling(seq_along(lst) / size))
}

message("Pulling Leisure & Hospitality data...")
leisure_raw <- map_dfr(batch(leisure_series), pull_bls)

message("Pulling Retail Trade data...")
retail_raw <- map_dfr(batch(retail_series), pull_bls)

message("Pulling Unemployment Rate data...")
laus_raw <- map_dfr(batch(laus_series), pull_bls)

# ============================================================
# SAVE RAW DATA
# ============================================================

write_csv(leisure_raw, here("data/raw/leisure_raw.csv"))
write_csv(retail_raw,  here("data/raw/retail_raw.csv"))
write_csv(laus_raw,    here("data/raw/laus_raw.csv"))

message("Done. Raw data saved to data/raw/")
