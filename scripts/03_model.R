# ============================================================
# 03_model.R
# Raising the Floor: Causal Evidence on Employment from the
# 2026 State Minimum Wage Increases
#
# Purpose: Pre-treatment analysis and model specification
#
# NOTE: BLS CES data for Jan-Feb 2026 not yet published as of
# Feb 2026. This script estimates the pre-treatment placebo
# event study to validate the parallel trends assumption, and
# pre-specifies the full DiD models for execution upon data
# release. This constitutes a pre-registered research design.
# ============================================================

library(tidyverse)
library(fixest)
library(modelsummary)
library(here)

# ============================================================
# LOAD PANEL
# ============================================================

panel <- read_csv(here("data/clean/panel.csv"), col_types = cols(.default = "c")) %>%
  mutate(
    across(c(emp_leisure, emp_retail, unemp_rate, treated, post,
             treated_post, wage_change, wage_change_post,
             time_index, event_time, min_wage_post, min_wage_pre),
           as.numeric),
    date  = as.Date(date),
    state = as.character(state)
  ) %>%
  mutate(
    log_leisure = log(emp_leisure),
    log_retail  = log(emp_retail)
  )

# ============================================================
# CONFIRM DATA COVERAGE
# ============================================================

message("Data coverage: ", min(panel$date), " to ", max(panel$date))
message("Post-treatment obs: ", sum(panel$post))
message("All observations are pre-treatment — running placebo analysis")

# ============================================================
# PRE-TREATMENT PLACEBO EVENT STUDY
# Tests parallel trends assumption by checking whether
# treated and control states trended differently BEFORE
# the Jan 2026 treatment
# A valid design shows coefficients near zero with wide CIs
# We use Dec 2024 (event_time = -1) as the reference month
# ============================================================

message("Estimating pre-treatment placebo event study...")

es_leisure <- feols(
  log_leisure ~ i(event_time, treated, ref = -1) + unemp_rate | state + date,
  data    = panel,
  cluster = ~state
)

es_retail <- feols(
  log_retail ~ i(event_time, treated, ref = -1) + unemp_rate | state + date,
  data    = panel,
  cluster = ~state
)

summary(es_leisure)
summary(es_retail)

# ============================================================
# PARALLEL TRENDS SUMMARY STATS
# Check if pre-treatment coefficients are jointly zero
# ============================================================

message("\nLeisure: Wald test for joint significance of pre-treatment coefs")
wald(es_leisure, keep = "event_time")

message("\nRetail: Wald test for joint significance of pre-treatment coefs")
wald(es_retail, keep = "event_time")

# ============================================================
# PRE-SPECIFIED FULL DiD MODELS
# These will be estimated once Jan-Feb 2026 data is released
# Included here to document the pre-registered specification
# ============================================================

message("\n--- PRE-SPECIFIED MODELS (awaiting post-treatment data) ---")
message("
  # Model 1: Baseline TWFE DiD
  twfe_leisure <- feols(
    log_leisure ~ treated_post + unemp_rate | state + date,
    data = panel, cluster = ~state
  )

  # Model 2: Continuous treatment DiD
  cont_leisure <- feols(
    log_leisure ~ wage_change_post + unemp_rate | state + date,
    data = panel, cluster = ~state
  )
")

# ============================================================
# SAVE EVENT STUDY MODELS
# ============================================================

saveRDS(es_leisure, here("output/tables/es_leisure.rds"))
saveRDS(es_retail,  here("output/tables/es_retail.rds"))

# ============================================================
# DESCRIPTIVE SUMMARY TABLE
# ============================================================

desc_table <- panel %>%
  group_by(treated) %>%
  summarise(
    n_states        = n_distinct(state),
    avg_leisure     = round(mean(emp_leisure), 1),
    avg_retail      = round(mean(emp_retail), 1),
    avg_unemp       = round(mean(unemp_rate), 2),
    avg_min_wage    = round(mean(min_wage_post), 2),
    avg_wage_change = round(mean(wage_change), 2)
  )

print(desc_table)
write_csv(desc_table, here("output/tables/descriptive_stats.csv"))

message("Models and descriptive stats saved to output/tables/")