# ============================================================
# 04_visualize.R
# Raising the Floor: Causal Evidence on Employment from the
# 2026 State Minimum Wage Increases
#
# Purpose: Generate all figures
#   1. Choropleth map of treatment states and wage changes
#   2. Parallel trends plot (leisure & retail)
#   3. Event study plot (leisure & retail)
# ============================================================

library(tidyverse)
library(fixest)
library(sf)
library(tigris)
library(scales)
library(patchwork)
library(broom)
library(here)

options(tigris_use_cache = TRUE)

# ============================================================
# LOAD DATA AND MODELS
# ============================================================

panel <- read_csv(here("data/clean/panel.csv"), col_types = cols(.default = "c")) %>%
  mutate(
    across(c(emp_leisure, emp_retail, unemp_rate, treated, post,
             treated_post, wage_change, wage_change_post,
             time_index, event_time, min_wage_post, min_wage_pre),
           as.numeric),
    date  = as.Date(date),
    state = as.character(state),
    log_leisure = log(as.numeric(emp_leisure)),
    log_retail  = log(as.numeric(emp_retail))
  )

es_leisure <- readRDS(here("output/tables/es_leisure.rds"))
es_retail  <- readRDS(here("output/tables/es_retail.rds"))

# ============================================================
# THEME
# ============================================================

theme_raising <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(color = "grey40", size = 10),
      plot.caption     = element_text(color = "grey50", size = 8),
      axis.title       = element_text(size = 10),
      legend.position  = "bottom",
      panel.grid.minor = element_blank(),
      strip.text       = element_text(face = "bold")
    )
}

# ============================================================
# FIGURE 1: CHOROPLETH MAP
# ============================================================

message("Building choropleth map...")

states_sf <- states(cb = TRUE, resolution = "20m", year = 2023) %>%
  filter(!STUSPS %in% c("AK", "HI", "PR", "GU", "VI", "MP", "AS")) %>%
  select(STUSPS, geometry)

map_data <- panel %>%
  distinct(state, treated, wage_change, min_wage_post) %>%
  mutate(
    map_category = case_when(
      treated == 0 ~ "Control ($7.25)",
      wage_change < 0.50 ~ "Increase < $0.50",
      wage_change < 1.00 ~ "Increase $0.50–$0.99",
      TRUE ~ "Increase ≥ $1.00"
    ),
    map_category = factor(map_category, levels = c(
      "Control ($7.25)",
      "Increase < $0.50",
      "Increase $0.50–$0.99",
      "Increase ≥ $1.00"
    ))
  )

map_sf <- states_sf %>%
  left_join(map_data, by = c("STUSPS" = "state")) %>%
  mutate(
    map_category = as.character(map_category),
    map_category = if_else(is.na(map_category), "Not in Analysis", map_category),
    map_category = factor(map_category, levels = c(
      "Control ($7.25)",
      "Increase < $0.50",
      "Increase $0.50–$0.99",
      "Increase ≥ $1.00",
      "Not in Analysis"
    ))
  )

fig1 <- ggplot(map_sf) +
  geom_sf(aes(fill = map_category), color = "white", linewidth = 0.3) +
  scale_fill_manual(
    values = c(
      "Control ($7.25)"      = "#888888",
      "Increase < $0.50"     = "#a8c8e8",
      "Increase $0.50–$0.99" = "#4a90c4",
      "Increase ≥ $1.00"     = "#1a5a8a",
      "Not in Analysis"      = "#f0f0f0"
    ),
    name = NULL
  ) +
  labs(
    title    = "Treatment Status by State: January 2026 Minimum Wage Increases",
    subtitle = "19 states raised their minimum wage on January 1, 2026; 12 control states remain at federal $7.25",
    caption  = "Source: EPI Minimum Wage Tracker. States not included in analysis (e.g. AK, HI) shown in light grey."
  ) +
  theme_raising() +
  theme(
    axis.text       = element_blank(),
    axis.title      = element_blank(),
    panel.grid      = element_blank(),
    legend.position = "bottom"
  )

ggsave(here("output/figures/fig1_map.png"), fig1,
       width = 10, height = 6, dpi = 300)
message("Figure 1 saved.")

# ============================================================
# FIGURE 2: PARALLEL TRENDS
# ============================================================

message("Building parallel trends plot...")

trends <- panel %>%
  group_by(date, treated) %>%
  summarise(
    mean_leisure = mean(log_leisure, na.rm = TRUE),
    mean_retail  = mean(log_retail,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(group = if_else(treated == 1, "Treated States", "Control States"))

baseline <- trends %>%
  filter(date == ymd("2024-01-01")) %>%
  select(treated, base_leisure = mean_leisure, base_retail = mean_retail)

trends <- trends %>%
  left_join(baseline, by = "treated") %>%
  mutate(
    norm_leisure = mean_leisure - base_leisure,
    norm_retail  = mean_retail  - base_retail
  )

p2a <- ggplot(trends, aes(x = date, y = norm_leisure,
                          color = group, linetype = group)) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = ymd("2026-01-01"),
             linetype = "dotted", color = "grey30", linewidth = 0.5) +
  annotate("text", x = ymd("2026-01-01"), y = Inf,
           label = "Treatment →", hjust = 1.1, vjust = 1.5,
           size = 3, color = "grey30") +
  scale_color_manual(values = c("Treated States" = "#1a5a8a",
                                "Control States"  = "#c0392b")) +
  scale_linetype_manual(values = c("Treated States" = "solid",
                                   "Control States"  = "dashed")) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  labs(
    title    = "Leisure & Hospitality Employment",
    subtitle = "Log employment normalized to January 2024 = 0",
    x = NULL, y = "Log Employment (normalized)",
    color = NULL, linetype = NULL
  ) +
  theme_raising() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2b <- ggplot(trends, aes(x = date, y = norm_retail,
                          color = group, linetype = group)) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = ymd("2026-01-01"),
             linetype = "dashed", color = "grey30", linewidth = 0.6) +
  annotate("text", x = ymd("2026-01-01"), y = Inf,
           label = "Treatment\n(Jan 2026)", hjust = -0.1, vjust = 1.5,
           size = 3, color = "grey30") +
  scale_color_manual(values = c("Treated States" = "#1a5a8a",
                                "Control States"  = "#c0392b")) +
  scale_linetype_manual(values = c("Treated States" = "solid",
                                   "Control States"  = "dashed")) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  labs(
    title    = "Retail Trade Employment",
    subtitle = "Log employment normalized to January 2024 = 0",
    x = NULL, y = "Log Employment (normalized)",
    color = NULL, linetype = NULL
  ) +
  theme_raising() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

fig2 <- p2a + p2b +
  plot_annotation(
    title   = "Parallel Trends: Treated vs. Control States, Jan 2024 – Dec 2025",
    caption = "Source: BLS Current Employment Statistics. Dashed vertical line marks January 2026 treatment date.",
    theme   = theme(plot.title = element_text(face = "bold", size = 14))
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(here("output/figures/fig2_parallel_trends.png"), fig2,
       width = 12, height = 5, dpi = 300)
message("Figure 2 saved.")

# ============================================================
# FIGURE 3: EVENT STUDY PLOT
# ============================================================

message("Building event study plot...")

extract_es <- function(model, outcome_label) {
  broom::tidy(model, conf.int = TRUE) %>%
    filter(str_detect(term, "event_time")) %>%
    mutate(
      event_time = as.integer(str_extract(term, "-?\\d+")),
      outcome    = outcome_label
    ) %>%
    select(event_time, estimate, conf.low, conf.high, outcome)
}

es_df <- bind_rows(
  extract_es(es_leisure, "Leisure & Hospitality"),
  extract_es(es_retail,  "Retail Trade")
) %>%
  bind_rows(
    tibble(event_time = -1, estimate = 0, conf.low = 0, conf.high = 0,
           outcome = "Leisure & Hospitality"),
    tibble(event_time = -1, estimate = 0, conf.low = 0, conf.high = 0,
           outcome = "Retail Trade")
  )

# Create month label lookup
month_labels <- panel %>%
  distinct(event_time, date) %>%
  filter(event_time %in% seq(-24, 0, by = 4)) %>%
  arrange(event_time) %>%
  mutate(label = format(date, "%b %Y"))

fig3 <- ggplot(es_df, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -0.5, linetype = "dotted",
             color = "grey30", linewidth = 0.5) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = "95% Confidence Interval"),
              alpha = 0.15) +
  geom_line(aes(color = "Estimated Coefficient"), linewidth = 0.8) +
  geom_point(aes(color = "Estimated Coefficient"), size = 1.8) +
  facet_wrap(~outcome, scales = "free_y") +
  scale_x_continuous(
    breaks = month_labels$event_time,
    labels = month_labels$label
  ) +
  scale_color_manual(
    name   = NULL,
    values = c("Estimated Coefficient" = "#1a5a8a")
  ) +
  scale_fill_manual(
    name   = NULL,
    values = c("95% Confidence Interval" = "#1a5a8a")
  ) +
  annotate("text", x = -0.5, y = Inf,
           label = "Treatment →", hjust = 1.1, vjust = 1.5,
           size = 3, color = "grey30") +
  labs(
    title    = "Pre-Treatment Placebo Event Study",
    subtitle = "Coefficients represent differential trend in treated vs. control states relative to Dec 2025\nCoefficients near zero support the parallel trends assumption",
    x        = NULL,
    y        = "Estimated Coefficient (log employment)",
    caption  = "Source: BLS CES & LAUS. 95% confidence intervals shown. Standard errors clustered by state."
  ) +
  theme_raising() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here("output/figures/fig3_event_study.png"), fig3,
       width = 12, height = 5, dpi = 300)
message("Figure 3 saved.")

message("All figures saved to output/figures/")