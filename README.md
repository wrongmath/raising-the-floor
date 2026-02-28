# Raising the Floor: Causal Evidence on Employment from the 2026 State Minimum Wage Increases

**Author:** Yeva Butovska  
**Institution:** Case Western Reserve University  
**Date:** February 2026  

---

## Abstract

On January 1, 2026, nineteen U.S. states raised their minimum wages while twelve states remained at the federal floor of $7.25 per hour — unchanged since 2009. This paper exploits this sharp, simultaneous policy divergence as a natural experiment to estimate the causal effect of minimum wage increases on employment in leisure and hospitality and retail trade. I employ a two-way fixed effects (TWFE) difference-in-differences design with state and time fixed effects, clustering standard errors at the state level. A continuous treatment specification using the dollar magnitude of each state's wage increase captures dose-response heterogeneity. I validate the identifying parallel trends assumption through a 24-month pre-treatment placebo event study. Results for retail trade show near-perfect pre-treatment parallel trends between treated and control states, strongly supporting the research design. Leisure and hospitality exhibits some pre-treatment divergence near the treatment date, which I attribute to possible anticipation effects and note as a limitation. As BLS employment data for January and February 2026 are not yet published at the time of writing, this paper constitutes a pre-registered research design; full DiD estimates will be produced upon data release expected in March 2026.

---

## Project Structure
```
raising-the-floor/
├── data/
│   ├── raw/          # Raw BLS API pulls (not tracked in Git)
│   └── clean/        # Cleaned panel dataset
├── scripts/
│   ├── 01_pull_data.R     # BLS API data pull
│   ├── 02_clean.R         # Panel construction and DiD variables
│   ├── 03_model.R         # Event study and pre-specified DiD models
│   └── 04_visualize.R     # All figures
├── output/
│   ├── figures/      # PNG figures
│   └── tables/       # Model objects and regression tables
├── report/
│   └── raising_the_floor.Rmd   # Full paper (R Markdown → PDF)
└── README.md
```

## Data Sources

| Source | Series | Access |
|--------|--------|--------|
| BLS Current Employment Statistics (CES) | State-level leisure & hospitality and retail trade employment | BLS Public API |
| BLS Local Area Unemployment Statistics (LAUS) | State monthly unemployment rates | BLS Public API |
| EPI Minimum Wage Tracker | State minimum wage levels, January 2026 | epi.org/minimum-wage-tracker |

## Replication

All data are pulled directly from the BLS API and are fully reproducible. To replicate:

1. Clone this repository
2. Add your BLS API key to a `.env` file in the project root: `BLS_API_KEY=your_key_here`
3. Install R dependencies: run `install.packages(c("tidyverse", "fixest", "modelsummary", "sf", "tigris", "lubridate", "blsAPI", "dotenv", "here", "scales", "patchwork", "broom", "zoo"))`
4. Run scripts in order: `01_pull_data.R` → `02_clean.R` → `03_model.R` → `04_visualize.R`
5. Knit `report/raising_the_floor.Rmd` to PDF

## Key Results

- **Retail trade** shows near-perfect parallel trends between treated and control states across all 24 pre-treatment months, strongly validating the DiD design for this sector
- **Leisure and hospitality** shows mostly parallel pre-trends with a significant deviation at $k = -2$ (November 2025), possibly reflecting anticipation effects
- The average wage increase among treated states is **$0.41/hour**, ranging from small inflation adjustments to Hawaii's **$2.00 increase**
- Full DiD estimates pending BLS release of January–February 2026 data (~March 2026)

## Methodology

- **Estimator:** Two-way fixed effects DiD (`fixest::feols`)
- **Fixed effects:** State + calendar month-year
- **Standard errors:** Clustered at the state level
- **Outcome variables:** Log employment in leisure & hospitality and retail trade
- **Covariate:** State monthly unemployment rate
- **Pre-trend test:** 24-month placebo event study with December 2025 as reference period
- **Continuous treatment:** Dollar magnitude of wage increase interacted with post indicator

## Literature

- Card & Krueger (1994) — *Minimum Wages and Employment*
- Dube, Lester & Reich (2010) — *Minimum Wage Effects Across State Borders*
- Callaway & Sant'Anna (2021) — *Difference-in-Differences with Multiple Time Periods*
- Sun & Abraham (2021) — *Estimating Dynamic Treatment Effects in Event Studies*

