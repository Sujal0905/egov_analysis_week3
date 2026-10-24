# E-Governance Data Analysis in R: UN E-Government Development Index (EGDI) 2024

Three-week internship project: acquisition, transformation and visualization of public e-governance data.

**Main dataset:** UN DESA E-Government Development Index 2024 (193 UN Member States)
Source: https://publicadministration.un.org/egovkb/Data-Center

## Structure
| Path | Description |
|---|---|
| `egov_analysis.R` | Week 1: import, cleaning, EDA, five charts, Wilcoxon test |
| `week2_transform.R` | Week 2: dplyr/tidyr pipeline (joins, derived variables, aggregation, reshaping) |
| `week3_visualize.R` | Week 3: seven ggplot2 charts with design iteration |
| `data/` | Raw, lookup (regions, South Asia peers), cleaned and transformed CSVs |
| `figures/` | All charts (fig*.png Week 1, w2_* pipeline, w3_* Week 3) |
| `reports/` | Word reports for Weeks 1 to 3 |

## How to run (from the project folder, in this order)
```r
install.packages(c("readr", "dplyr", "tidyr", "ggplot2", "scales", "stringr"))
source("egov_analysis.R")
source("week2_transform.R")
source("week3_visualize.R")
```

## Notes
- `data/country_regions.csv` (UN regions) and `data/south_asia_peers.csv` (SAARC members) are lookup tables compiled for this project.
- Regional means from the transformed data were checked against the UN 2024 Survey's published regional averages.
