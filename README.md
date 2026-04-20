# Replication Package
---
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.19651609.svg)](https://doi.org/10.5281/zenodo.19651609)

**"Zero fare, cleaner air? The causal effect of Luxembourg's free public transportation on transport emissions"**

---

### Authors

**Tobias Eibinger**, University of Graz, AT

**Sachintha Fernando**, Martin Luther University Halle-Wittenberg, DE

---

## Overview

This package contains all code and instructions necessary to replicate the results, 
tables, and figures in the paper. The analysis covers three main outcomes: 
transport emissions, traffic counts, and air quality. Statistical analysis and 
estimation are carried out in **Stata 18**; all graphs and figures 
are produced in **R 4.5.2**. The code has been tested on Windows (11) and macOS (26.4).

---

## Software Requirements

### Stata
- **Stata 18**
- The following user-written packages are required and can be installed via 
`ssc install` or `net install`:
  - `[sdid]` 
  - `[reghdfe]` 
  - `[ftools]`
  - `[require]`
  - `[estout]`


### R
- **R 4.5.2** (2025-10-31 ucrt) — *"[Not] Part in a Rumble"*
- Platform: `x86_64-w64-mingw32/x64` (Windows)
- Platform: `aarch64-apple-darwin20` (macOS)
- The following R packages are required:

```r
install.packages(c(
  "dplyr",
  "ggplot2",
  "tidyr",
  "zoo",
  "data.table",
  "haven",
  "sf",
  "terra",
  "raster",
  "osmextract",
  "geodata",
  "exactextractr",
  "spData",
  "maps",
  "RColorBrewer",
  "kableExtra",
  "knitr",
  "magrittr",
  "latticeExtra",
  "grid",
  "ggtext",
  "gridExtra",
  "cowplot",
  "patchwork",
  "ggpattern",
  "scales"
))
```

---

## Data and Codebook

This replication package contains **processed datasets** ready for analysis. 
The raw data underlying these files is not included in the package but is 
publicly available. Data sources are listed in ``Data-sources.html``. 
All variables used in the replication package are listed and described in 
``codebook.csv``. Please refer to the paper for further details and contact the
authors for remaining queries.

---

## Folder Structure

```
replication_package/
├── 00_master.do                     # Entry point: sets global paths and runs all Stata do-files in sequence
├── 10_data_raw/                     # Raw CO2 emission data and NUTS2 shapefiles used for mapping figures
├── 20_data_derived/                 # Contains traffic count data for descriptive plot
├── 30_data_analysis/                # Final analysis-ready datasets used for estimation and plotting
├── 50_code_analysis_descriptives/   # R scripts producing descriptive statistics and summary figures
├── 51_code_analysis_emissions/      # Stata estimation scripts and R plotting scripts for transport emissions
├── 52_code_analysis_traffic/        # Stata estimation scripts and R plotting scripts for traffic counts
├── 53_code_analysis_airquality/     # Stata estimation scripts and R plotting scripts for air quality
├── 60_results/                      # Output folder: all tables and figures are saved here
```



## How to Replicate

### Stata — Estimation & Tables

Open and execute `00_master.do` from within the replication package folder. 
The working directory is set automatically using `` `c(pwd)' ``, so no manual 
path configuration is needed. This will call all do-files in sequence 
and populate `60_results/`.

---

### R — Figures

Plotting scripts are found in each subfolder ``50_–53_`` and are named and 
numbered to match their corresponding figure in the paper. Note that Stata 
estimation scripts must be run first in numerical order, as the R scripts 
depend on datasets produced by them. Open and run each script from RStudio. 
The working directory is set automatically to the script's location using 
``rstudioapi::getActiveDocumentContext()$path``, so no manual path configuration 
is needed. Each script populates `60_results/`.

---

### Contact

For questions, comments, and feedback, please contact:

**Tobias Eibinger**,
University of Graz,
[tobias.eibinger@uni-graz.at] or 
[eibinger.tobias@gmail.com]

**Sachintha Fernando**,
Martin Luther University Halle-Wittenberg,
[sachintha.fernando@wiwi.uni-halle.de]

---

### License

Unless otherwise noted, materials in this replication package are licensed under CC BY 4.0.

Third-party data and source materials remain subject to their original licenses and terms of use.

*Last updated: [April, 2026]*
