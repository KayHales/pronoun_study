Title: Contextualizing Pronoun Use
Authors: Ellen D.B. Riggle, Zak Clements, Kay Hales, Paz Galupo

---

### Overview
This repository contains all scripts and functions used to conduct the analyses for the manuscript listed above. Analyses include:

- Data cleaning and preparation
- Construction and validation of emotion composite variables
- ANOVAs for They+ pronoun groups
- Descriptive statistics and Games-Howell post hoc comparisons
- Mixed-effects models exploring Pronoun × Race (Black vs. White) interactions

---

### File Structure

```
project/
├── full_analysis.R            # Main analysis script
├── utility_functions.R        # Reusable functions and label definitions
├── pronoundata_public.Rds    # Cleaned dataset used in analysis
```

---

### Requirements

This project requires R (>= 4.2) and the following packages:
- tidyverse
- haven
- sjlabelled
- rstatix
- lme4
- car
- emmeans
- psych
- broom
- effectsize
- gt
- crayon

Install with:
```r
install.packages(c(
  "tidyverse", "haven", "sjlabelled", "rstatix", "lme4", "car",
  "emmeans", "psych", "broom", "effectsize", "gt", "crayon"
))
```

---

### Running the Analysis

1. Open `full_analysis.R`
2. Ensure `pronoundata_public.csv` is in your working directory
3. Source `utility_functions.R`
4. Run the script from top to bottom

---


### Session Snapshot

R version 4.4.1 (2024-06-14 ucrt)
Platform: x86_64-w64-mingw32/x64
Running under: Windows 11 x64 (build 26100)

Matrix products: default


locale:
[1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8   
[3] LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                          
[5] LC_TIME=English_United States.utf8    

time zone: America/New_York
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices datasets  utils     methods   base     

other attached packages:
 [1] car_3.1-3        carData_3.0-5    effectsize_0.8.9 pwr_1.3-0        sandwich_3.1-1  
 [6] gt_0.11.1        gtsummary_2.0.4  lme4_1.1-35.5    brms_2.22.0      Rcpp_1.0.13-1   
[11] glmnet_4.1-8     Matrix_1.7-0     rstatix_0.7.2    lubridate_1.9.3  forcats_1.0.0   
[16] stringr_1.5.1    dplyr_1.1.4      purrr_1.0.2      readr_2.1.5      tidyr_1.3.1     
[21] tibble_3.2.1     ggplot2_3.5.1    tidyverse_2.0.0  haven_2.5.4      sjlabelled_1.2.0
[26] devtools_2.4.5   usethis_3.1.0    emmeans_1.10.5  
 
