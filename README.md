# Occupational and Demographic Factors for Firefighter Mental Health Outcomes

## Description
This repository contains the R code (`FF Mental Health Data.R`) for performing multiple logistic regression analyses on demographic features and occupation specific experiences/scenarios to determine the risk factors that directly lead to the development of depression, anxiety, stress, post-traumatic stress disorder, and cognitive failure.

## How to Use
1.  Ensure you have R and the following packages installed: `dyplr`, `tidyr`, `janior`, `ggplot2`, `rms`, `reshape2`, `mass`, `car`, `pROC`, `ResourceSelection`, `broom`, `lmtest`, `table1`, and `purr`.
2.  Download the `FF Mental Health Data Analysis.R` script.
3.  Run the script in RStudio or your R environment.

## Code Overview
The main script includes:
- Data cleaning and preparation
- Logistic regression model fitting
- Results summary and visualization

```r
# Example snippet of the code
library(dyplr)
FF <- read.csv(insert dataset file directory)
```
