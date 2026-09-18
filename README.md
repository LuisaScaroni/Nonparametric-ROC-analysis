# Studying Covariate Effects in ROC Curve
This repository contains an empirical application and validation of the nonparametric test proposed by Fanjul-Hevia et al.(2025) ROC curves.

The theoretical framework and core testing functions reproduce the methodology described in the original paper. The original contribution of this repository focuses on applying, testing, and validating these models on real-world, noisy medical datasets to see how they behave with real data (BUPA and ILPD).

## Project Overiview
Covariates can influence the diagnostic capability of a marker. The project studies if it is strictly necessary to adjust for these covariates or if standard pooled ROC is sufficient.

* **Goal:** compare pooled ROC, conditional ROC and covariate-adjusted ROC
* **Testing Procedure:** apply bootstrap based hypothesis test to check the null hypothesis $H_0:AROC(p)=ROC(p)$

My independent work involved evaluating the framework against the **Indian Liver Records (ILPD)** dataset to diagnose hepatocellular injury.

* **Variable Analyzed**: Alamine Aminotransferase used as continouos diagnostic marker and Age as continouos covariate
* **Data Processing & EDA:** plotted conditional densities and estimated marker distributions across different age groups
* **Statistical Validation:** evaluated conditional AUC and average AUC with trapezoidal integration
* **Key Findings:** bootstrap procedure p-value>0.05 so we can accept the null hypothesis $H_0$

## Tools:
* **Language:** R
* **Libraries:** `ggplot2`, `ggExtra`, `hdrcde`, `pROC`, `pracma`, `viridis`
  
## Repository Contents:
* `ROC curve paper.pdf`: report about the paper and the study conducted on real world data by myself
* `ROC curve.R`: R code for studying how the methodology works on real world unperfect data

## References
* **Original Methodology**: Fanjul-Hevia, A., Pardo-Fernández, J. C., & González-Manteiga, W. (2025). *A New Test for Assessing the Covariate Effect in ROC Curves*. Statistics in Medicine
* **Data Sources**: BUPA Liver Disorders Dataset (UCI Machine Learning Repository) and Indian Liver Patient Records (Kaggle)
