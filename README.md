# Studying Covariate Effects in ROC Curve

My independent work involved evaluating the framework against the **Indian Liver Records (ILPD)** dataset to diagnose hepatocellular injury.

* **Variable Analyzed**: Alamine Aminotransferase used as continouos diagnostic marker and Age as continouos covariate
* **Data Processing & EDA:** plotted conditional densities and estimated marker distributions across different age groups
* **Statistical Validation:** evaluated conditional AUC and average AUC with trapezoidal integration
* **Key Findings:** bootstrap procedure p-value>0.05 so we can accept the null hypothesis $H_0$

## Tools:
* **Language:** R
* **Libraries:** `ggplot2`, `ggExtra`, `hdrcde`, `pROC`, `pracma`, `viridis`
  
## References
* **Original Methodology**: Fanjul-Hevia, A., Pardo-Fernández, J. C., & González-Manteiga, W. (2025). *A New Test for Assessing the Covariate Effect in ROC Curves*. Statistics in Medicine
* **Data Sources**: BUPA Liver Disorders Dataset (UCI Machine Learning Repository) and Indian Liver Patient Records (Kaggle)
