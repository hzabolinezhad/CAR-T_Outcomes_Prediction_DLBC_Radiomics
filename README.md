# CAR-T_Outcomes_Prediction_DLBC_Radiomics
 This repository hosts R scripts developed in RStudio (v2023.12.1) for predicting CAR T-cell therapy outcomes in DLBCL patients. By analyzing radiomics features from FDG-PET/CT scans and clinical data from the first month of treatment, the project forecasts treatment response at the third month post-treatment using machine learning techniques.

## Overview
This project encompasses R scripts developed using RStudio version 2023.12.1 to perform predictive modeling analysis on biomedical data related to CAR-T therapy. The workflow includes preprocessing, feature selection, logistic regression modeling, and performance evaluation.

### Scripts

#### clinical_data_analysis.R

**Functions:**
- **load_data:** Loads the dataset from an Excel file.
- **preprocess_data_2:** Performs preprocessing on the dataset to convert it into a binary format.
- **preprocess_data_3:** Performs additional preprocessing steps on the dataset, handling continuous variables.
- **visualize_correlation:** Visualizes the correlation heatmap and handles highly correlated pairs of features.
- **preselection_correlation_2:** Performs feature preselection and correlation analysis on the dataset, returning a matrix of pre-selected features with their statistics.
- **forward_sel_logistic:** Implements forward selection logistic regression to select significant features and evaluate their performance.
- **create_baseline_model:** Creates a baseline logistic regression model and evaluates its performance on both training and testing datasets.
- **run_analysis:** Main function to execute the entire analysis workflow. It calls all the necessary functions in the correct sequence.

#### Radiomics_analysis.R

**Functions:**
- **load_data:** Loads the PET features data from an Excel file, specifying column types, and replaces any remaining NA, NaN, or Inf values with 0.
- **divide_and_export:** Divides the dataset based on a specified segmentation value and exports the subset to an Excel file.
- **prepare_files:** Loads the PET features dataset, divides it into subsets based on different segmentation values, and exports each subset to separate Excel files.
- **removeLowVariance:** Removes variables with low variance from the dataset.
- **visualize_correlation:** Visualizes the correlation heatmap and handles highly correlated pairs of features.
- **preselection_correlation_2:** Performs feature preselection and correlation analysis on the dataset, returning a matrix of pre-selected features with their statistics.
- **forward_sel_logistic:** Implements forward selection logistic regression to select significant features and evaluate their performance.
- **create_model:** Creates a Radiomics logistic regression model and evaluates its performance on both training and testing datasets.
- **run_analysis:** Main function to execute the entire analysis workflow. It calls all the necessary functions in the correct sequence.

### Instructions for Use
1. Ensure all scripts and required Excel files are saved in the same directory.
2. Open RStudio version 2023.12.1.
3. Set the working directory to the directory containing the scripts.
4. Source the `run_analysis.R` script.
5. Call the `run_analysis()` function to execute the analysis workflow.


### Author
Hedieh Zabolinezhad

### Libraries Used
- gridExtra
- Hmisc
- corrr
- polycor
- corrplot
- readxl
- writexl
- openxlsx
- tibble
- gplots
- rms
- reshape2
- caret
- rsample
- MASS
- logistf
- DescTools
- boot
- ggplot2
- pROC

