#' Load Required Packages
#'
#' This function checks for the presence of required R packages and installs them if necessary. 
#' Then, it loads the installed packages.
#'
#' @return None
#' @examples
#' load_packages()
load_packages <- function() {
  required_packages <- c("Hmisc", "corrr", "polycor", "corrplot", "mlbench", "pander", "broom", 
                         "readxl", "writexl", "openxlsx", "tibble", "Hmisc", "gplots", "rms", 
                         "reshape2", "caret", "rsample", "MASS", "logistf", "DescTools", "boot", 
                         "ggplot2", "pROC","mfp","R6","gridExtra")
  
  # Check if each required package is installed, and install if necessary
  for (package in required_packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
    }
  }
  
  # Load the installed packages
  lapply(required_packages, function(pkg) library(pkg, character.only = TRUE))
}

#' Load Radiomics Features from Excel File
#'
#' This function reads Radiomics features from an Excel file and returns a dataframe.
#'
#' @param file_path The path to the Excel file containing the Radiomics features
#' @return A dataframe containing the Radiomics features
#' @examples
#' PET_features_total <- load_data("path/to/excel/file.xlsx")
load_data <- function(file_path) {
  # Read the Excel file
  PET_features_total <- read_excel(file_path, col_types = c(rep("numeric", 45)))
  
  # Replace any remaining NA, NaN, or Inf values with 0
  PET_features_total[is.na(PET_features_total)] <- 0
  
  return(PET_features_total)
}

#' Divide and Export Data Subset
#'
#' This function subsets rows based on a specified value in the Segmentation column and saves it to a new Excel file.
#'
#' @param data The dataframe containing the data to be subsetted
#' @param value The value to filter the data by in the Segmentation column
#' @param file_name The name of the Excel file to save the subsetted data to
#' @return None
#' @examples
#' divide_and_export(data, "value", "subsetted_data.xlsx")
divide_and_export <- function(data, value, file_name) {
  # Subset rows where Segmentation column contains the specified value
  subset_data <- data[grepl(value, data$Segmentation), ]
  
  # Check if any rows were found with the specified value
  if (nrow(subset_data) > 0) {
    # Save the subset to a new Excel file
    write.xlsx(subset_data, file = file_name)
    cat("Subset with Segmentation value '", value, "' saved to '", file_name, "'.\n", sep = "")
  } else {
    cat("No rows found with Segmentation value '", value, "'.\n", sep = "")
  }
}

#' Prepare Files
#'
#' This function loads data from an Excel file, separates it into subsets based on Segmentation values,
#' and saves each subset to a new Excel file.
#'
#' @return None
#' @examples
#' prepare_files()
prepare_files <- function() {
  # Load the data from Excel file
  PET_features_total <- load_data("path/to/excel/file.xlsx")
  
  # Separate and save the subset with Segmentation value "suv2.5"
  divide_and_export(PET_features_total, "suv2.5", "pet_feature_total_suv2.5.xlsx")
  
  # Separate and save the subset with Segmentation value "suv3.0"
  divide_and_export(PET_features_total, "suv3.0", "pet_feature_total_suv3.0.xlsx")
  
  # Separate and save the subset with Segmentation value "suv4.0"
  divide_and_export(PET_features_total, "suv4.0", "pet_feature_total_suv4.0.xlsx")
}

#' Prepare Data
#'
#' This function prepares the input data by removing specific rows and columns.
#'
#' @param data The dataframe containing the data to be prepared
#' @return A list containing the prepared data and the last column of the original dataframe
#' @examples
#' prepared_data <- data_preparation(data)
data_preparation <- function(data) {
  # Remove rows where Segmentation column contains "suv3.0" or "suv4.0"
  data <- data[!grepl("suv3\\.0|suv4\\.0", data$Segmentation), ]
  
  # Remove the Segmentation column
  data <- data[, -which(names(data) == "Segmentation")]
  
  # Capture the last column
  last_column <- data[, ncol(data)]
  
  # Remove the last column (the target column)
  data <- data[, -ncol(data)]
  
  # Remove (ml) and (mm) from all column names and change "Volume" to "vol"
  colnames(data) <- gsub("\\(cc\\)|\\(mm\\)|\\(ml\\)", "", colnames(data))
  colnames(data) <- gsub("\\s+", "", colnames(data))
  
  # Remove variables with low variance
  data <- removeLowVariance(data)
  
  # Return the prepared data along with the last column
  return(list(data = data, last_column = last_column))
}

#' Normalize Data
#'
#' This function checks for normality using the Shapiro-Wilk test and normalizes non-normally distributed variables.
#'
#' @param data The dataframe containing the data to be normalized
#' @return The normalized dataframe
#' @examples
#' normalized_data <- normalize_data(data)
normalize_data <- function(data) {
  # Check Normality using Shapiro-Wilk test
  normality_results <- sapply(data, function(x) shapiro.test(x)$p.value > 0.05)
  
  # Print variable names with non-normal distribution
  non_normal_variables <- names(normality_results)[!normality_results]
  cat("Variables with non-normal distribution (p-value <= 0.05):\n")
  print(non_normal_variables)
  
  # Print count of non-normal variables
  cat("Count of variables with non-normal distribution: ", length(non_normal_variables), "\n")
  
  # Normalize Data
  for (column in non_normal_variables) {
    data[[column]] <- scale(data[[column]], center = TRUE, scale = TRUE)
  }
  
  return(data)  # Return the normalized data
}

#' Visualize Correlation
#'
#' This function generates a heatmap to visualize the correlation matrix of the input data.
#' Correlation values are rounded to two decimal places and displayed in the heatmap.
#' Highly correlated pairs with correlation > 0.8 are identified and printed.
#'
#' @param data The dataframe containing the data to visualize
#' @return The input dataframe with highly correlated columns removed
#' @examples
#' visualize_correlation(data)
visualize_correlation <- function(data) {
  library(reshape2)
  library(gplots)
  
  # Draw a heatmap to visualize the correlation matrix
  cor_matrix <- cor(data, method = "pearson")
  
  # Set colors for the heatmap
  col_scale <- colorRampPalette(c("white", "orange"))(20)  
  
  # Create the heatmap using heatmap.2
  heatmap.2(cor_matrix,
            col = col_scale,
            trace = "none",
            margins = c(8, 8),
            dendrogram = "none",
            keysize = 1.5,
            main = "Correlation Heatmap",
            labRow = colnames(cor_matrix),
            labCol = colnames(cor_matrix),
            cexRow = 0.7,
            cexCol = 0.7,
            cellnote = round(cor_matrix, digits = 2),  # Round the correlation values
            notecol = "black",
            notecex = 0.7)
  
  # Return the input dataframe with highly correlated columns removed
  return(data[, -which(colnames(data) %in% highly_correlated_pairs)])
}

#' Split Data Based on Target Balance
#'
#' This function splits the input data into training and testing sets based on the balance of the target column.
#'
#' @param data The dataframe containing the data to split
#' @param target_column The name of the target column
#' @param test_percentage The percentage of data to allocate for testing (default is 0.2)
#' @return A list containing the training and testing datasets
#' @examples
#' split_data <- split_data_balance_target(data, "target_column", 0.2)
split_data_balance_target <- function(data, target_column, test_percentage = 0.2) {
  # Check the distribution of the target column
  target_distribution <- table(data[[target_column]])
  
  # Display the distribution
  print(target_distribution)
  
  # Set the seed for reproducibility
  set.seed(123)
  
  # Create the stratified split
  split_obj <- initial_split(data, prop = 1 - test_percentage, strata = target_column)
  
  # Extract the training and testing sets
  train_data <- training(split_obj)
  test_data <- testing(split_obj)
  
  cat("Data has been split into training and testing sets.\n")
  
  # Return the result
  return(list(train_data = train_data, test_data = test_data))
}

# Define a function for preselection and correlation analysis
#' Preselection and correlation analysis
#' 
#' This function performs preselection and correlation analysis for logistic regression.
#' 
#' @param DATA The dataframe containing the data.
#' @param ynam The name of the dependent variable.
#' @param output_file The file path to save the results.
#' @return A dataframe containing the preselected features and their statistics.
preselection_correlation <- function(DATA, ynam, output_file) {
  
  # Make a list of headers excluding the dependent variable
  feats_of_interest <- colnames(DATA)[!colnames(DATA) %in% ynam]
  
  # Initialize matrices to store results
  Model_predict <- list(type = any)
  OR_95 <- matrix(NA, nrow = length(feats_of_interest), ncol = 4)
  stats.s <- matrix()
  
  # Loop through each feature of interest
  for (i in 1:length(feats_of_interest)) {
    # Create a formula for logistic regression model
    frm <- as.formula(paste(ynam, " ~ ", feats_of_interest[i]))
    
    # Try to fit logistic regression model
    Model_predict[[i]] <- lrm(formula = frm, x = TRUE, y = TRUE, data = DATA)
    
    # Compute odds ratio and 95% confidence interval
    StanER = sqrt(diag(vcov(Model_predict[[i]]))) 
    OR_95[i,] = cbind(Model_predict[[i]]$coefficients[[feats_of_interest[i]]], 
                      exp(Model_predict[[i]]$coefficients[[feats_of_interest[i]]]),
                      exp(Model_predict[[i]]$coefficients[[feats_of_interest[i]]] - StanER[[feats_of_interest[i]]] * 1.96),
                      exp(Model_predict[[i]]$coefficients[[feats_of_interest[i]]] + StanER[[feats_of_interest[i]]] * 1.96))
    
    # Store statistical information
    if (i > 1) {
      stats.s = cbind(stats.s, c(Model_predict[[i]]$stats, anova(Model_predict[[i]])[1,3]))
    } else {
      stats.s = c(Model_predict[[i]]$stats, anova(Model_predict[[i]])[1,3])
    }
  }
  
  # Set column names and row names for the statistical matrix
  colnames(stats.s) <- feats_of_interest
  rownames(stats.s)[5] = "uni_p_value"
  
  # Create a matrix combining statistical information and odds ratio
  stat_mat = cbind(t(stats.s[c(5,6,10), order(stats.s[5,])]), OR_95[order(stats.s[5,]),])
  colnames(stat_mat)[4:7] = c("beta", "OR", "OR95-", "OR95+")
  
  # Print the number of univariable features significant at 0.05 level
  cat("\n", "Univariable significant: ", nrow(stat_mat[stat_mat[,'uni_p_value'] < 0.05,]), " of ", nrow(stat_mat))
  p005 = stat_mat[stat_mat[,'uni_p_value'] < 0.05,]
  
  # Initialize variables for further processing
  var.sel1.names = rownames(stat_mat[stat_mat[,'uni_p_value'] < 1,])
  var.sel1.stats = stat_mat[stat_mat[,'uni_p_value'] < 1,]
  z = 0
  correlatiens = c()
  
  # Loop through variables to check correlations
  for (i in 1:length(var.sel1.names)) {
    for (j in 1:length(var.sel1.names)) {
      if (i != j & i < j) {
        # Calculate correlation and check for high correlation (abs > 0.8)
        correlat = cor(DATA[, var.sel1.names[i]], DATA[, var.sel1.names[j]])
        if (abs(correlat) > 0.8) {
          # if (abs(correlat) > cor_cutoff) {
          z = z + 1
          correlatiens = rbind(correlatiens, c(var.sel1.names[i], var.sel1.stats[i], var.sel1.names[j], var.sel1.stats[j], correlat))
        }
      }
    }
  }
  
  # Store correlation information in 'correlatiens_dyn'
  correlatiens_dyn = correlatiens
  i1 = 0
  preselected_vars = c()
  
  # Process correlated variables to select superior features
  while (length(dim(correlatiens_dyn)) > 1 && is.matrix(correlatiens_dyn)) {
    i1 = i1 + 1
    superior_feature = correlatiens_dyn[1, 1]
    exluderen = correlatiens_dyn[correlatiens_dyn[, 1] %in% superior_feature, 3]
    correlatiens_dyn = correlatiens_dyn[!correlatiens_dyn[, 1] %in% exluderen,]
    if (length(dim(correlatiens_dyn)) > 1) {
      correlatiens_dyn = correlatiens_dyn[!correlatiens_dyn[, 1] %in% superior_feature,]
    }
    preselected_vars = c(preselected_vars, superior_feature)
  }
  
  # Check if 'correlatiens_dyn' is a vector and not a matrix
  if (length(dim(correlatiens_dyn)) == 1 && !is.null(correlatiens_dyn)) {
    # Extract feature names and values
    feature_names <- correlatiens_dyn[seq(1, length(correlatiens_dyn), by = 2)]
    values <- correlatiens_dyn[seq(2, length(correlatiens_dyn), by = 2)]
    
    # Create a matrix with two columns
    correlatiens_dyn = matrix(c(feature_names, values), ncol = 2, byrow = TRUE)
    
    # Append the resulting matrix to 'preselected_vars'
    preselected_vars = c(preselected_vars, correlatiens_dyn[, 1])
  }
  
  # Identify variables with no correlations
  no_corr_at_all = var.sel1.names[!var.sel1.names %in% c(correlatiens[, 1], correlatiens[, 3])]
  # Prepare lists of features for further analysis
  features_of_interest0 = var.sel1.names[!var.sel1.names %in% correlatiens[, 3]]
  features_of_interest = var.sel1.names[var.sel1.names %in% c(no_corr_at_all, preselected_vars)]
  
  # Extract and print selected features
  Preselection = var.sel1.stats[var.sel1.names %in% features_of_interest,]
  # Filter out rows with uni_p_value less than 0.05
  Preselection <- Preselection[Preselection[, "uni_p_value"] <= 0.05, ]
  print(Preselection)
  cat("\n", "Selected features: ", nrow(Preselection), " of ", nrow(stat_mat))
  
  # Save the preselected features and their statistics to an Excel file
  univar_result_df <- as.data.frame(Preselection)
  univar_result_df <- tibble::rownames_to_column(univar_result_df, "variable")
  write_xlsx(univar_result_df, paste(output_file, ".xlsx", sep = ""))
  # Return the result
  return(Preselection)
}

# Function to perform forward selection logistic regression
#' Forward Selection Logistic Regression
#' 
#' This function performs forward selection logistic regression.
#' 
#' @param DATA The dataframe containing the data.
#' @param ynam The name of the dependent variable.
#' @param boot Bootstrapping parameter (not used in the function).
#' @param excel_export Whether to export results to Excel (1 for yes, 0 for no).
#' @param outputdir The directory to save the Excel files.
#' @param p_val_thresh The p-value threshold for variable selection.
#' @return A list containing the selected variables and their statistics.
#' @author Dr. L.V. (Lisanne) van Dijk
#' @modifiedBy Hedieh Zabolinezhad
forward_sel_logistic <- function(DATA, ynam, boot, excel_export, outputdir, p_val_thresh) {
  # Force include candidate variables
  sel <- c()  # Selected variables
  TEST_pvalue <- 0  # P-value threshold
  run_i0 <- 0  # Initialization of the run counter
  feats_of_interest <- colnames(DATA)[!colnames(DATA) %in% ynam]  # Features of interest (exclude response variable)
  Steps_together <- data.frame()  # Initialize an empty dataframe to store results
  
  # Iterate until TEST_pvalue is smaller than the threshold and the number of selected features is less than the total number of variables
  while (TEST_pvalue < p_val_thresh & length(sel) < length(feats_of_interest)) {
    run_i0 <- run_i0 + 1  # Increment run counter
    
    # Delete variables already in the model
    selfor <- sel[1]  # Selected features
    if (length(sel) > 1) {
      for (z in 2:length(sel)) {
        selfor <- paste(selfor, sel[z], sep = " + ")
      }
    }
    
    # Erase the features added to 'sel'
    feats_of_interest <- feats_of_interest[!feats_of_interest %in% sel]
    
    # Make reference model
    if (length(sel) > 0) {
      frm <- as.formula(paste(ynam, " ~ ", selfor))
      Model_ref <- lrm(formula = frm, x = TRUE, y = TRUE, data = DATA)
    }
    
    # Make a list 'model_predict' that can handle any type of value
    Model_predict <- vector("list", length(feats_of_interest))
    
    # Make a list of matrices for different statistical parameters
    stats.s <- matrix(NA, nrow = 18, ncol = length(feats_of_interest))
    stats.LLRT <- matrix(NA, nrow = 1, ncol = length(feats_of_interest))
    AIC_BIC <- matrix(NA, nrow = 2, ncol = length(feats_of_interest))
    beta <- matrix(NA, nrow = 2, ncol = length(feats_of_interest))
    
    # Loop through the features remaining in the feats_of_interest list
    for (i in 1:length(feats_of_interest)) {
      # Make a new formula that contains the selected features and the ith feature to compare with the reference model
      frm <- as.formula(paste(ynam, " ~ ", selfor, " + ", feats_of_interest[i]))
      
      # This code make a model based on the new list of features and handle any possible error 
      Model_predict[[i]] = lrm(formula = frm, x = T, y = T,  data = DATA) 
      
      # Handle errors or perfect separation
      if (length(Model_predict[[i]]) < 3) {
        stats.LLRT[i] <- NA  # Set LLRT to NA in case of errors
        next
      } else {
        # Basic performance measures
        stats.s[, i] <- Model_predict[[i]]$stats
        AIC_BIC[, i] <- c(AIC(Model_predict[[i]]), BIC(Model_predict[[i]]))
        beta[, i] <- c(Model_predict[[i]]$coefficients[[feats_of_interest[i]]], exp(Model_predict[[i]]$coefficients[[feats_of_interest[i]]]))
        
        # Likelihood ratio test
        if (length(sel) > 0) {
          LLRT <- lrtest(Model_ref, Model_predict[[i]])
          stats.LLRT[i] <- LLRT$stats[3]
        } else {
          LLRT <- anova(Model_predict[[i]])
          stats.LLRT[i] <- LLRT["TOTAL", "P"]
        }
      }
    }
    
    # Assign names to columns and rows of different datasets
    colnames(stats.s) <- feats_of_interest
    rownames(stats.s) <- names(Model_predict[[i]]$stats)
    rownames(stats.LLRT) <- "p_LRT"
    rownames(AIC_BIC) <- c("AIC", "BIC")
    rownames(beta) <- c("beta", "OR")
    
    # Transpose the matrix to change the place of rows and columns
    vallalles <- t(rbind(stats.s[c(5:6, 10), ], beta, AIC_BIC, stats.LLRT))
    
    # Order the matrix based on p_LRT in ascending form
    vgl <- vallalles[order(vallalles[, "p_LRT"]), ]
    
    # Round the matrix float numbers
    round(vgl, 3)
    
    # Assign the smallest p-value to 'TEST_pvalue', if it is equal to null value, assign 1 to 'TEST_pvalue'
    TEST_pvalue <- vgl[1, "p_LRT"]
    if (is.na(as.numeric(TEST_pvalue))) {
      TEST_pvalue <- 1
    }
    
    # Extract the name of the variable with the smallest p-value
    selected_variable <- rownames(vgl)[1]
    
    # Add it to 'sel' which is the list of selected features
    sel <- c(sel, selected_variable)
    
    # Show the selected feature and also the p-value
    print(paste0(round(TEST_pvalue, 4), "-->  ", selfor, " + ", selected_variable))
    
    # If 'excel_report' == 1, write everything in an excel file
    if (excel_export == 1) {
      # save vlg in an excel file that shows the number of run and the name is like "Preselection_step_(number).xlsx"
      vgl_df <- as.data.frame(vgl)
      vgl_df <- tibble::rownames_to_column(vgl_df, "variable")
      write.xlsx(vgl_df, paste(outputdir, paste("RadiomicsFeatures_selection_step_", run_i0, ".xlsx", sep = "")))
    }
    
    # Make a formula based on old selected features and the new one
    frm <- as.formula(paste(ynam, " ~ ", selfor, " + ", selected_variable))
    
    # Fit a logistic regression model for this new set of variables
    step_model <- lrm(formula = frm, x = TRUE, y = TRUE, data = DATA)
    ss <- summary(step_model)
    
    # Make a matrix (dataframe) from coefficients, the exponential of them, and the variances of these coefficients
    step_model_coef <- cbind(step_model$coefficients, exp(step_model$coefficients), c(NA, anova(step_model)[1:(length(step_model$coefficients) - 1), 3]))
    
    # Make a matrix from the first model in vlg and the coefficients
    step_model_params <- matrix(NA, nrow = nrow(step_model_coef), 3 + length(vgl[1, ]))
    step_model_params[, 1:3] <- step_model_coef
    step_model_params[1, 4:(3 + length(vgl[1, ]))] <- vgl[1, ]
    
    # Assign names to the columns and rows of the created matrix
    if (length(sel) > 1) {
      rownames(step_model_params) <- rownames(step_model_coef)
      colnames(step_model_params) <- c(c("beta", "OR", "p_val"), names(vgl[1, ]))
    } else {
      rownames(step_model_params) <- rownames(step_model_coef)
      colnames(step_model_params) <- c(c("beta", "OR", "p_val"), names(vgl[1, ]))
    }
    
    # Add the mentioned matrix as a row of the final matrix 'Steps_together'
    Steps_together <- rbind(Steps_together, step_model_params)
  }
  
  # Round the final matrix float numbers
  round(Steps_together, 2)
  
  # Return both the matrix and the names of selected variables
  return(list(selected_variables = sel, Steps_together = Steps_together))
}


# Function to create the correlation matrix dataframe
#' Create Correlation Matrix
#' 
#' This function creates a correlation matrix dataframe.
#' 
#' @param df The dataframe containing the data.
#' @param target The name of the target variable.
#' @param include_target Whether to include the target variable in the correlation matrix.
#' @return A correlation matrix dataframe.
createCorrMatrix <- function(df, target = NULL, include_target = FALSE) {
  if (!include_target && !is.null(target)) {
    df <- df[, !(names(df) %in% target)]
  }
  corrMatrix <- cor(df, method = "pearson")
  return(corrMatrix)
}

# Function to create and return the feature to target correlation matrix dataframe
#' Create Correlation Matrix with Target
#' 
#' This function creates and returns the feature to target correlation matrix dataframe.
#' 
#' @param df The dataframe containing the data.
#' @param target The name of the target variable.
#' @return A feature to target correlation matrix dataframe.
createCorrMatrixWithTarget <- function(df, target) {
  corrMatrix <- createCorrMatrix(df, target = target, include_target = TRUE)
  corrWithTarget <- data.frame(correlation = corrMatrix[, target])
  corrWithTarget <- corrWithTarget[order(corrWithTarget$correlation), , drop = FALSE]
  return(corrWithTarget)
}

# Function to create and return the list of correlated features
#' Create Correlated Features List
#' 
#' This function creates and returns the list of correlated features.
#' 
#' @param df The dataframe containing the data.
#' @param threshold The correlation threshold.
#' @return A list of correlated features.
createCorrelatedFeaturesList <- function(df, threshold=0.8) {
  corrMatrix <- createCorrMatrix(df)
  colCorr <- character(0)
  for (column in colnames(corrMatrix)) {
    for (idx in rownames(corrMatrix)) {
      if (corrMatrix[idx, column] > threshold && corrMatrix[idx, column] < 1) {
        if (!(idx %in% colCorr)) {
          colCorr <- c(colCorr, idx)
        }
        if (!(column %in% colCorr)) {
          colCorr <- c(colCorr, column)
        }
      }
    }
  }
  return(colCorr)
}

# Function to delete features from the list of correlated features
#' Delete Features
#' 
#' This function deletes features from the list of correlated features.
#' 
#' @param df The dataframe containing the data.
#' @param colCorr The list of correlated features.
#' @param target The name of the target variable.
#' @return A dataframe with correlated features deleted.
deleteFeatures <- function(df, colCorr, target) {
  corrWithTarget <- createCorrMatrixWithTarget(df, target)
  for (idx in rownames(corrWithTarget)) {
    if (idx %in% colCorr) {
      df <- df[, !(names(df) %in% idx)]
      break
    }
  }
  return(df)
}


#' Create Model with K-Fold Cross-Validation
#' 
#' This function creates a logistic regression model with K-Fold cross-validation and evaluates its performance using Area Under Curve (AUC) and accuracy metrics.
#' 
#' @param final_vars A character vector containing the names of the final selected predictor variables.
#' @param data The training dataframe containing the predictor variables and the target variable.
#' @param test The test dataframe containing the predictor variables and the target variable for external validation.
#' @param y The name of the target variable.
#' @param outputdir The directory to save the output plots.
#' @param iterations The number of iterations for K-Fold cross-validation.
#' @param folds_num The number of folds for K-Fold cross-validation.
#' @return A matrix containing the mean AUC and accuracy values across iterations for the training, validation, and external test datasets.
#' @details This function trains a logistic regression model using the provided training data and evaluates its performance using K-Fold cross-validation. It then applies the trained model to the external test data for further evaluation. Performance metrics including AUC and accuracy are calculated for each fold and iteration. The function returns the mean AUC and accuracy values across iterations for the training, validation, and external test datasets.
create_model_KFold_cross_valid <- function(final_vars, data, test, y, outputdir, iterations=50 , folds_num=3 ) {
  
  # Initialize variables to track the best model and its accuracy
  best_accuracy <- 0
  best_model <- NULL
  
  # Initialize an empty list to store statistics for each iteration
  statistics_list <- list()
  
  for (iter in 1:iterations) {
    # Initialize an empty list to store statistics for each fold
    fold_statistics_list <- list()
    
    # Create indices for cross-validation folds
    folds <- createFolds(data[[y]], k = folds_num)
    
    # Iterate over each fold
    for (fold_index in seq_along(folds)) {
      # Get training and test indices for the current fold
      train_indices <- unlist(folds[-fold_index])
      valid_indices <- unlist(folds[fold_index])
      
      # Split the data into training and test sets
      train_data <- data[train_indices, ]
      valid_data <- data[valid_indices, ]
      
      # Baseline model performance on training data
      frm <- as.formula(paste(y, " ~ ", paste(final_vars, collapse = ' + ')))
      base_line_model <- lrm(formula = frm, x = TRUE, y = TRUE, data = train_data)
      predict_train_fit <- predict(base_line_model, train_data)
      
      # Baseline model performance on valid data
      predict_valid_fit <- predict(base_line_model, valid_data)
      
      # Convert probabilities to class predictions (0 or 1)
      predict_train_fit_classes <- ifelse(predict_train_fit > 0.5, 1, 0)
      predict_valid_fit_classes <- ifelse(predict_valid_fit > 0.5, 1, 0)
      
      # Calculate the AUC for train and valid datasets
      roc_curve_train <- roc(train_data[[y]], predict_train_fit)
      auc_value_train <- auc(roc_curve_train)
      
      roc_curve_valid <- roc(valid_data[[y]], predict_valid_fit)
      auc_value_valid <- auc(roc_curve_valid)
      
      # Performance statistics for train dataset
      train_conf_matrix <- confusionMatrix(factor(predict_train_fit_classes), factor(train_data[[y]]))
      
      # Performance statistics for test dataset
      valid_conf_matrix <- confusionMatrix(factor(predict_valid_fit_classes), factor(valid_data[[y]]))
      
      
      predict_test_fit <- predict(base_line_model, test)
      predict_test_fit_classes <- ifelse(predict_test_fit > 0.5, 1, 0)
      
      # Performance statistics for external test dataset
      test_conf_matrix <- confusionMatrix(factor(predict_test_fit_classes), factor(test[[y]]))
      roc_curve_test <- roc(test[[y]], predict_test_fit)
      auc_value_test <- auc(roc_curve_test)
      
      
      # Store performance statistics for the current fold
      fold_statistics <- matrix(NA, nrow = 3, ncol = 2,
                                dimnames = list(c("Train", "valid", "test"),
                                                c("AUC", "Accuracy")))
      
      # Assign values to the statistics matrix for Train
      fold_statistics["Train", "AUC"] <- auc_value_train
      fold_statistics["Train", "Accuracy"] <- train_conf_matrix$overall['Accuracy']
      
      
      
      fold_statistics["test", "AUC"] <- auc_value_test
      fold_statistics["test", "Accuracy"] <- test_conf_matrix$overall['Accuracy']
      
      
      # Assign values to the statistics matrix for Test
      fold_statistics["valid", "AUC"] <- auc_value_valid
      fold_statistics["valid", "Accuracy"] <- valid_conf_matrix$overall['Accuracy']
      
      
      
      
      
      
      
      # Store fold statistics in the list
      fold_statistics_list[[fold_index]] <- fold_statistics
    }
    
    # Combine statistics from all folds into a single matrix for the current iteration
    iteration_statistics <- Reduce('+', fold_statistics_list) / folds_num
    
    # Store iteration statistics in the list
    statistics_list[[iter]] <- iteration_statistics
  }
  
  # Combine statistics from all iterations into a single matrix
  statistics <- Reduce('+', statistics_list) / iterations
  
  
  
  # Print the combined statistics
  cat("Model cross validation performance for k=3 and iteration 50:\n")
  print(statistics)
  
  # Plot AUC-ROC curves
  auc_plot_valid <- ggroc(roc_curve_valid, legacy.axes = TRUE) +
    geom_line(aes(x = 1 - specificity, y = sensitivity), color = "blue", size = 1.5) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
    theme_minimal() +
    labs(title = "AUC-ROC Curve (valid)",
         x = "False Positive Rate",
         y = "True Positive Rate",
         caption = paste("AUC =", round(statistics["valid", "AUC"], 2)), color = "red")
  
  auc_plot_test <- ggroc(roc_curve_test, legacy.axes = TRUE) +
    geom_line(aes(x = 1 - specificity, y = sensitivity), color = "blue", size = 1.5) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
    theme_minimal() +
    labs(title = "AUC-ROC Curve (Test)",
         x = "False Positive Rate",
         y = "True Positive Rate",
         caption = paste("AUC =", round(statistics["test", "AUC"], 2)),
         color = "red")
  
  # Arrange plots side by side
  plots <- grid.arrange(auc_plot_valid, auc_plot_test, ncol = 2)
  
  # Print the AUC plots
  print(plots)
  
  # Return the mean AUC and accuracy values across iterations for the training, validation, and external test datasets
  return(statistics)
} 



#' Run Analysis Workflow
#' 
#' This function executes the entire workflow, including data preprocessing, feature selection, model training with K-Fold cross-validation, and performance evaluation.
#' 
#' @details The function loads necessary packages, performs data preprocessing, prepares the dataset by removing specific rows and columns, converts units, and changes variable names. It then performs feature pre-selection by removing correlated and less significant features, splits the data based on the balance of the target column with stratified splitting, and trains a logistic regression model with forward feature selection using bootstrap iterations. Finally, it evaluates the model's performance using K-Fold cross-validation and visualizes the results.
run_analysis <- function() {
  
  load_packages()
  
  # Perform data preprocessing to build necessary files
  prepare_files()
  
  pet_feature_total_suv2.5 <- load_data("pet_feature_total_suv2.5.xlsx")
  
  # Call the data_preparation function
  # Function to prepare dataset by removing specific rows and columns, converting
  # units and changing variable names
  prepared_data <- data_preparation(pet_feature_total_suv2.5)
  
  # Access the prepared data frame
  pet_feature_total_suv2.5_modified <- prepared_data$data
  
  # Access the last column that was removed
  target_col <- prepared_data$last_column
  column_names <- colnames(pet_feature_total_suv2.5_modified)
  
  # Specify target
  target_col_name <- "M3_binary_Response"
  
  # Add the 'target_col' column to the modified data frame
  pet_feature_total_suv2.5_modified_with_label <- pet_feature_total_suv2.5_modified
  pet_feature_total_suv2.5_modified_with_label <- cbind(pet_feature_total_suv2.5_modified_with_label, target_col)
  
  # Set the correlation threshold
  threshold <- 0.8
  
  # Call the visualize_correlation function with the dataset
  visualize_correlation(pet_feature_total_suv2.5_modified)
  
  # Call the Function to create and return the list of correlated features
  correlated_pairs <- createCorrelatedpairsList(pet_feature_total_suv2.5_modified)
  
  # Feature Pre-selection: Remove correlated and less significant features strategy
  pre_selected_features <- preselection_correlation(pet_feature_total_suv2.5_modified_with_label, target_col_name)
  print(rownames(pre_selected_features))
  view(pre_selected_features)
  
  # Check if pre-selected features are not empty
  if (nrow(pre_selected_features) > 0) {
    
    # Remove "SpreadPatient" from pre_selected_features
    remived_feat_nam <- pre_selected_features[!rownames(pre_selected_features) %in% c("SpreadPatient","TLG"), , drop = FALSE]
    
    # Add target column name to the selected features names
    pre_selected_featNam_wilth_label <- c(target_col_name, rownames(remived_feat_nam))
    
    # Filter data based on pre-selected features
    purified_data <- pet_feature_total_suv2.5_modified_with_label[, intersect(colnames(pet_feature_total_suv2.5_modified_with_label), pre_selected_featNam_wilth_label)]
    
    # More data preperation
    # Check for zero variance predictors
    purified_data <- removeLowVariance(purified_data)
    # purified_data <- normalize_data(purified_data)
    
    # Split the data based on the balance of the target column with stratified splitting
    splited_data <- split_data_balance_target(purified_data, target_col_name)
    
    # Check if the result is not NULL before using it
    if (!is.null(splited_data)) {
      # Access the training and testing sets
      train_data <- splited_data$train_data
      test_data <- splited_data$test_data
      
     
      
      # Create the 'dd' object using the 'datadist' function
      dd <<- datadist(train_data)
      options(datadist = 'dd')
      
      # Define the number of bootstrap iterations
      n_bootstrap <- 500
      
      # Initialize a list to store significant features from each bootstrap iteration
      significant_features_list <- vector("list", n_bootstrap)
      
      # Perform bootstrap iterations
      for (i in 1:n_bootstrap) {
        
        # Sample with replacement from the training data
        bootstrap_sample <- train_data[sample(nrow(train_data), replace = TRUE), ]
        result <- forward_sel_logistic_2(bootstrap_sample, target_col_name, boot = 0, excel_export = 0, outputdir = "", p_val_thresh = 0.05)
        # Extract significant features based on p-values
        significant_features <-result$selected_variables
        
        # Store the significant features for this iteration
        significant_features_list[[i]] <- significant_features
        print(significant_features_list[[i]])
      }
      
      # Combine significant features from all bootstrap iterations
      all_significant_features <- unlist(significant_features_list)
      
      # Calculate the frequency of each significant feature
      feature_frequencies <- table(all_significant_features)
      
      # Print the feature frequencies
      cat("Feature Frequencies (across all bootstrap iterations):\n")
      view(feature_frequencies)
      
      # Sort the feature frequencies in descending order
      sorted_frequencies <- sort(feature_frequencies, decreasing = TRUE)
      
      # Extract the top three features and their frequencies
      top_three_features <- head(sorted_frequencies, 3)
      
      # Print the top three features and their frequencies
      print(top_three_features)
      
      # Extract top features as predictors
      Radiomics_predictors <- names(top_three_features)
      
      # Train model with K-Fold Cross-Validation
      stat <- create_model_KFold_cross_valid(Radiomics_predictors, train_data, test_data, target_col_name, outputdir = "", iterations = 50, folds_num = 3) 
      print(stat)
      
    } else {
      cat("Error: Unable to split the data based on the target column.")
    }
    
  } else {
    cat("Error: No pre-selected features available.")
  }
}

# Run the analysis
run_analysis()

# Check for warnings and print them in red
if (length(warnings()) > 0) {
  cat("\033[31m")  # ANSI escape code for red text
  print(warnings())
  cat("\033[0m")   # ANSI escape code to reset text color
}
