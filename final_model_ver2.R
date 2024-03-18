#' Load Required Packages
#' 
#' This function checks for the existence of required packages and installs them if necessary. It then loads the installed packages into the R environment.
#' 
#' @return NULL
#' @export
#' 
load_packages <- function() {
  required_packages <- c("gridExtra","Hmisc","corrr","polycor","corrplot","mlbench","pander","broom","readxl","writexl", "openxlsx", "tibble","Hmisc", "gplots","rms", "reshape2", "caret", "rsample", "MASS", "logistf", "DescTools", "boot", "MASS", "ggplot2","pROC")
  
  # Install and load required packages
  for (package in required_packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
    }
  }
  
  # Load the installed packages
  lapply(required_packages, function(pkg) library(pkg, character.only = TRUE))
}


#' Load Data from Excel File
#' 
#' This function reads data from an Excel file and performs initial preprocessing.
#' 
#' @param file_path The path to the Excel file.
#' @return A list containing the preprocessed data and the last column.
#' @export
#'

# Function to load data from an Excel file
load_data <- function(file_path) {
  data <- read_excel(file_path, sheet = "sheet1", 
                     col_types = c("skip", "skip", "skip", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "text", "numeric", "numeric", 
                                   "skip", "numeric", "numeric", 
                                   "numeric", "text", "numeric", 
                                   "numeric", "text", "numeric", "numeric"), 
                     na = "NA")
  return(data)
}

#' Data Preparation
#' 
#' This function performs preprocessing steps on the input data.
#' 
#' @param data The input data.
#' @return A list containing the prepared data and the last column.
#' @export
#' 
data_preparation <- function(data) {
  # Remove rows where Segmentation column contains "suv3.0" or "suv4.0"
  data <- data[!grepl("suv3\\.0|suv4\\.0", data$Segmentation), ]
  
  
  # Remove the first (Segmentation) and last (label) columns
  # Remove Segmentation column
  data <- data[, -which(names(data) == "Segmentation")]
  
  # Capture the last column
  last_column <- data[, ncol(data)]
  
  # Remove the last column (the target column)
  data <- data[, -ncol(data)]
  
  
  
  # Remove (ml) and (mm) from all column names and change "Volume" to "vol"
  colnames(data) <- gsub("\\(cc\\)|\\(mm\\)|\\(ml\\)", "", colnames(data))
  # Remove all spaces from column names
  colnames(data) <- gsub("\\s+", "", colnames(data))
  
  
  
  # Check for zero variance predictors
  data<-removeLowVariance(data)
  
  
  # Return the prepared data along with the last column
  return(list(data = data, last_column = last_column))
}

#' Split Data Based on Target Column Balance
#' 
#' This function splits the data into training and testing sets based on the balance of the target column.
#' 
#' @param data The input data.
#' @param target_column The name of the target column.
#' @param test_percentage The percentage of data to be used for testing.
#' @return A list containing the training and testing sets.
#' @export
#'
# Function to split data based on the balance of the target column
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


#' Create Model with K-Fold Cross-Validation
#' 
#' This function creates a logistic regression model using K-fold cross-validation.
#' 
#' @param final_vars The final set of predictors.
#' @param data The training data.
#' @param test The test data.
#' @param y The target column name.
#' @param outputdir The directory path to save the output.
#' @param iterations The number of iterations for cross-validation.
#' @param folds_num The number of folds for cross-validation.
#' @return A matrix containing model performance statistics.
#' @export
#'

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
    
    # Check if this model has the best accuracy so far
    # if (iteration_statistics["valid", "Accuracy"] > best_accuracy) {
    #   best_accuracy <- iteration_statistics["valid", "Accuracy"]
    #   best_model <- iteration_statistics
    # }
    
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
  
  # Return the best model based on accuracy
  return(statistics)
}


#' Create Model with K-Fold Cross-Validation and Add Radiomics Features
#' 
#' This function extends an existing clinical model by adding radiomics features and evaluates its performance using K-fold cross-validation.
#' 
#' @param clinical_model The existing clinical logistic regression model.
#' @param radiomics_features The radiomics features to be added.
#' @param data The training data.
#' @param test The test data.
#' @param target_col_name The name of the target column.
#' @param outputdir The directory path to save the output.
#' @param iterations The number of iterations for cross-validation.
#' @param folds_num The number of folds for cross-validation.
#' @return A matrix containing model performance statistics.
#' @export
#'

# Function to perform K-fold cross-validation and add one radiomics feature at a time
create_model_KFold_cross_valid_2 <- function(clinical_model, radiomics_features, data, test, target_col_name, outputdir, iterations = 50, folds_num = 3) {
  # Initialize an empty list to store statistics for each iteration
  statistics_list <- list()
  
  for (iter in 1:iterations) {
    # Initialize an empty list to store statistics for each fold
    fold_statistics_list <- list()
    
    # Create indices for cross-validation folds
    folds <- createFolds(data[[target_col_name]], k = folds_num)
    
    # Iterate over each fold
    for (fold_index in seq_along(folds)) {
      # Get training and test indices for the current fold
      train_indices <- unlist(folds[-fold_index])
      valid_indices <- unlist(folds[fold_index])
      
      # Split the data into training and validation sets
      train_data <- data[train_indices, ]
      valid_data <- data[valid_indices, ]
      
      # Create a formula using the target column name and predictors from clinical model
      frm_clinical <- as.formula(paste(target_col_name, " ~ ", paste(names(clinical_model$coefficients), collapse = " + ")))
      
      # Fit the logistic regression model using clinical predictors
      Model <- glm(formula = frm_clinical, data = train_data, family = binomial)
      
      # Add one radiomics feature at a time and evaluate model performance
      for (feature in radiomics_features) {
        # Create a formula by adding the radiomics feature to the existing clinical predictors
        frm <- as.formula(paste(target_col_name, " ~ ", paste(names(clinical_model$coefficients), collapse = " + "), " + ", feature))
        
        # Fit the logistic regression model with clinical predictors and the current radiomics feature
        Model <- glm(formula = frm, data = train_data, family = binomial)
        
        # Evaluate model performance on validation data
        predict_valid_fit <- predict(Model, valid_data, type = "response")
        
        # Calculate AUC
        roc_curve_valid <- roc(valid_data[[target_col_name]], predict_valid_fit)
        auc_value_valid <- auc(roc_curve_valid)
        
        # Store fold statistics
        fold_statistics <- matrix(NA, nrow = 1, ncol = 2,
                                  dimnames = list(c("Valid"),
                                                  c("AUC", "Accuracy")))
        
        fold_statistics["Valid", "AUC"] <- auc_value_valid
        fold_statistics["Valid", "Accuracy"] <- NULL  # Calculate accuracy if needed
        
        # Store fold statistics in the list
        fold_statistics_list[[fold_index]] <- fold_statistics
      }
    }
    
    # Combine statistics from all folds into a single matrix for the current iteration
    iteration_statistics <- Reduce('+', fold_statistics_list) / folds_num
    
    # Store iteration statistics in the list
    statistics_list[[iter]] <- iteration_statistics
  }
  
  # Combine statistics from all iterations into a single matrix
  statistics <- Reduce('+', statistics_list) / iterations
  
  # Print the combined statistics
  cat("Model cross-validation performance for k = 3 and iteration 50:\n")
  print(statistics)
}

#' Fine-tune the existing model with new features and evaluate its performance
#'
#' This function fine-tunes an existing predictive model by incorporating new features, 
#' trains it using cross-validation, and evaluates its performance on test data.
#'
#' @param existing_model The existing predictive model to be fine-tuned.
#' @param new_features A vector containing the names of the new features to be added to the model.
#' @param train_data The training dataset containing both predictors and the target column.
#' @param test_data The testing dataset containing both predictors and the target column.
#' @param y The name of the target column in the datasets.
#'
#' @return The fine-tuned predictive model.
#'
#' @examples
#' # Fine-tune the existing model with radiomics predictors and evaluate its performance
#' Fine_tune_baseline_model(baseline_model, radiomics_predictors, radiomics_train_data, radiomics_test_data, target_col_name)
#'
#' @export
Fine_tune_baseline_model <- function(existing_model, new_features, train_data, test_data, y) {
 
  updated_model <- update(existing_model, . ~ . + ., data = new_features)
  
  # cross fold adjustments
  set.seed(123)  
  fold_control <- trainControl(method = "cv", number = 10)  
  fine_tuned_model <- train(target_col_name ~ ., data = train_data, method = "rlm", trControl = fold_control)
  
  #test the model
  predictions <- predict(fine_tuned_model, newdata = test_data[, new_features])
  

  confusion_matrix <- confusionMatrix(predictions, test_data[, target_col_name])
  
  # Print summary of the logistic regression model
  summary(fine_tuned_model)
  
  # Convert probabilities to class predictions (0 or 1)
 
  predict_test_fit_classes <- ifelse(predictions > 0.5, 1, 0)
  

  
  roc_curve_test <- roc(test[[y]], predict_test_fit)
  auc_value_test <- auc(roc_curve_test)
  

  
  # Performance statistics for test dataset
  test_conf_matrix <- confusionMatrix(as.factor(predict_test_fit_classes), as.factor(test[[y]]))

  
  # Store performance statistics in the matrix for Test
  statistics["Test", "AUC"] <- auc_value_test

  statistics["Test", "Accuracy"] <- test_conf_matrix$overall['Accuracy']
  

  
  auc_plot_test <- ggroc(roc_curve_test, legacy.axes = TRUE) +
    geom_line(aes(x = 1 - specificity, y = sensitivity), color = "blue", size = 1.5) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
    theme_minimal() +
    labs(title = "AUC-ROC Curve (Test)",
         x = "False Positive Rate",
         y = "True Positive Rate",
         caption = paste("AUC =", round(auc_value_test, 3)),
         color = "red")
  
  
  
  # Print the AUC plots
  print(plots)
  


return(statistics)
}
  
  


 # Main function to execute the workflow with bootstrapping and influence analysis
 run_analysis <- function() {
  
  # Load the readxl package if not already installed
  if (!requireNamespace("readxl", quietly = TRUE)) {
    install.packages("readxl")
  }
  
  
  
  ############## Obtain Clinical Data ##############
  
   # Load the readxl package
   # library(readxl)
  
   # # Specify the file path and name of the Excel file
   # excel_file <- "../Desktop/new_version/CART_BaseFeatures_last.xlsx"
   # 
   # # Read the Excel file into a data frame
   # clinical_data <- read_excel(excel_file)
   # 
   # 
   # clinical_data <- preprocess_data_2(clinical_data)
   # clinical_continuous_binary <- preprocess_data_3(clinical_data)
   # 
   # # Specify target column
   # target_col_name <- "M3_binary_Response"
   # 
   # # Clinical predictors resulted from uni-variable and forward feature selection steps
   #  clinical_predictors <- c("M1_Response_binary_CR","Deauville_binary_4.5", "")
   # 
   # 
   #  
   # # Add target column name to the selected features names
   #  clinical_predictors_wilth_label <- c(target_col_name, rownames(clinical_predictors))
   #  
   # # Filter the data based on pre-selected features
   # clinical_continuous_binary <- clinical_continuous_binary[, intersect(colnames(clinical_continuous_binary), clinical_predictors_wilth_label)]
   #  
   # # Split the data based on the balance of the target column with stratified splitting
   # splited_data <- split_data_balance_target(clinical_continuous_binary, target_col_name)
   # 
   # # Access the training and testing sets
   # clinical_train_data <- splited_data$train_data
   # clinical_test_data <- splited_data$test_data
      
    # view(clinical_train_data)
    # view(clinical_train_data)
      
    
        
 
    
    ############## Obtain Radiomics Data ##############

    # Load and preprocess radiomics data
    radiomics_data <- load_data("C:/Users/ZabolinezhH/Desktop/new_version/pet_feature_total_suv2.5.xlsx")
    radiomics_prepared <- data_preparation(radiomics_data)
    radiomics_modified <- radiomics_prepared$data
    radiomics_target_col <- radiomics_prepared$last_column

    # Add the target column to the radiomics data
    radiomics_with_target <- cbind(radiomics_modified, radiomics_target_col)

    # Radiomics predictors resulted from uni-variable and forward feature selection steps
    radiomics_predictors <- c("SUV_Range")
    target_col_name <- "M3_binary_Response"
   
    # Filter the radiomics data based on pre-selected radiomics features
    radiomics_data_subset <- radiomics_with_target[, c(radiomics_predictors, target_col_name)]

    # Split the radiomics data based on the balance of the target column with stratified splitting
    radiomics_split_data <- split_data_balance_target(radiomics_data_subset, target_col_name)

    # Check if the result is not NULL before using it
    if (!is.null(radiomics_split_data)) {
      # Access the training and testing sets for radiomics data
      radiomics_train_data <- radiomics_split_data$train_data
      radiomics_test_data <- radiomics_split_data$test_data
    }
    
    # view(radiomics_train_data)
    # view(radiomics_test_data)
    
    
    ############## Hybrid Model Developement approach 1 ##############
    
    # Read the baseline model
    outputdir="C:/Users/ZabolinezhH/Desktop/new_version/"
    baseline_model <- readRDS(paste0(outputdir, "baseline_model.rds"))
    
    #Fine-tune the existing model: 
    # You can fine-tune the existing model by adding the new features to the input data and retraining the model on the expanded dataset. 
    # This can be a good option if the new features are related to the old features and you want to preserve the knowledge that the existing model has already learned.
    stat= Fine_tune_baseline_model(baseline_model,  radiomics_predictors, radiomics_train_data, radiomics_test_data,  target_col_name)
    print(stat)
    
    
    
    
    ############## Hybrid Model Development approach 2 ##############
    # Train a new model: 
    # Training a new model from scratch on the entire dataset (including the new features) is one option. 
    # This can be a good choice if the new features are very different from the old features.
   
    # Combine clinical and radiomics predictors
    clinical_predictors_with_radiomics <- c(clinical_predictors, radiomics_predictors)    
    library(readxl)
    
    # Specify the file path containing clinical along with radiomics datasets together
    file_path <- "../Desktop/new_version/total_radiomics_clinical.xlsx"
    
    # Read the Excel file
    total_radiomics_clinical <- read_excel(file_path)
    
    # Display the structure of the data
    str(total_radiomics_clinical)
    
    # Specify target column
    target_col_name <- "M3_binary_Response"
    
    # Clinical predictors resulted from uni-variable and forward feature selection steps
    clinical_predictors <- c("M1_Response_binary_CR","Deauville_binary_4.5", "LDH")
    
    # Combine clinical and radiomics predictors
    predictors <- c(clinical_predictors, radiomics_predictors)

    
    # Filter the radiomics data based on pre-selected radiomics features
    total_radiomics_clinical_subset <- total_radiomics_clinical[, c(radiomics_predictors, target_col_name)]
    
    # Split the radiomics data based on the balance of the target column with stratified splitting
    total_radiomics_clinical_subset<- split_data_balance_target(total_radiomics_clinical_subset, target_col_name)
    
    # Check if the result is not NULL before using it
    if (!is.null(total_radiomics_clinical_subset)) {
      # Access the training and testing sets for radiomics data
      train_data <- total_radiomics_clinical_subset$train_data
      test_data <- total_radiomics_clinical_subset$test_data
      
      stat= create_model_KFold_cross_valid(predictors,  train_data,test_data, target_col_name, outputdir="", iterations=50 , folds_num=3 )
      print(stat)
      
      
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

