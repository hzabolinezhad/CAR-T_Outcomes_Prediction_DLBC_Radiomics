#' Load required R packages
#'
#' This function loads required R packages listed in the \code{required_packages} vector.
#' If any package is not installed, it installs it.
#' @return NULL
load_packages <- function() {
  required_packages <- c("gridExtra","Hmisc","corrr","polycor","corrplot","mlbench","pander","broom","readxl","writexl", "openxlsx", "tibble","Hmisc", "gplots","rms", "reshape2", "caret", "rsample", "MASS", "logistf", "DescTools", "boot", "MASS", "ggplot2","pROC", "vcd")
  
  # Install and load required packages
  for (package in required_packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
    }
  }
  
  # Load the installed packages
  lapply(required_packages, function(pkg) library(pkg, character.only = TRUE))
}


#' Load data from an Excel file
#'
#' This function reads data from an Excel file and returns it as a dataframe.
#' @param file_path The path to the Excel file
#' @return The dataframe containing the loaded data
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


#' Preprocess data
#'
#' This function preprocesses the input data by renaming columns, converting variables, 
#' replacing missing values with zeros, and saving the processed data to an Excel file.
#' @param data The dataframe containing the raw data
#' @return The dataframe containing the preprocessed data
preprocess_data <- function(data) {
  # Rename columns
  colnames(data)[colnames(data) == "Age\r\n"] <- "Age"
  colnames(data)[colnames(data) == "Prior_LinesOFTherapy_N \r\n"] <- "Prior_LinesOFTherapy"
  colnames(data)[colnames(data) == "StemCell_transplantati\r\n"] <- "StemCell_transplantation"
  colnames(data)[colnames(data) == "ECOG_PS _screening"] <- "ECOG_PS_screening"
  colnames(data)[colnames(data) == "ECOG_PS _infusion"] <- "ECOG_PS_infusion"
  
  # Convert categorical variables to factors
  data$Diagnosis_WHO2016_DLBCL <- as.factor(data$Diagnosis_WHO2016_DLBCL)
  data$BridgingRegimen <- as.factor(data$BridgingRegimen)
  data$M1_Response <- as.factor(data$M1_Response)
  
  # Convert factors to numeric
  data$Diagnosis_WHO2016_DLBCL <- as.numeric(data$Diagnosis_WHO2016_DLBCL)
  data$BridgingRegimen <- as.numeric(data$BridgingRegimen)
  data$M1_Response <- as.numeric(data$M1_Response)
  
  # Replace NAs with zeros
  data[is.na(data)] <- 0
  
  # Save data to Excel
  write_xlsx(data, paste("CAR_T_Data",".xlsx", sep = ""))
  
  return(data)
}


#' Preprocess data and create a file containing binary data
#'
#' This function preprocesses the input data by renaming columns, converting variables, 
#' replacing missing values with zeros, adding binary columns, removing unnecessary columns, 
#' and saving the processed data to an Excel file.
#' @param data The dataframe containing the raw data
#' @return The dataframe containing the preprocessed data
preprocess_data_2 <- function(data) {
  # Rename columns
  colnames(data)[colnames(data) == "Age\r\n"] <- "Age"
  colnames(data)[colnames(data) == "Prior_LinesOFTherapy_N \r\n"] <- "Prior_LinesOFTherapy"
  colnames(data)[colnames(data) == "StemCell_transplantati\r\n"] <- "StemCell_transplantation"
  colnames(data)[colnames(data) == "ECOG_PS _screening"] <- "ECOG_PS_screening"
  colnames(data)[colnames(data) == "ECOG_PS _infusion"] <- "ECOG_PS_infusion"
  colnames(data)[colnames(data) == "StemCell_transplantation"] <- "StemCell_transplantation_binary"
  colnames(data)[colnames(data) == "Bulky_disease"] <- "Bulky_disease_binary"
  
  # Replace NAs with zeros
  data[is.na(data)] <- 0
  
  # Add binary columns
  data$M1_Response_binary_CR_PR <- ifelse(data$M1_Response %in% c("CR", "PR"), 1, 0)
  data$M1_Response_binary_CR <- ifelse(data$M1_Response %in% c("CR"), 1, 0)
  data$screening_Stage_binary <- ifelse(data$screening_Stage %in% c(3, 4), 1, 0)
  data$IPI_binary_2.3.4.5 <- ifelse(data$IPI %in% c(2, 3, 4, 5), 1, 0)
  data$IPI_binary_3.4.5 <- ifelse(data$IPI %in% c(3, 4, 5), 1, 0)
  data$LDH_binary <- ifelse((data$Gender == 1 & data$LDH > 248) | (data$Gender == 2 & data$LDH > 247), 1, 0)
  data$LDH_binary_2_times_ULN <- ifelse((data$Gender == 1 & data$LDH > (2*248)) | (data$Gender == 2 & data$LDH > (2*247)), 1, 0)
  data$Age_binary <- ifelse(data$Age >= 60, 1, 0)
  data$Deauville_binary_4.5 <- ifelse(data$Deauville %in% c(4, 5), 1, 0)
  data$Deauville_binary_3.4.5 <- ifelse(data$Deauville %in% c(3, 4, 5), 1, 0)
  data$BridgingRegimen_Binary_RT_vs_rest <- ifelse(grepl("Radiotherapy", data$BridgingRegimen, ignore.case = TRUE), 1, 0)
  data$BridgingRegimen_Binary_RT_ST_vs_res <- ifelse(grepl("Radiotherapy|Systemic therapy|Combination", data$BridgingRegimen, ignore.case = TRUE), 1, 0)
  data$BridgingRegimen_Binary_RT_ST_vs_rest <- ifelse(grepl("Systemic therapy|Radiotherapy|Steroids", data$BridgingRegimen, ignore.case = TRUE), 1, 0)
  data$BridgingRegimen_Binary_RT_NB_VS_rest <- ifelse(grepl("Radiotherapy|No bridgings", data$BridgingRegimen, ignore.case = TRUE), 1, 0)
  data$BridgingRegimen_Binary_NB_VS_rest <- ifelse(grepl("No bridging", data$BridgingRegimen, ignore.case = TRUE), 1, 0)
  data$Diagnosis_WHO2016_NUM <- as.factor(data$Diagnosis_WHO2016_DLBCL)
  data$Diagnosis_WHO2016_NUM <- as.numeric(data$Diagnosis_WHO2016_NUM)
  
  # Remove unnecessary columns
  columns_to_remove <- c("Age", "Diagnosis_WHO2016_DLBCL", "M1_Response", "BridgingRegimen", "LDH", "Deauville", "IPI", "screening_Stage")
  data <- data[, !colnames(data) %in% columns_to_remove]
  
  # Save data to Excel
  writexl::write_xlsx(data, paste("CAR_T_Data_binary", ".xlsx", sep = ""))
  
  return(data)
}


#' Preprocess data and create a file containing all data
#'
#' This function preprocesses the input data by renaming columns, converting variables, 
#' replacing missing values with zeros, adding binary columns, and saving the processed data to an Excel file.
#' @param data The dataframe containing the raw data
#' @return The dataframe containing the preprocessed data
preprocess_data_3 <- function(data) {
  # Rename columns
  colnames(data)[colnames(data) == "Age\r\n"] <- "Age"
  colnames(data)[colnames(data) == "Prior_LinesOFTherapy_N \r\n"] <- "Prior_LinesOFTherapy"
  colnames(data)[colnames(data) == "StemCell_transplantati\r\n"] <- "StemCell_transplantation"
  colnames(data)[colnames(data) == "ECOG_PS _screening"] <- "ECOG_PS_screening"
  colnames(data)[colnames(data) == "ECOG_PS _infusion"] <- "ECOG_PS_infusion"
  colnames(data)[colnames(data) == "StemCell_transplantation"] <- "StemCell_transplantation_binary"
  colnames(data)[colnames(data) == "Bulky_disease"] <- "Bulky_disease_binary"
  
  # Replace NAs with zeros
  data[is.na(data)] <- 0
  
  # Add binary columns
  data$M1_Response_binary_CR_PR <- ifelse(data$M1_Response %in% c("CR", "PR"), 1, 0)
  data$M1_Response_binary_CR <- ifelse(data$M1_Response %in% c("CR"), 1, 0)
  data$screening_Stage_binary <- ifelse(data$screening_Stage %in% c(3, 4), 1, 0)
  data$IPI_binary_2.3.4.5 <- ifelse(data$IPI %in% c(2, 3, 4, 5), 1, 0)
  data$IPI_binary_3.4.5 <- ifelse(data$IPI %in% c(3, 4, 5), 1, 0)
  data$LDH_binary <- ifelse((data$Gender == 1 & data$LDH > 248) | (data$Gender == 2 & data$LDH > 247), 1, 0)
  data$LDH_binary_2_times_ULN <- ifelse((data$Gender == 1 & data$LDH > (2*248)) | (data$Gender == 2 & data$LDH > (2*247)), 1, 0)
  data$Age_binary <- ifelse(data$Age >= 60, 1, 0)
  data$Deauville_binary_4.5 <- ifelse(data$Deauville %in% c(4, 5), 1, 0)
  data$Deauville_binary_3.4.5 <- ifelse(data$Deauville %in% c(3, 4, 5), 1, 0)
  data$BridgingRegimen_Binary_RT_vs_rest <- ifelse(grepl("Radiotherapy", data$BridgingRegimen, ignore.case = TRUE), 1, 0)
  data$BridgingRegimen_Binary_RT_ST_vs_res <- ifelse(grepl("Radiotherapy|Systemic therapy|Combination", data$BridgingRegimen, ignore.case = TRUE), 1, 0)
  data$BridgingRegimen_Binary_RT_ST_vs_rest <- ifelse(grepl("Systemic therapy|Radiotherapy|Steroids", data$BridgingRegimen, ignore.case = TRUE), 1, 0)
  data$BridgingRegimen_Binary_RT_NB_VS_rest <- ifelse(grepl("Radiotherapy|No bridgings", data$BridgingRegimen, ignore.case = TRUE), 1, 0)
  data$BridgingRegimen_Binary_NB_VS_rest <- ifelse(grepl("No bridging", data$BridgingRegimen, ignore.case = TRUE), 1, 0)
  
  # Convert categorical variables to factors
  data$Diagnosis_WHO2016_DLBCL <- as.factor(data$Diagnosis_WHO2016_DLBCL)
  data$BridgingRegimen <- as.factor(data$BridgingRegimen)
  data$M1_Response <- as.factor(data$M1_Response)
  
  # Convert factors to numeric
  data$Diagnosis_WHO2016_DLBCL <- as.numeric(data$Diagnosis_WHO2016_DLBCL)
  data$BridgingRegimen <- as.numeric(data$BridgingRegimen)
  data$M1_Response <- as.numeric(data$M1_Response)
  
  # Save data to Excel
  writexl::write_xlsx(data, paste("CAR_T_Data", ".xlsx", sep = ""))
  
  return(data)
}


library(vcd)

#' Visualize Correlation Matrix
#'
#' This function computes and visualizes the correlation matrix for a given dataset,
#' considering different correlation measures based on variable types: Pearson's correlation
#' for continuous variables, the point biserial correlation coefficient for the relationship
#' between continuous and binary variables, and the chi-square association measure for
#' categorical/binary variables.
#'
#' @param data A data frame containing the variables for which the correlation matrix
#'             needs to be computed and visualized.
#'
#' @return A modified version of the input data frame after potentially removing columns
#'         with high correlation.
#'
#' @details This function computes the correlation matrix for the given dataset using
#'          appropriate methods based on the types of variables involved. It then visualizes
#'          the correlation matrix as a heatmap and identifies highly correlated pairs
#'          with a correlation coefficient greater than 0.8. Columns involved in highly
#'          correlated pairs are potentially removed from the dataset.
#'

#'
#' @import vcd
#' @export
visualize_correlation <- function(data) {
  # Compute correlation matrix using appropriate methods based on variable types
  continuous_vars <- sapply(data, is.numeric)
  categorical_vars <- !continuous_vars
  
  # Pearson correlation for continuous variables
  cor_matrix_continuous <- cor(data[, continuous_vars], method = "pearson")
  
  # Point biserial correlation for the relationship between continuous and categorical/binary variables
  cor_matrix_mixed <- matrix(NA, nrow = sum(continuous_vars), ncol = sum(categorical_vars))
  for (i in which(continuous_vars)) {
    for (j in which(categorical_vars)) {
      point_biserial <- cor.test(data[, i], data[, j])
      cor_matrix_mixed[i, j] <- point_biserial$estimate
    }
  }
  
  # Chi-square association measure for categorical/binary variables
  cor_matrix_categorical <- matrix(NA, nrow = sum(categorical_vars), ncol = sum(categorical_vars))
  for (i in which(categorical_vars)) {
    for (j in which(categorical_vars)) {
      if (i != j) {
        chi_square <- assoc(data[, i], data[, j])
        cor_matrix_categorical[i, j] <- chi_square$statistics$chisq
      }
    }
  }
  
  # Combine correlation matrices
  cor_matrix_combined <- cbind(cor_matrix_continuous, cor_matrix_mixed)
  cor_matrix_combined <- rbind(cor_matrix_combined, cbind(t(cor_matrix_mixed), cor_matrix_categorical))
  
  col_scale <- colorRampPalette(c("white", "blue"))(20)
  
  # Create the heatmap using heatmap.2
  heatmap.2(cor_matrix_combined,
            col = col_scale,
            trace = "none",
            margins = c(8, 8),
            dendrogram = "none",
            keysize = 1.5,
            main = "Correlation Heatmap",
            labRow = colnames(cor_matrix_combined),
            labCol = colnames(cor_matrix_combined),
            cexRow = 0.7,
            cexCol = 0.7,
            cellnote = round(cor_matrix_combined, digits = 2),  # Round the correlation values
            notecol = "black",
            notecex = 0.7)
  
  # Identify highly correlated pairs
  highly_correlated_pairs <- subset(melt(cor_matrix_combined), value > 0.8 & Var1 != Var2)
  
  # Check if there are highly correlated pairs
  if (nrow(highly_correlated_pairs) > 0) {
    # Print a message before printing highly correlated pairs
    cat("Highly correlated pairs with correlation > 0.8:\n")
    
    # Print highly correlated pairs
    print(highly_correlated_pairs)
    # Drop one column from each highly correlated pair
    columns_to_drop <- unique(c(highly_correlated_pairs$Var1, highly_correlated_pairs$Var2))
    data <- data[, !(colnames(data) %in% columns_to_drop)]
  } else {
    # Print a message if there are no highly correlated pairs
    cat("No highly correlated pairs with correlation > 0.8 found.\n")
  }
  
  return(data)
}


#' Split data based on the balance of the target column
#'
#' This function splits the input data into training and testing sets based on the balance
#' of the target column. It displays the distribution of the target column and returns the
#' training and testing sets.
#' @param data The dataframe containing the data
#' @param target_column The name of the target column for splitting
#' @param test_percentage The percentage of data to be allocated for testing
#' @return A list containing the training and testing sets
split_data_based_on_target_balance <- function(data, target_column, test_percentage = 0.2) {
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


# Function: preselection_correlation_2
# Description: This function performs feature preselection and correlation analysis on the input data.
# Input:
#   - DATA: The dataset containing both independent and dependent variables.
#   - ynam: The name of the dependent variable column.
#   - output_file: The path where the Excel file containing preselected features and their statistics will be saved.
# Output:
#   - Preselection: A matrix containing preselected features along with their statistics.
preselection_correlation_2 <- function(DATA, ynam, output_file) {
  
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
  print(stats.s) 
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
  print(stat_mat)
  var.sel1.names = rownames(stat_mat[stat_mat[,'uni_p_value'] < 1,])
  var.sel1.stats = stat_mat[stat_mat[,'uni_p_value'] < 1,]
  z = 0
  correlatiens = c()
  
  # Loop through variables to check correlations
  # Loop through variables to check correlations
  for (i in 1:length(var.sel1.names)) {
    for (j in 1:length(var.sel1.names)) {
      if (i != j) {
        # Check the types of variables involved
        var1 <- var.sel1.names[i]
        var2 <- var.sel1.names[j]
        
        # Check if both variables are continuous
        if (is.numeric(DATA[, var1]) && is.numeric(DATA[, var2])) {
          # Calculate Pearson's correlation for continuous variables
          correlat <- cor(DATA[, var1], DATA[, var2])
          
          # Check for high correlation (abs > 0.8)
          if (abs(correlat) > 0.8) {
            z = z + 1
            correlatiens = rbind(correlatiens, c(var1, var.sel1.stats[i], var2, var.sel1.stats[j], correlat))
          }
        }
        # Check if both variables are categorical/binary
        else if (!is.numeric(DATA[, var1]) && !is.numeric(DATA[, var2])) {
          # Perform chi-square test for categorical variables
          chi_square <- chisq.test(DATA[, var1], DATA[, var2])
          
          # Check the p-value to determine significance
          if (chi_square$p.value < 0.05) {
            z = z + 1
            correlatiens = rbind(correlatiens, c(var1, var.sel1.stats[i], var2, var.sel1.stats[j], chi_square$p.value))
          }
        }
        # Check if one variable is continuous and the other is categorical/binary
        else {
          # Calculate point biserial correlation for mixed variables
          point_biserial <- cor.test(DATA[, var1], DATA[, var2])
          
          # Check the p-value to determine significance
          if (point_biserial$p.value < 0.05) {
            z = z + 1
            correlatiens = rbind(correlatiens, c(var1, var.sel1.stats[i], var2, var.sel1.stats[j], point_biserial$estimate))
          }
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
  cat("\n", "Selected features: ", nrow(Preselection), "\n")
  # Save results to an Excel file
  write.xlsx(Preselection, file = output_file, sheetName = "Preselection", row.names = FALSE)
  # Return the matrix of preselected features
  return(Preselection)
}


#' Perform forward selection logistic regression
#'
#' This function performs forward selection logistic regression to select variables based on their significance in predicting the outcome variable.
#'
#' @param DATA A data frame containing the dataset with variables to be analyzed.
#' @param ynam The name of the outcome variable in the dataset.
#' @param boot The number of bootstrap iterations for model validation. Default is 0 (no bootstrapping).
#' @param excel_export Logical indicating whether to export results to Excel. Default is FALSE.
#' @param outputdir The directory where the Excel file will be saved. Default is the working directory.
#' @param p_val_thresh The p-value threshold for variable selection. Default is 0.05.
#'
#' @return A list containing:
#'   \describe{
#'     \item{selected_variables}{A vector of names of the selected variables.}
#'     \item{Steps_together}{A matrix containing the coefficients, odds ratios, p-values, and other model statistics for each step of the selection process.}
#'   }
#'
#' @examples
#' \dontrun{
#' result <- forward_sel_logistic(DATA, "Outcome", boot = 100, excel_export = TRUE, outputdir = "./output", p_val_thresh = 0.05)
#' }
#'
#' @export
#' @author Dr. L.V. (Lisanne) van Dijk
#' @modifiedBy Hedieh Zabolinezhad
forward_sel_logistic <- function(DATA, ynam, boot, excel_export, outputdir, p_val_thresh) {
  
  # Force include candidate variables
  
  # Assign some initial variable
  sel <- c()
  TEST_pvalue <- 0  # p_val_thresh = 0.05
  
  run_i0 <- 0
  DATA <- DATA[, c("M1_Response_binary_CR", "Deauville_binary_4.5", "LDH", ynam)]
  feats_of_interest <- colnames(DATA)[!colnames(DATA) %in% ynam]
  feats_of_interest <- c("M1_Response_binary_CR", "Deauville_binary_4.5", "LDH")  # Features analyzed in this order
  Steps_together <- c()
  selected_variable_names <- c()  # Initialize vector to store selected variable names
  
  # Iterate until TEST_pvalue is smaller than threshold and the number of features in the selected features
  # is less than the total number of variables.
  while (TEST_pvalue < p_val_thresh & length(sel) < length(feats_of_interest)) {
    run_i0 <- run_i0 + 1
    
    ####-- delete variables already in the model --
    # This line makes a string of all the elements in 'sel' which are separated with '+'
    # If 'sel' is empty, it returns an empty string.
    selfor <- sel[1]
    if (length(sel) > 1) {
      for (z in 2:length(sel)) {
        selfor <- paste(selfor, sel[z], sep = " + ")
      }
    }
    selfor
    
    # erase the features added to 'sel'
    feats_of_interest <- feats_of_interest[!feats_of_interest %in%  sel]
    
    ####-- make reference model
    # If 'sel' contains a feature, first make a formula, then implement a model out of it.
    if (length(sel) > 0) {
      frm <- as.formula(paste(ynam, " ~ ", selfor))
      Model_ref <- lrm(formula = frm, x = TRUE, y = TRUE, data = DATA)
    }  
    
    # make a list 'model_predict' that can get any types of value
    Model_predict <- list(type = any)
    
    # make a list of matrices for different statistical parameters.
    stats.s <- matrix(NA, nrow = 18, ncol = length(feats_of_interest))
    stats.val <- matrix(NA, nrow = 11, ncol = length(feats_of_interest))
    stats.LLRT <- matrix(NA, nrow = 1, ncol = length(feats_of_interest))
    AIC_BIC <- matrix(NA, nrow = 2, ncol = length(feats_of_interest))
    beta <- matrix(NA, nrow = 2, ncol = length(feats_of_interest))
    
    # loop through the features remained in the feats_of_interest list
    for (i in 1:length(feats_of_interest)) {
      
      # make a new formula that contains the selected features and ith feature to compare with reference model
      frm <- as.formula(paste(ynam, " ~ ", selfor, " + ", feats_of_interest[i]))
      
      # This code makes a model based on the new list of features and handles any possible error 
      Model_predict[[i]] <- try(
        (lrm(formula = frm, x = TRUE, y = TRUE, data = DATA)))
      
      
      # use inherits to find out whether model_predict[[i]] is an object 'try-error'.
      # It means that is there any error in the previous step
      if (inherits(Model_predict[[i]], "try-error")) {
        stats.LLRT[i] <- NA  # set LLRT equal to null
        
        # If the length of the model is less than 3  set h = 'k'
      } else if (length(Model_predict[[i]]) < 3) {
        h <- "k"
        
        # Otherwise we have a good model and we can add the metrics of this model to our dataset
      } else {
        ###-- basic performance measures
        stats.s[, i] <- Model_predict[[i]]$stats
        AIC_BIC[, i] <- c(AIC(Model_predict[[i]]), BIC(Model_predict[[i]]))
        beta[, i] <- c(Model_predict[[i]]$coefficients[[feats_of_interest[i]]], exp(Model_predict[[i]]$coefficients[[feats_of_interest[i]]]))
        
        ###-- bootstrap per model
        if (boot > 0) {  # validate function is used to implement bootstrap model as follows 
          p <- validate(Model_predict[[i]], method = "boot", B = boot)  # print(p)
          stats.val[, i] <- p[, 5]  # Assign the fifth column of p to stat.val
        } else { stats.val[, i] <- NA }  # otherwise return null
        
        ###-- likelihood ratio test
        # If there are selected features in 'sel', it implements likelihood ratio test with the reference model
        # and store all the parameters in stats.LLRT. However, if there is no selected feature it just implement
        # Analysis of Variance (ANOVA), then it saves other components in stats.LLRT
        if (length(sel) > 0) {
          LLRT <- lrtest(Model_ref, Model_predict[[i]])
          stats.LLRT[i] <- LLRT$stats[3]
        } else {
          LLRT <- anova(Model_predict[[i]])
          stats.LLRT[i] <- LLRT["TOTAL", "P"]
        }
        
      }
    }
    
    # Assign some names to columns and rows of different datasets
    colnames(stats.s) <- feats_of_interest
    rownames(stats.s) <- names(Model_predict[[i]]$stats)
    rownames(stats.LLRT) <- "p_LRT"
    
    # paste0 concatenate val_ with all the available parameters in the list
    rownames(stats.val) <- paste0("val_", c("Dxy", "R2", "Intercept", "Slope", "Emax", "D", "U", "Q", "B", "g", "gp"))
    rownames(AIC_BIC) <- c("AIC", "BIC")
    rownames(beta) <- c("beta", "OR")
    
    # implement some operation on the first row of stats.val matrix
    stats.val[1, ] <- (stats.val[1, ] + 1) / 2
    
    # The all the brackets here are slicing some rows from different matrices, then 'rbind' binds all the 
    # rows on top of each other to make a dataframe. Finally, 't' transpose the matrix to change the place
    # of rows and columns
    vallalles <- t(rbind(stats.s[c(5:6, 10), ], stats.val[1:2, ], beta, AIC_BIC, stats.LLRT))
    
    # this line order the matrix based on p_LRT in an ascending form.
    vgl <- vallalles[order(vallalles[,"p_LRT"]),]
    
    
    # round the matrix float numbers
    round(vgl, 3)
    
    # assigning the smallest p-value to 'TEST_pvalue'. But, if it is equal to null value assign 1 to 'TEST_pvalue'
    TEST_pvalue <- vgl[1,"p_LRT"]
    if (is.na(as.numeric(TEST_pvalue))) {
      TEST_pvalue <- 1
    }
    
    # extract the name of the variable with the smallest p-value
    selected_variable <- rownames(vgl)[1]
    view(selected_variable)
    
    # add it to the 'sel' which is the list of selected features 
    sel <- c(sel, selected_variable)
    selected_variable_names <- c(selected_variable_names, selected_variable)  # Store selected variable name
    
    # Show the selected feature and also p-value
    cat(paste0("\n"))
    print(paste0(round(TEST_pvalue, 4), " --> ", selfor, " + ", selected_variable))
    
    
    
    # Make a formula based on old selected features and the new one
    frm <- as.formula(paste(ynam, " ~ ", selfor, " + ", selected_variable))
    
    dd <<- datadist(DATA)
    options(datadist = "dd")
    
    # launch a regression model for this new set of variables
    # dd <- datadist; options(datadist = 'dd')
    step_model <- lrm(formula = frm, x = TRUE, y = TRUE,  data = DATA) 
    ss <- summary(step_model)
    
    # make a matrix (dataframe) from coefficients the exponential of them and the variances of these coefficients.
    step_model_coef <- cbind(step_model$coefficients, exp(step_model$coefficients),c(NA, anova(step_model)[1:(length(step_model$coefficients)-1),3]))
    
    # make a matrix from the first model in vlg and the coefficients
    step_model_params <- matrix(NA,  nrow(step_model_coef), 3 + length(vgl[1,]));
    step_model_params[,1:3] <- step_model_coef
    step_model_params[1,4:(3 + length(vgl[1,]))] <- vgl[1,]
    
    # Assign names to the columns and rows of the created matrix.
    if (length(sel) > 1){
      rownames(step_model_params) <- rownames(step_model_coef)
      colnames(step_model_params) <- c(c("beta","OR","p_val"),  names(vgl[1,]))
    }else{
      rownames(step_model_params) <- rownames(step_model_coef)
      colnames(step_model_params) <- c(c("beta","OR","p_val"),  names(vgl[1,]))
    }
    # Add the mentioned matrix as a row of the final matrix 'Steps_together'
    Steps_together <- rbind(Steps_together, step_model_params)
  }
  round(Steps_together, 2)
  
  # Return both the matrix and the names of selected variables
  return(list(selected_variables = selected_variable_names, Steps_together = Steps_together))
}


#' Function to create baseline logistic regression model and save it
#' 
#' This function builds a baseline logistic regression model using the specified final variables on the training dataset.
#' It then evaluates the model's performance on both the training and testing datasets, calculating various performance metrics such as AUC, specificity, accuracy, precision, recall, and F1 score.
#' The function also plots the AUC-ROC curves for both the training and testing datasets.
#' Finally, it saves the trained baseline model and the performance statistics to the specified output directory.
#' 
#' @param final_vars A character vector containing the names of the final predictor variables selected for the model.
#' @param train The training dataset as a data frame.
#' @param test The testing dataset as a data frame.
#' @param y The name of the target variable in the datasets.
#' @param outputdir The directory path where the trained model and performance statistics will be saved.
#' 
#' @return A matrix containing the performance statistics of the baseline logistic regression model on both the training and testing datasets.
#' 
#' @examples
#' \dontrun{
#' stats <- create_baseline_model(selected_variables, train_data, test_data, "M3_binary_Response", "output/")
#' }
#' 
#' @export
# Function to create baseline model and save it
create_baseline_model <- function(final_vars, train, test, y, outputdir) {
  
  thresh <- "suv2.5"
  
  # Initialize the statistics matrix with row and column names
  statistics <- matrix(NA, nrow = 2, ncol = 9,
                       dimnames = list(c("Train", "Test"),
                                       c("AUC", "Specificity", "Accuracy", "Precision",
                                         "Recall", "F1 Score", "True Positive", "True Negative", "Threshold")))
  
  # Check if final variables are not empty
  if (!is.null(length(final_vars))) {
    # Baseline model performance on training data
    frm <- as.formula(paste(y, " ~ ", paste(final_vars, collapse = ' + ')))
    base_line_model <- lrm(formula = frm, x = T, y = T, data = train)
    predict_train_fit <- predict(base_line_model, train)
    
    # Baseline model performance on test data
    predict_test_fit <- predict(base_line_model, test)
    
    # Convert probabilities to class predictions (0 or 1)
    predict_train_fit_classes <- ifelse(predict_train_fit > 0.5, 1, 0)
    predict_test_fit_classes <- ifelse(predict_test_fit > 0.5, 1, 0)
    
    # Calculate the AUC for train and test datasets
    roc_curve_train <- roc(train[[y]], predict_train_fit)
    auc_value_train <- auc(roc_curve_train)
    
    roc_curve_test <- roc(test[[y]], predict_test_fit)
    auc_value_test <- auc(roc_curve_test)
    
    # Performance statistics for train dataset
    train_conf_matrix <- confusionMatrix(factor(predict_train_fit_classes), factor(train[[y]]))
    
    # Performance statistics for test dataset
    test_conf_matrix <- confusionMatrix(as.factor(predict_test_fit_classes), as.factor(test[[y]]))
    
    # Store performance statistics in the matrix for Train
    statistics["Train", "AUC"] <- auc_value_train
    statistics["Train", "Specificity"] <- train_conf_matrix$byClass['Specificity']
    statistics["Train", "Accuracy"] <- train_conf_matrix$overall['Accuracy']
    statistics["Train", "Precision"] <- train_conf_matrix$byClass['Pos Pred Value']
    statistics["Train", "Recall"] <- train_conf_matrix$byClass['Recall']
    statistics["Train", "F1 Score"] <- train_conf_matrix$byClass['F1']
    statistics["Train", "True Positive"] <- train_conf_matrix$table[2, 2]
    statistics["Train", "True Negative"] <- train_conf_matrix$table[1, 1]
    
    # Store performance statistics in the matrix for Test
    statistics["Test", "AUC"] <- auc_value_test
    statistics["Test", "Specificity"] <- test_conf_matrix$byClass['Specificity']
    statistics["Test", "Accuracy"] <- test_conf_matrix$overall['Accuracy']
    statistics["Test", "Precision"] <- test_conf_matrix$byClass['Pos Pred Value']
    statistics["Test", "Recall"] <- test_conf_matrix$byClass['Recall']
    statistics["Test", "F1 Score"] <- test_conf_matrix$byClass['F1']
    statistics["Test", "True Positive"] <- test_conf_matrix$table[2, 2]
    statistics["Test", "True Negative"] <- test_conf_matrix$table[1, 1]
    
    # Plot AUC-ROC curves
    auc_plot_train <- ggroc(roc_curve_train, legacy.axes = TRUE) +
      geom_line(aes(x = 1 - specificity, y = sensitivity), color = "blue", size = 1.5) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
      theme_minimal() +
      labs(title = "AUC-ROC Curve (Train)",
           x = "False Positive Rate",
           y = "True Positive Rate",
           caption = paste("AUC =", round(auc_value_train, 3)), color = "red")
    
    auc_plot_test <- ggroc(roc_curve_test, legacy.axes = TRUE) +
      geom_line(aes(x = 1 - specificity, y = sensitivity), color = "blue", size = 1.5) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
      theme_minimal() +
      labs(title = "AUC-ROC Curve (Test)",
           x = "False Positive Rate",
           y = "True Positive Rate",
           caption = paste("AUC =", round(auc_value_test, 3)),
           color = "red")
    
    # Arrange plots side by side
    plots <- grid.arrange(auc_plot_train, auc_plot_test, ncol = 2)
    
    # Print the AUC plots
    print(plots)
    
    # Save the baseline model
    saveRDS(base_line_model, file = paste0(outputdir, "baseline_model.rds"))
    
    # Save the output statistics
    # write.table(statistics, file = paste0(outputdir, "baseline_model_statistics.txt"), row.names = TRUE)
  }
  
  return(statistics)
}



#' Execute the workflow with feature selection and logistic regression modeling
#'
#' This function executes the workflow, including feature preselection, logistic regression modeling with forward selection, and evaluation of model performance.
#'
#' @details
#' The workflow consists of the following steps:
#' 1. Loading required packages.
#' 2. Loading and preprocessing the data.
#' 3. Visualizing the correlation heatmap and handling highly correlated pairs of features.
#' 4. Performing feature preselection and correlation analysis.
#' 5. Filtering the data based on preselected features.
#' 6. Splitting the data into training and testing sets using stratified splitting.
#' 7. Performing forward selection logistic regression on the training data.
#' 8. Creating a baseline logistic regression model using the selected variables and evaluating its performance on both training and testing datasets.
#'
#' @return A summary of the workflow execution including preselected features, selected variables, and model performance statistics.
#'
#' @examples
#' \dontrun{
#' run_analysis()
#' }
#' 
#' @export
run_analysis <- function() {
  
  load_packages()
  
  # Load the data and preprocessing
  CAR_T_Data <- load_data("C:/Users/ZabolinezhH/Desktop/new_version/CART_BaseFeatures_last.xlsx")
  
  CAR_T_Data_binary <- preprocess_data_2(CAR_T_Data)
  CAR_T_Data_binary_continous <- preprocess_data_3(CAR_T_Data)
  target_col_name <- "M3_binary_Response"
  
  # Remove the label before correlation visualization
  CAR_T_Data_binary_continous_No_Target <- CAR_T_Data_binary_continous[, -which(colnames(CAR_T_Data_binary_continous) == target_col_name)]
  
  # Visualize the correlation heatmap and handle highly correlated pairs
  CAR_T_Data_new <- visualize_correlation(CAR_T_Data_binary_continous_No_Target)
  
  # Perform feature preselection and correlation analysis
  pre_selected_features <- preselection_correlation_2(CAR_T_Data_binary_continous, target_col_name)
  print(pre_selected_features)
  print(rownames(pre_selected_features))
  
  # Check if pre-selected features are not empty
  if (nrow(pre_selected_features) > 0) {
    rownames(pre_selected_features) <- c("M1_Response_binary_CR", "Deauville_binary_4.5", "LDH")
    view(pre_selected_features)
    
    # Add target column name to the selected features names
    pre_selected_featNam_with_label <- c(target_col_name, rownames(pre_selected_features))
    
    # Filter the training and testing data based on pre-selected features
    CAR_T_Data_binary_continous <- CAR_T_Data_binary_continous[, intersect(colnames(CAR_T_Data_binary_continous), pre_selected_featNam_with_label)]
    view(CAR_T_Data_binary_continous)
    
    # Split the data based on the balance of the target column with stratified splitting
    splited_data <- split_data_balance_target(CAR_T_Data_binary_continous, target_col_name)
    
    # Check if the result is not NULL before using it
    if (!is.null(splited_data)) {
      # Access the training and testing sets
      train_data <- splited_data$train_data
      test_data <- splited_data$test_data
    
      
      # Perform forward selection logistic regression
      forward_sel_logistic <- forward_sel_logistic(train_data, target_col_name, boot = 0, excel_export = 1, outputdir = "C:/Users/ZabolinezhH/Desktop/new_version/", p_val_thresh = 0.05)
      print(forward_sel_logistic)
      
      # Get the names of the model predictors in order of their significance.
      selected_variables <- forward_sel_logistic$selected_variables
      print(forward_sel_logistic$steps_together)
      
      # Print or use the selected variable names as needed
      print("\n")
      
      # Create a baseline logistic regression model and evaluate its performance
      stat <- create_baseline_model(selected_variables, train_data, test_data, target_col_name, outputdir = "C:/Users/ZabolinezhH/Desktop/new_version/") 
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