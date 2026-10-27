setwd("C:/Users/Dolce/Documents/FINAL DATA ANALYSIS 1 OF 3")# ============================================================
# CHUNK 1 OF 25
# LIBRARIES AND PROJECT FOLDERS
# ============================================================

# Install packages once only if they are not already installed
install.packages(c(
  "dplyr",
  "stringr",
  "janitor",
  "psych",
  "writexl",
  "car",
  "lmtest",
  "rsample",
  "parsnip",
  "workflows",
  "tune",
  "yardstick",
  "ggplot2",
  "corrplot",
  "broom",
  "ggpubr",
  "patchwork",
  "scales",
  "knitr",
  "kableExtra",
  "shiny",
  "DT"
))

# Load libraries each time you open RStudio
library(dplyr)
library(stringr)
library(janitor)
library(psych)
library(writexl)
library(car)
library(lmtest)
library(rsample)
library(parsnip)
library(workflows)
library(tune)
library(yardstick)
library(ggplot2)
library(corrplot)
library(broom)
library(ggpubr)
library(patchwork)
library(scales)
library(knitr)
library(kableExtra)
library(shiny)
library(DT)

# Create project folders if they do not already exist
dir.create("data", showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)

# Confirm the active working directory
getwd()

# Confirm project folders
list.files()
# ============================================================
# CHUNK 2 OF 25
# IMPORT AND INITIAL INSPECTION
# ============================================================

# Import the dataset
mydata <- read.csv(
  "data/mydata.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# View the dataset in RStudio
View(mydata)

# Initial checks
head(mydata)                              # First rows
dim(mydata)                               # Number of rows and columns
names(mydata)                             # Variable names
str(mydata)                               # Variable types and non-missing observations
summary(mydata)                           # Statistical summary
colSums(is.na(mydata))                    # Missing values by variable
sum(duplicated(mydata))                   # Fully duplicated rows
round(colMeans(is.na(mydata)) * 100, 1)  # Percentage missing per variable

# Keep an unchanged copy of the original dataset
rawdata <- mydata

# Create a working copy for subsequent cleaning and analysis
analysisdata <- mydata
# ============================================================
# CHUNK 3 OF 25
# CLEAN TEXT VALUES
# ============================================================

# Remove unnecessary spaces in all text variables
analysisdata <- analysisdata %>%
  mutate(
    across(
      where(is.character),
      ~ str_squish(.x)
    )
  )

# Change empty text cells into NA values
analysisdata <- analysisdata %>%
  mutate(
    across(
      where(is.character),
      ~ na_if(.x, "")
    )
  )

# Check missing values after cleaning
colSums(is.na(analysisdata))
# ============================================================
# CHUNK 4 OF 25
# ASSESS MISSINGNESS
# ============================================================

# Create a table showing missingness for each variable
missingness_table <- data.frame(
  original_variable = names(analysisdata),
  missing_n = colSums(is.na(analysisdata)),
  missing_percent = round(colMeans(is.na(analysisdata)) * 100, 1)
) %>%
  arrange(desc(missing_percent))

# View the missingness table
missingness_table

# Export the missingness table
write.csv(
  missingness_table,
  "output/missingness_before_column_removal.csv",
  row.names = FALSE
)

# Identify variables with more than 40% missing data
missing_threshold <- 40

columns_to_remove <- missingness_table %>%
  filter(missing_percent > missing_threshold) %>%
  pull(original_variable)

# Display columns that will be removed
columns_to_remove
# ============================================================
# CHUNK 5 OF 25
# REMOVE COLUMNS WITH EXTENSIVE MISSINGNESS
# ============================================================

# Remove variables with more than 40% missing data
analysisdata <- analysisdata %>%
  select(-all_of(columns_to_remove))

# Check the remaining dataset
dim(analysisdata)
names(analysisdata)

# Create a report of removed variables
removed_columns_report <- missingness_table %>%
  filter(original_variable %in% columns_to_remove)

removed_columns_report

# Export reports
write.csv(
  removed_columns_report,
  "output/columns_removed_extensive_missingness.csv",
  row.names = FALSE
)

# Calculate missingness after column removal
missingness_after_removal <- data.frame(
  variable = names(analysisdata),
  missing_n = colSums(is.na(analysisdata)),
  missing_percent = round(colMeans(is.na(analysisdata)) * 100, 1)
) %>%
  arrange(desc(missing_percent))

missingness_after_removal

write.csv(
  missingness_after_removal,
  "output/missingness_after_column_removal.csv",
  row.names = FALSE
)
# ============================================================
# CHUNK 6 OF 25
# REMOVE TIMESTAMP AND RENAME VARIABLES
# ============================================================

# Remove Timestamp
analysisdata <- analysisdata %>%
  select(-Timestamp)

# Rename all remaining 49 variables with underscores
names(analysisdata) <- c(
  
  # 1–2 Demographics
  "sex",
  "year_study",
  
  # 3–11 Digital access and previous exposure
  "smartphone",
  "computer",
  "internet_access",
  "internet_location",
  "academic_device_use",
  "formal_computer_training",
  "computer_training_type",
  "ehr_exposure",
  "ehr_exposure_level",
  
  # 12–22 Educational and institutional factors
  "computer_lab",
  "institution_internet",
  "informatics_taught_nurse_tutor",
  "informatics_taught_other_professional",
  "informatics_lectures",
  "tutor_confidence",
  "computer_confidence",
  "online_learning_exposure",
  "digital_academic_communication",
  "ict_resources_adequate",
  "school_ict_resources",
  
  # 23–48 C-NICAS informatics-competency items
  "cnicas_use_ict_devices",
  "cnicas_use_ict_tools",
  "cnicas_search_online_information",
  "cnicas_evaluate_online_information",
  "cnicas_use_patient_data_decisions",
  "cnicas_record_electronic_records",
  "cnicas_help_patients_find_information",
  "cnicas_understand_information_standards",
  "cnicas_understand_standard_nursing_terms",
  "cnicas_follow_information_ethics",
  "cnicas_support_technology_healthcare",
  "cnicas_advocate_health_technology",
  "cnicas_professional_judgment_technology",
  "cnicas_nurse_role_ict_systems",
  "cnicas_electronic_patient_documentation",
  "cnicas_identify_ehr_errors",
  "cnicas_recognise_data_quality_gaps",
  "cnicas_identify_system_device_problems",
  "cnicas_report_ict_problems",
  "cnicas_patient_safety_system_downtime",
  "cnicas_use_ict_patient_care",
  "cnicas_use_ict_clinical_decisions",
  "cnicas_ict_nurse_patient_relationship",
  "cnicas_identify_clinical_system_components",
  "cnicas_distinguish_health_record_systems",
  "cnicas_informatics_improves_quality",
  
  # 49 Informatics experience source
  "informatics_experience_source"
)

# Verify the number and names of variables
ncol(analysisdata)
names(analysisdata)
# ============================================================
# CHUNK 7 OF 25
# IDENTIFY C-NICAS ITEMS
# ============================================================

# Check total number of remaining variables
ncol(analysisdata)

# Identify all C-NICAS variables
cnicas_items <- names(analysisdata)[
  grepl("cnicas", names(analysisdata))
]

# Check the number of C-NICAS items
length(cnicas_items)

# Display the 26 C-NICAS variable names
cnicas_items
# ============================================================
# CHUNK 8 OF 25
# CORRECT DEVICE-USE CATEGORY
# ============================================================

# Correct the spelling of "Mothly" to "Monthly"
analysisdata <- analysisdata %>%
  mutate(
    academic_device_use = dplyr::recode(
      as.character(academic_device_use),
      "Mothly" = "Monthly"
    )
  )

# Check the categories after correction
table(analysisdata$academic_device_use, useNA = "ifany")
# ============================================================
# CHUNK 9 OF 25
# CONVERT C-NICAS RESPONSES TO NUMERIC
# ============================================================

# Convert C-NICAS responses to numeric values from 1 to 5
analysisdata <- analysisdata %>%
  mutate(
    across(
      all_of(cnicas_items),
      ~ as.numeric(str_extract(as.character(.x), "[1-5]"))
    )
  )

# Check minimum, maximum, and missing values for every C-NICAS item
cnicas_check <- data.frame(
  item = cnicas_items,
  minimum = sapply(
    analysisdata[cnicas_items],
    min,
    na.rm = TRUE
  ),
  maximum = sapply(
    analysisdata[cnicas_items],
    max,
    na.rm = TRUE
  ),
  missing_n = sapply(
    analysisdata[cnicas_items],
    function(x) sum(is.na(x))
  )
)

cnicas_check
# ============================================================
# CHUNK 10 OF 25
# C-NICAS MISSINGNESS ASSESSMENT
# ============================================================

# Count missing C-NICAS items for each participant
analysisdata$cnicas_missing_items <- rowSums(
  is.na(analysisdata[cnicas_items])
)

# Count completed C-NICAS items for each participant
analysisdata$cnicas_answered_items <- rowSums(
  !is.na(analysisdata[cnicas_items])
)

# Show the number of participants with 0, 1, 2, etc. missing items
table(analysisdata$cnicas_missing_items, useNA = "ifany")

# Missing values by C-NICAS item
cnicas_missing_by_item <- data.frame(
  item = cnicas_items,
  missing_n = colSums(is.na(analysisdata[cnicas_items])),
  missing_percent = round(
    colMeans(is.na(analysisdata[cnicas_items])) * 100,
    1
  )
)

cnicas_missing_by_item

# Export item-level C-NICAS missingness results
write.csv(
  cnicas_missing_by_item,
  "output/cnicas_missingness_by_item.csv",
  row.names = FALSE
)
# ============================================================
# CHUNK 11 OF 25
# CALCULATE C-NICAS TOTAL AND MEAN SCORES
# ============================================================

# Minimum number of completed items required: 80% of 26 items
minimum_cnicas_items <- ceiling(length(cnicas_items) * 0.80)

minimum_cnicas_items

# Calculate total and mean C-NICAS scores
analysisdata <- analysisdata %>%
  mutate(
    cnicas_total = ifelse(
      cnicas_answered_items >= minimum_cnicas_items,
      rowSums(across(all_of(cnicas_items)), na.rm = TRUE),
      NA_real_
    ),
    cnicas_mean = ifelse(
      cnicas_answered_items >= minimum_cnicas_items,
      rowMeans(across(all_of(cnicas_items)), na.rm = TRUE),
      NA_real_
    )
  )

# Review C-NICAS scores
summary(analysisdata$cnicas_total)
summary(analysisdata$cnicas_mean)

mean(analysisdata$cnicas_mean, na.rm = TRUE)
sd(analysisdata$cnicas_mean, na.rm = TRUE)
min(analysisdata$cnicas_mean, na.rm = TRUE)
max(analysisdata$cnicas_mean, na.rm = TRUE)

sum(!is.na(analysisdata$cnicas_mean))
sum(is.na(analysisdata$cnicas_mean))
# ============================================================
# CHUNK 12 OF 25
# C-NICAS RELIABILITY ANALYSIS
# ============================================================

# Calculate Cronbach's alpha for the 26 C-NICAS items
cnicas_reliability <- psych::alpha(
  analysisdata[cnicas_items],
  check.keys = FALSE
)

# Display overall reliability results
cnicas_reliability
cnicas_reliability$total$raw_alpha

# Create item-level reliability table
reliability_table <- data.frame(
  item = cnicas_items,
  item_mean = round(cnicas_reliability$item.stats$mean, 2),
  item_sd = round(cnicas_reliability$item.stats$sd, 2),
  corrected_item_total_correlation = round(
    cnicas_reliability$item.stats$r.drop,
    3
  ),
  alpha_if_item_deleted = round(
    cnicas_reliability$alpha.drop$raw_alpha,
    3
  )
)

reliability_table
View(reliability_table)

# Export item-level reliability results
write.csv(
  reliability_table,
  "output/cnicas_reliability_results.csv",
  row.names = FALSE
)

# Create and export overall reliability summary
reliability_summary <- data.frame(
  scale = "C-NICAS overall scale",
  number_of_items = length(cnicas_items),
  sample_size = nrow(analysisdata),
  cronbach_alpha = round(cnicas_reliability$total$raw_alpha, 3)
)

reliability_summary

write.csv(
  reliability_summary,
  "output/cnicas_overall_reliability_summary.csv",
  row.names = FALSE
)
# ============================================================
# CHUNK 13 OF 25
# CREATE BINARY AND ORDERED PREDICTORS
# ============================================================

# Create a dataset for subsequent analyses
model_data <- analysisdata

# Create binary predictors
model_data <- model_data %>%
  mutate(
    smartphone_yes = ifelse(smartphone == "Yes", 1, 0),
    computer_yes = ifelse(computer == "Yes", 1, 0),
    internet_access_yes = ifelse(internet_access == "Yes", 1, 0),
    formal_computer_training_yes = ifelse(
      formal_computer_training == "Yes", 1, 0
    ),
    ehr_exposure_yes = ifelse(ehr_exposure == "Yes", 1, 0),
    computer_lab_yes = ifelse(computer_lab == "Yes", 1, 0),
    institution_internet_yes = ifelse(
      institution_internet == "Yes", 1, 0
    ),
    informatics_nurse_tutor_yes = ifelse(
      informatics_taught_nurse_tutor == "Yes", 1, 0
    ),
    informatics_other_professional_yes = ifelse(
      informatics_taught_other_professional == "Yes", 1, 0
    ),
    tutor_confidence_yes = ifelse(tutor_confidence == "Yes", 1, 0),
    online_learning_yes = ifelse(
      online_learning_exposure == "Yes", 1, 0
    ),
    digital_communication_yes = ifelse(
      digital_academic_communication == "Yes", 1, 0
    )
  )

# List binary predictor names
binary_predictors <- c(
  "smartphone_yes",
  "computer_yes",
  "internet_access_yes",
  "formal_computer_training_yes",
  "ehr_exposure_yes",
  "computer_lab_yes",
  "institution_internet_yes",
  "informatics_nurse_tutor_yes",
  "informatics_other_professional_yes",
  "tutor_confidence_yes",
  "online_learning_yes",
  "digital_communication_yes"
)

# Check binary predictor coding
lapply(model_data[binary_predictors], table, useNA = "ifany")
# ============================================================
# CHUNK 14 OF 25
# CODE ORDERED PREDICTORS
# ============================================================

# Convert ordered categories into numeric scores
model_data <- model_data %>%
  mutate(
    academic_device_use_score = dplyr::recode(
      as.character(academic_device_use),
      "Never" = 0,
      "Rarely" = 1,
      "Monthly" = 2,
      "Weekly" = 3,
      "Daily" = 4,
      .default = NA_real_
    ),
    
    computer_confidence_score = dplyr::recode(
      as.character(computer_confidence),
      "Very low" = 1,
      "Low" = 2,
      "Moderate" = 3,
      "High" = 4,
      "Very High" = 5,
      .default = NA_real_
    ),
    
    ict_resources_score = dplyr::recode(
      as.character(ict_resources_adequate),
      "No" = 0,
      "Partly" = 1,
      "Yes" = 2,
      .default = NA_real_
    ),
    
    ehr_exposure_level_score = dplyr::recode(
      as.character(ehr_exposure_level),
      "Not applicable" = 0,
      "Observed Only" = 1,
      "Assisted use" = 2,
      "Independent" = 3,
      "Formal Training" = 4,
      .default = NA_real_
    )
  )

# Check that every category was coded correctly
table(
  model_data$academic_device_use,
  model_data$academic_device_use_score,
  useNA = "ifany"
)

table(
  model_data$computer_confidence,
  model_data$computer_confidence_score,
  useNA = "ifany"
)

table(
  model_data$ict_resources_adequate,
  model_data$ict_resources_score,
  useNA = "ifany"
)

table(
  model_data$ehr_exposure_level,
  model_data$ehr_exposure_level_score,
  useNA = "ifany"
)
# ============================================================
# CHUNK 15 OF 25
# DESCRIPTIVE STATISTICS AND FREQUENCIES
# ============================================================

# Descriptive statistics for the C-NICAS mean score
cnicas_descriptives <- data.frame(
  measure = c(
    "Sample size",
    "Mean C-NICAS score",
    "Standard deviation",
    "Median",
    "Minimum",
    "Maximum"
  ),
  value = c(
    sum(!is.na(model_data$cnicas_mean)),
    round(mean(model_data$cnicas_mean, na.rm = TRUE), 2),
    round(sd(model_data$cnicas_mean, na.rm = TRUE), 2),
    round(median(model_data$cnicas_mean, na.rm = TRUE), 2),
    round(min(model_data$cnicas_mean, na.rm = TRUE), 2),
    round(max(model_data$cnicas_mean, na.rm = TRUE), 2)
  )
)

cnicas_descriptives

write.csv(
  cnicas_descriptives,
  "output/cnicas_descriptive_statistics.csv",
  row.names = FALSE
)

# Frequency and percentage table for binary predictors
binary_frequency_table <- do.call(
  rbind,
  lapply(binary_predictors, function(variable) {
    
    tab <- table(model_data[[variable]])
    total <- sum(tab)
    
    data.frame(
      predictor = variable,
      category = c("No", "Yes"),
      frequency = as.numeric(tab[c("0", "1")]),
      percent = round(as.numeric(tab[c("0", "1")]) / total * 100, 1)
    )
  })
)

binary_frequency_table

write.csv(
  binary_frequency_table,
  "output/binary_predictor_frequencies.csv",
  row.names = FALSE
)

# View original response categories for ordered predictors
table(model_data$academic_device_use, useNA = "ifany")
table(model_data$computer_confidence, useNA = "ifany")
table(model_data$ict_resources_adequate, useNA = "ifany")
table(model_data$ehr_exposure_level, useNA = "ifany")
# ============================================================
# CHUNK 16 OF 25
# SPEARMAN CORRELATIONS
# ============================================================

# Spearman correlation: Academic device-use frequency
cor_device_use <- cor.test(
  model_data$academic_device_use_score,
  model_data$cnicas_mean,
  method = "spearman",
  exact = FALSE
)

# Spearman correlation: Computer confidence
cor_computer_confidence <- cor.test(
  model_data$computer_confidence_score,
  model_data$cnicas_mean,
  method = "spearman",
  exact = FALSE
)

# Spearman correlation: Perceived ICT-resource adequacy
cor_ict_resources <- cor.test(
  model_data$ict_resources_score,
  model_data$cnicas_mean,
  method = "spearman",
  exact = FALSE
)

# Spearman correlation: EHR exposure level
cor_ehr_exposure <- cor.test(
  model_data$ehr_exposure_level_score,
  model_data$cnicas_mean,
  method = "spearman",
  exact = FALSE
)

# Combine correlation findings into one results table
correlation_results <- data.frame(
  predictor = c(
    "Academic device-use frequency",
    "Computer confidence",
    "Perceived ICT-resource adequacy",
    "EHR exposure level"
  ),
  spearman_rho = round(
    c(
      unname(cor_device_use$estimate),
      unname(cor_computer_confidence$estimate),
      unname(cor_ict_resources$estimate),
      unname(cor_ehr_exposure$estimate)
    ),
    3
  ),
  p_value = format.pval(
    c(
      cor_device_use$p.value,
      cor_computer_confidence$p.value,
      cor_ict_resources$p.value,
      cor_ehr_exposure$p.value
    ),
    digits = 3,
    eps = 0.001
  )
)

correlation_results

write.csv(
  correlation_results,
  "output/spearman_correlation_results.csv",
  row.names = FALSE
)
# ============================================================
# CHUNK 17 OF 25
# INDEPENDENT-SAMPLES T-TESTS
# ============================================================

# Select binary predictors for group comparisons
binary_predictors_for_tests <- c(
  "computer_yes",
  "internet_access_yes",
  "formal_computer_training_yes",
  "ehr_exposure_yes",
  "computer_lab_yes",
  "institution_internet_yes",
  "informatics_nurse_tutor_yes",
  "informatics_other_professional_yes",
  "tutor_confidence_yes",
  "online_learning_yes",
  "digital_communication_yes"
)

# Labels for reporting
binary_predictor_labels <- c(
  "Computer ownership",
  "Regular internet access",
  "Formal computer training",
  "EHR exposure",
  "Computer laboratory available",
  "Institutional internet available",
  "Informatics taught by nurse tutor",
  "Informatics taught by another professional",
  "Tutor confidence",
  "Prior online-learning exposure",
  "Digital academic communication"
)

# Run independent-samples t-tests
binary_results <- do.call(
  rbind,
  lapply(seq_along(binary_predictors_for_tests), function(i) {
    
    variable <- binary_predictors_for_tests[i]
    
    temp_data <- model_data %>%
      select(cnicas_mean, all_of(variable)) %>%
      filter(!is.na(cnicas_mean), !is.na(.data[[variable]]))
    
    group_counts <- table(temp_data[[variable]])
    
    test_result <- t.test(
      temp_data$cnicas_mean ~ temp_data[[variable]]
    )
    
    group_means <- tapply(
      temp_data$cnicas_mean,
      temp_data[[variable]],
      mean
    )
    
    yes_minus_no <- group_means["1"] - group_means["0"]
    
    data.frame(
      predictor = binary_predictor_labels[i],
      n_no = as.numeric(group_counts["0"]),
      n_yes = as.numeric(group_counts["1"]),
      mean_no = round(group_means["0"], 2),
      mean_yes = round(group_means["1"], 2),
      mean_difference_yes_minus_no = round(yes_minus_no, 2),
      lower_95_CI = round(-test_result$conf.int[2], 2),
      upper_95_CI = round(-test_result$conf.int[1], 2),
      p_value = format.pval(
        test_result$p.value,
        digits = 3,
        eps = 0.001
      )
    )
  })
)

binary_results

write.csv(
  binary_results,
  "output/binary_predictor_ttest_results.csv",
  row.names = FALSE
)
# ============================================================
# CHUNK 18 OF 25
# ANOVA AND TUKEY POST-HOC TESTS
# ============================================================

# Sex
anova_sex <- aov(
  cnicas_mean ~ sex,
  data = model_data
)

summary(anova_sex)

# Current year of study
anova_year_study <- aov(
  cnicas_mean ~ year_study,
  data = model_data
)

summary(anova_year_study)

# Type of prior computer training
training_type_data <- model_data %>%
  filter(
    formal_computer_training_yes == 1,
    !is.na(computer_training_type),
    !is.na(cnicas_mean)
  )

table(training_type_data$computer_training_type, useNA = "ifany")

anova_training_type <- aov(
  cnicas_mean ~ computer_training_type,
  data = training_type_data
)

summary(anova_training_type)

# Number of nursing-informatics lectures
anova_informatics_lectures <- aov(
  cnicas_mean ~ informatics_lectures,
  data = model_data
)

summary(anova_informatics_lectures)

# Main source of hands-on informatics experience
anova_experience_source <- aov(
  cnicas_mean ~ informatics_experience_source,
  data = model_data
)

summary(anova_experience_source)

# Tukey post-hoc comparisons
TukeyHSD(anova_sex)
TukeyHSD(anova_year_study)
TukeyHSD(anova_training_type)
TukeyHSD(anova_informatics_lectures)
TukeyHSD(anova_experience_source)
# ============================================================
# CHUNK 19 OF 25
# PREPARE MULTIPLE LINEAR REGRESSION DATA
# ============================================================

# Check that model_data exists
exists("model_data")

# Select variables for multiple linear regression
regression_variables <- c(
  "cnicas_mean",
  "academic_device_use_score",
  "computer_confidence_score",
  "internet_access_yes",
  "formal_computer_training_yes",
  "ehr_exposure_level_score",
  "informatics_nurse_tutor_yes",
  "online_learning_yes"
)

# Confirm all required variables are available
regression_variables %in% names(model_data)

# Create regression dataset
regression_data <- model_data %>%
  dplyr::select(dplyr::all_of(regression_variables))

# Inspect missing data before complete-case analysis
names(regression_data)
colSums(is.na(regression_data))
nrow(regression_data)

# Retain complete cases for the regression model
regression_data <- regression_data[
  stats::complete.cases(regression_data),
]

# Confirm final regression sample size and missingness
nrow(regression_data)
nrow(model_data) - nrow(regression_data)
colSums(is.na(regression_data))
# ============================================================
# CHUNK 20 OF 25
# MULTIPLE LINEAR REGRESSION AND DIAGNOSTICS
# ============================================================

# Fit the multiple linear regression model
model_1 <- stats::lm(
  cnicas_mean ~
    academic_device_use_score +
    computer_confidence_score +
    internet_access_yes +
    formal_computer_training_yes +
    ehr_exposure_level_score +
    informatics_nurse_tutor_yes +
    online_learning_yes,
  data = regression_data
)

# View regression results
summary(model_1)

# Check multicollinearity
car::vif(model_1)

# Check homoscedasticity
lmtest::bptest(model_1)

# Standard regression diagnostic plots
par(mfrow = c(2, 2))
plot(model_1)
par(mfrow = c(1, 1))

# Cook's distance: identify influential observations
cooks_d <- cooks.distance(model_1)
cook_threshold <- 4 / nrow(regression_data)

cook_threshold
max(cooks_d)
sum(cooks_d > cook_threshold)
which(cooks_d > cook_threshold)
# ============================================================
# CHUNK 21 OF 25
# EXPORT MULTIPLE LINEAR REGRESSION RESULTS
# ============================================================

# Create a regression-results table
regression_results <- data.frame(
  predictor = rownames(summary(model_1)$coefficients),
  estimate = round(summary(model_1)$coefficients[, "Estimate"], 3),
  standard_error = round(summary(model_1)$coefficients[, "Std. Error"], 3),
  t_value = round(summary(model_1)$coefficients[, "t value"], 3),
  p_value = format.pval(
    summary(model_1)$coefficients[, "Pr(>|t|)"],
    digits = 3,
    eps = 0.001
  ),
  lower_95_CI = round(confint(model_1)[, 1], 3),
  upper_95_CI = round(confint(model_1)[, 2], 3),
  row.names = NULL
)

regression_results

# Export regression results
write.csv(
  regression_results,
  "output/multiple_linear_regression_results.csv",
  row.names = FALSE
)

# Display model and regression data
model_1
regression_data
# ============================================================
# CHUNK 22 OF 25
# TEN-FOLD CROSS-VALIDATION
# ============================================================

# Set a seed so results can be reproduced
set.seed(2026)

# Create 10 cross-validation folds
cv_folds <- vfold_cv(
  regression_data,
  v = 10
)

cv_folds

# Specify the linear regression model
lm_spec <- linear_reg() %>%
  set_engine("lm")

# Build the workflow using the original regression formula
cv_workflow <- workflow() %>%
  add_model(lm_spec) %>%
  add_formula(
    cnicas_mean ~
      academic_device_use_score +
      computer_confidence_score +
      internet_access_yes +
      formal_computer_training_yes +
      ehr_exposure_level_score +
      informatics_nurse_tutor_yes +
      online_learning_yes
  )

# Run ten-fold cross-validation
cv_results <- fit_resamples(
  cv_workflow,
  resamples = cv_folds,
  metrics = metric_set(rmse, rsq, mae),
  control = control_resamples(save_pred = TRUE)
)

# View cross-validation metrics
cv_metrics <- collect_metrics(cv_results)

cv_metrics

# Create a clean validation-results table
validation_results <- cv_metrics %>%
  dplyr::select(.metric, mean, std_err) %>%
  dplyr::mutate(
    mean = round(mean, 3),
    std_err = round(std_err, 3)
  )

validation_results

# Export internal cross-validation results
write.csv(
  validation_results,
  "output/internal_cross_validation_results.csv",
  row.names = FALSE
)

# ============================================================
# CHUNK 23 OF 25
# FIGURE SETUP AND FIGURES 1–4
# ============================================================

# Load required packages for figures
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(corrplot)

# Create output folders if they do not already exist
dir.create("figures", showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)

# Set a consistent plot theme
theme_set(
  theme_minimal(base_size = 13, base_family = "sans") +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 16,
        colour = "#17365D",
        hjust = 0
      ),
      plot.subtitle = element_text(
        size = 11,
        colour = "#4D4D4D",
        hjust = 0
      ),
      axis.title = element_text(
        face = "bold",
        colour = "#17365D"
      ),
      axis.text = element_text(colour = "#333333"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(
        colour = "#D9E2F3",
        linewidth = 0.35
      ),
      legend.title = element_text(
        face = "bold",
        colour = "#17365D"
      ),
      legend.position = "bottom",
      plot.margin = margin(12, 18, 12, 12)
    )
)

# Thesis colour palette
thesis_blue <- "#0072B2"
thesis_orange <- "#E69F00"
thesis_green <- "#009E73"
thesis_red <- "#D55E00"
thesis_purple <- "#CC79A7"
thesis_sky <- "#56B4E9"
thesis_yellow <- "#F0E442"
thesis_dark <- "#17365D"
thesis_grey <- "#6C757D"

confidence_palette <- c(
  "Very low" = thesis_red,
  "Low" = thesis_orange,
  "Moderate" = thesis_sky,
  "High" = thesis_green,
  "Very High" = thesis_blue
)

device_palette <- c(
  "Never" = thesis_red,
  "Rarely" = thesis_orange,
  "Monthly" = thesis_yellow,
  "Weekly" = thesis_sky,
  "Daily" = thesis_blue
)

yes_no_palette <- c(
  "No" = thesis_red,
  "Yes" = thesis_green
)

# ------------------------------------------------------------
# FIGURE 1: Distribution of Nursing Informatics Competency
# ------------------------------------------------------------

mean_cnicas <- mean(model_data$cnicas_mean, na.rm = TRUE)

figure_1_histogram <- ggplot(
  model_data,
  aes(x = cnicas_mean)
) +
  geom_histogram(
    binwidth = 0.25,
    boundary = 0,
    fill = thesis_sky,
    colour = "white",
    linewidth = 0.4,
    alpha = 0.95
  ) +
  geom_density(
    aes(y = after_stat(count * 0.25)),
    colour = thesis_dark,
    linewidth = 1.1
  ) +
  geom_vline(
    xintercept = mean_cnicas,
    colour = thesis_red,
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  annotate(
    "label",
    x = mean_cnicas,
    y = Inf,
    label = paste0("Mean = ", round(mean_cnicas, 2)),
    colour = thesis_red,
    fill = "white",
    fontface = "bold",
    vjust = 1.5
  ) +
  scale_x_continuous(
    limits = c(1, 5),
    breaks = seq(1, 5, 0.5)
  ) +
  labs(
    title = "Distribution of Nursing Informatics Competency",
    subtitle = "Mean C-NICAS score among nursing students",
    x = "Mean C-NICAS competency score",
    y = "Number of students"
  )

figure_1_histogram

ggsave(
  "figures/Figure1_CNICAS_distribution.png",
  plot = figure_1_histogram,
  width = 9,
  height = 6,
  dpi = 300,
  bg = "white"
)

# ------------------------------------------------------------
# FIGURE 2: Computer Confidence
# ------------------------------------------------------------

confidence_plot_data <- model_data %>%
  filter(!is.na(computer_confidence)) %>%
  mutate(
    computer_confidence = factor(
      computer_confidence,
      levels = c("Very low", "Low", "Moderate", "High", "Very High")
    )
  ) %>%
  count(computer_confidence) %>%
  mutate(
    percentage = n / sum(n) * 100
  )

figure_2_confidence <- ggplot(
  confidence_plot_data,
  aes(
    x = computer_confidence,
    y = percentage,
    fill = computer_confidence
  )
) +
  geom_col(
    width = 0.7,
    colour = "white",
    linewidth = 0.5
  ) +
  geom_text(
    aes(label = paste0(n, " (", round(percentage, 1), "%)")),
    vjust = -0.4,
    size = 3.7,
    colour = thesis_dark,
    fontface = "bold"
  ) +
  scale_fill_manual(values = confidence_palette) +
  scale_y_continuous(
    limits = c(0, max(confidence_plot_data$percentage) + 15),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Computer Confidence",
    subtitle = "Distribution of computer-confidence levels",
    x = "Computer-confidence level",
    y = "Students",
    fill = "Confidence"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(face = "bold")
  )

figure_2_confidence

ggsave(
  "figures/Figure2_computer_confidence.png",
  plot = figure_2_confidence,
  width = 9,
  height = 6,
  dpi = 300,
  bg = "white"
)

# ------------------------------------------------------------
# FIGURE 3: Frequency of Academic Device Use
# ------------------------------------------------------------

device_plot_data <- model_data %>%
  filter(!is.na(academic_device_use)) %>%
  mutate(
    academic_device_use = factor(
      academic_device_use,
      levels = c("Never", "Rarely", "Monthly", "Weekly", "Daily")
    )
  ) %>%
  count(academic_device_use) %>%
  mutate(
    percentage = n / sum(n) * 100
  )

figure_3_device_use <- ggplot(
  device_plot_data,
  aes(
    x = academic_device_use,
    y = percentage,
    fill = academic_device_use
  )
) +
  geom_col(
    width = 0.7,
    colour = "white",
    linewidth = 0.5
  ) +
  geom_text(
    aes(label = paste0(n, " (", round(percentage, 1), "%)")),
    vjust = -0.4,
    size = 3.7,
    colour = thesis_dark,
    fontface = "bold"
  ) +
  scale_fill_manual(values = device_palette) +
  scale_y_continuous(
    limits = c(0, max(device_plot_data$percentage) + 15),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Frequency of Academic Device Use",
    subtitle = "Use of computers or smartphones for academic activities",
    x = "Academic device-use frequency",
    y = "Students",
    fill = "Device use"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(face = "bold")
  )

figure_3_device_use

ggsave(
  "figures/Figure3_academic_device_use.png",
  plot = figure_3_device_use,
  width = 9,
  height = 6,
  dpi = 300,
  bg = "white"
)

# ------------------------------------------------------------
# FIGURE 4: Digital Access and Informatics Exposure
# ------------------------------------------------------------

technology_profile <- data.frame(
  factor = c(
    "Smartphone ownership",
    "Laptop or desktop ownership",
    "Regular internet access",
    "Formal computer training",
    "EHR exposure",
    "Computer laboratory available",
    "Institutional internet available",
    "Informatics taught by nurse tutor",
    "Prior online-learning exposure"
  ),
  percentage = c(
    mean(model_data$smartphone_yes == 1, na.rm = TRUE) * 100,
    mean(model_data$computer_yes == 1, na.rm = TRUE) * 100,
    mean(model_data$internet_access_yes == 1, na.rm = TRUE) * 100,
    mean(model_data$formal_computer_training_yes == 1, na.rm = TRUE) * 100,
    mean(model_data$ehr_exposure_yes == 1, na.rm = TRUE) * 100,
    mean(model_data$computer_lab_yes == 1, na.rm = TRUE) * 100,
    mean(model_data$institution_internet_yes == 1, na.rm = TRUE) * 100,
    mean(model_data$informatics_nurse_tutor_yes == 1, na.rm = TRUE) * 100,
    mean(model_data$online_learning_yes == 1, na.rm = TRUE) * 100
  )
) %>%
  arrange(percentage) %>%
  mutate(
    category = case_when(
      percentage < 50 ~ "Lower access",
      percentage < 75 ~ "Moderate access",
      TRUE ~ "Higher access"
    )
  )

figure_4_technology <- ggplot(
  technology_profile,
  aes(
    x = reorder(factor, percentage),
    y = percentage,
    fill = category
  )
) +
  geom_col(
    width = 0.7,
    colour = "white",
    linewidth = 0.4
  ) +
  geom_text(
    aes(label = paste0(round(percentage, 1), "%")),
    hjust = -0.15,
    fontface = "bold",
    size = 3.7,
    colour = thesis_dark
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Lower access" = thesis_orange,
      "Moderate access" = thesis_sky,
      "Higher access" = thesis_blue
    )
  ) +
  scale_y_continuous(
    limits = c(0, 110),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Digital Access and Informatics Exposure",
    subtitle = "Students reporting technology access or learning exposure",
    x = NULL,
    y = "Students reporting yes",
    fill = "Access level"
  )

figure_4_technology

ggsave(
  "figures/Figure4_digital_access_exposure.png",
  plot = figure_4_technology,
  width = 11,
  height = 7,
  dpi = 300,
  bg = "white"
)
# ============================================================
# CHUNK 24 OF 25
# FIGURES 5–8: C-NICAS COMPETENCY BY KEY FACTORS
# ============================================================

# ------------------------------------------------------------
# FIGURE 5: Informatics Competency by Computer Confidence
# ------------------------------------------------------------

confidence_cnicas_data <- model_data %>%
  filter(
    !is.na(cnicas_mean),
    !is.na(computer_confidence)
  ) %>%
  mutate(
    computer_confidence = factor(
      computer_confidence,
      levels = c("Very low", "Low", "Moderate", "High", "Very High")
    )
  )

figure_5_cnicas_confidence <- ggplot(
  confidence_cnicas_data,
  aes(
    x = computer_confidence,
    y = cnicas_mean,
    fill = computer_confidence
  )
) +
  geom_boxplot(
    alpha = 0.85,
    colour = thesis_dark,
    linewidth = 0.55,
    outlier.alpha = 0.25
  ) +
  geom_jitter(
    width = 0.12,
    alpha = 0.12,
    colour = thesis_dark,
    size = 1.1
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 23,
    size = 4,
    fill = "white",
    colour = "black"
  ) +
  scale_fill_manual(values = confidence_palette) +
  scale_y_continuous(
    limits = c(1, 5),
    breaks = seq(1, 5, 0.5)
  ) +
  labs(
    title = "Informatics Competency by Computer Confidence",
    subtitle = "White diamonds represent group mean C-NICAS scores",
    x = "Computer-confidence level",
    y = "Mean C-NICAS competency score",
    fill = "Confidence"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(face = "bold")
  )

figure_5_cnicas_confidence

ggsave(
  "figures/Figure5_CNICAS_by_computer_confidence.png",
  plot = figure_5_cnicas_confidence,
  width = 10,
  height = 6.5,
  dpi = 300,
  bg = "white"
)

# ------------------------------------------------------------
# FIGURE 6: Informatics Competency by Academic Device Use
# ------------------------------------------------------------

device_cnicas_data <- model_data %>%
  filter(
    !is.na(cnicas_mean),
    !is.na(academic_device_use)
  ) %>%
  mutate(
    academic_device_use = factor(
      academic_device_use,
      levels = c("Never", "Rarely", "Monthly", "Weekly", "Daily")
    )
  )

figure_6_cnicas_device <- ggplot(
  device_cnicas_data,
  aes(
    x = academic_device_use,
    y = cnicas_mean,
    fill = academic_device_use
  )
) +
  geom_boxplot(
    alpha = 0.85,
    colour = thesis_dark,
    linewidth = 0.55,
    outlier.alpha = 0.25
  ) +
  geom_jitter(
    width = 0.12,
    alpha = 0.12,
    colour = thesis_dark,
    size = 1.1
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 23,
    size = 4,
    fill = "white",
    colour = "black"
  ) +
  scale_fill_manual(values = device_palette) +
  scale_y_continuous(
    limits = c(1, 5),
    breaks = seq(1, 5, 0.5)
  ) +
  labs(
    title = "Informatics Competency by Academic Device Use",
    subtitle = "White diamonds represent group mean C-NICAS scores",
    x = "Academic device-use frequency",
    y = "Mean C-NICAS competency score",
    fill = "Device use"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(face = "bold")
  )

figure_6_cnicas_device

ggsave(
  "figures/Figure6_CNICAS_by_academic_device_use.png",
  plot = figure_6_cnicas_device,
  width = 10,
  height = 6.5,
  dpi = 300,
  bg = "white"
)

# ------------------------------------------------------------
# FIGURE 7: Informatics Competency by Formal Computer Training
# ------------------------------------------------------------

training_cnicas_data <- model_data %>%
  filter(
    !is.na(cnicas_mean),
    !is.na(formal_computer_training_yes)
  ) %>%
  mutate(
    formal_training = factor(
      formal_computer_training_yes,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  )

figure_7_cnicas_training <- ggplot(
  training_cnicas_data,
  aes(
    x = formal_training,
    y = cnicas_mean,
    fill = formal_training
  )
) +
  geom_boxplot(
    alpha = 0.85,
    colour = thesis_dark,
    linewidth = 0.6,
    width = 0.6,
    outlier.alpha = 0.25
  ) +
  geom_jitter(
    width = 0.11,
    alpha = 0.13,
    colour = thesis_dark,
    size = 1.1
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 23,
    size = 4,
    fill = "white",
    colour = "black"
  ) +
  scale_fill_manual(values = yes_no_palette) +
  scale_y_continuous(
    limits = c(1, 5),
    breaks = seq(1, 5, 0.5)
  ) +
  labs(
    title = "Informatics Competency by Formal Computer Training",
    subtitle = "White diamonds represent group mean C-NICAS scores",
    x = "Formal computer training before nursing school",
    y = "Mean C-NICAS competency score",
    fill = "Training"
  ) +
  theme(
    legend.position = "none"
  )

figure_7_cnicas_training

ggsave(
  "figures/Figure7_CNICAS_by_formal_training.png",
  plot = figure_7_cnicas_training,
  width = 8,
  height = 6,
  dpi = 300,
  bg = "white"
)

# ------------------------------------------------------------
# FIGURE 8: Informatics Competency by EHR Exposure
# ------------------------------------------------------------

ehr_cnicas_data <- model_data %>%
  filter(
    !is.na(cnicas_mean),
    !is.na(ehr_exposure_yes)
  ) %>%
  mutate(
    ehr_exposure_group = factor(
      ehr_exposure_yes,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  )

figure_8_cnicas_ehr <- ggplot(
  ehr_cnicas_data,
  aes(
    x = ehr_exposure_group,
    y = cnicas_mean,
    fill = ehr_exposure_group
  )
) +
  geom_boxplot(
    alpha = 0.85,
    colour = thesis_dark,
    linewidth = 0.6,
    width = 0.6,
    outlier.alpha = 0.25
  ) +
  geom_jitter(
    width = 0.11,
    alpha = 0.13,
    colour = thesis_dark,
    size = 1.1
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 23,
    size = 4,
    fill = "white",
    colour = "black"
  ) +
  scale_fill_manual(values = yes_no_palette) +
  scale_y_continuous(
    limits = c(1, 5),
    breaks = seq(1, 5, 0.5)
  ) +
  labs(
    title = "Informatics Competency by EHR Exposure",
    subtitle = "White diamonds represent group mean C-NICAS scores",
    x = "Exposure to electronic health records",
    y = "Mean C-NICAS competency score",
    fill = "EHR exposure"
  ) +
  theme(
    legend.position = "none"
  )

figure_8_cnicas_ehr

ggsave(
  "figures/Figure8_CNICAS_by_EHR_exposure.png",
  plot = figure_8_cnicas_ehr,
  width = 8,
  height = 6,
  dpi = 300,
  bg = "white"
)

# ============================================================
# CHUNK 25 OF 25 — PART 1
# FIGURE 9: MEAN SCORES ACROSS C-NICAS ITEMS
# ============================================================

# Recreate the C-NICAS item list if it is not already available
if (!exists("cnicas_items")) {
  cnicas_items <- names(model_data)[
    grepl("^cnicas", names(model_data))
  ]
}

# Check that C-NICAS items were found
length(cnicas_items)
cnicas_items

# Calculate mean score for each C-NICAS item
cnicas_item_means <- model_data %>%
  summarise(
    across(
      all_of(cnicas_items),
      ~ mean(.x, na.rm = TRUE)
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "item",
    values_to = "mean_score"
  ) %>%
  mutate(
    item_label = case_when(
      item == "cnicas_use_ict_devices" ~ "Use ICT devices",
      item == "cnicas_use_ict_tools" ~ "Use ICT tools",
      item == "cnicas_search_online_information" ~ "Search online information",
      item == "cnicas_evaluate_online_information" ~ "Evaluate online information",
      item == "cnicas_use_patient_data_decisions" ~ "Use patient data in decisions",
      item == "cnicas_record_electronic_records" ~ "Record electronic records",
      item == "cnicas_help_patients_find_information" ~ "Help patients find information",
      item == "cnicas_understand_information_standards" ~ "Understand information standards",
      item == "cnicas_understand_standard_nursing_terms" ~ "Understand nursing terminology",
      item == "cnicas_follow_information_ethics" ~ "Follow information ethics",
      item == "cnicas_support_technology_healthcare" ~ "Support technology in healthcare",
      item == "cnicas_advocate_health_technology" ~ "Advocate for health technology",
      item == "cnicas_professional_judgment_technology" ~ "Use professional judgement",
      item == "cnicas_nurse_role_ict_systems" ~ "Understand nurse role in ICT systems",
      item == "cnicas_electronic_patient_documentation" ~ "Electronic patient documentation",
      item == "cnicas_identify_ehr_errors" ~ "Identify EHR errors",
      item == "cnicas_recognise_data_quality_gaps" ~ "Recognise data-quality gaps",
      item == "cnicas_identify_system_device_problems" ~ "Identify system/device problems",
      item == "cnicas_report_ict_problems" ~ "Report ICT problems",
      item == "cnicas_patient_safety_system_downtime" ~ "Maintain safety during downtime",
      item == "cnicas_use_ict_patient_care" ~ "Use ICT in patient care",
      item == "cnicas_use_ict_clinical_decisions" ~ "Use ICT in clinical decisions",
      item == "cnicas_ict_nurse_patient_relationship" ~ "ICT and nurse-patient relationship",
      item == "cnicas_identify_clinical_system_components" ~ "Identify clinical system components",
      item == "cnicas_distinguish_health_record_systems" ~ "Distinguish health-record systems",
      item == "cnicas_informatics_improves_quality" ~ "Informatics improves quality",
      TRUE ~ item
    ),
    competency_level = case_when(
      mean_score < 3 ~ "Below midpoint",
      mean_score < 4 ~ "Moderate",
      TRUE ~ "High"
    )
  ) %>%
  arrange(mean_score)

cnicas_item_means

# Create Figure 9
figure_9_item_means <- ggplot(
  cnicas_item_means,
  aes(
    x = reorder(item_label, mean_score),
    y = mean_score,
    fill = competency_level
  )
) +
  geom_col(
    colour = "white",
    linewidth = 0.35
  ) +
  geom_hline(
    yintercept = 3,
    linetype = "dashed",
    colour = thesis_red,
    linewidth = 0.8
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Below midpoint" = thesis_red,
      "Moderate" = thesis_sky,
      "High" = thesis_green
    )
  ) +
  scale_y_continuous(
    limits = c(0, 5),
    breaks = seq(0, 5, 1)
  ) +
  labs(
    title = "Mean Scores Across Individual C-NICAS Items",
    subtitle = "Dashed line shows the response-scale midpoint of 3",
    x = NULL,
    y = "Mean item score",
    fill = "Competency level"
  )

figure_9_item_means

ggsave(
  "figures/Figure9_CNICAS_item_means.png",
  plot = figure_9_item_means,
  width = 12,
  height = 11,
  dpi = 300,
  bg = "white"
)
# ============================================================
# FINAL FIGURES
# FIGURES 10–12: REGRESSION AND CROSS-VALIDATION
# ============================================================

# ------------------------------------------------------------
# FIGURE 10: Adjusted Regression Predictors
# ------------------------------------------------------------

regression_plot_data <- broom::tidy(
  model_1,
  conf.int = TRUE
) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    predictor = case_when(
      term == "academic_device_use_score" ~
        "Academic device-use frequency",
      
      term == "computer_confidence_score" ~
        "Computer confidence",
      
      term == "internet_access_yes" ~
        "Regular internet access",
      
      term == "formal_computer_training_yes" ~
        "Formal computer training",
      
      term == "ehr_exposure_level_score" ~
        "EHR exposure level",
      
      term == "informatics_nurse_tutor_yes" ~
        "Informatics taught by nurse tutor",
      
      term == "online_learning_yes" ~
        "Prior online-learning exposure",
      
      TRUE ~ term
    ),
    significance = if_else(
      p.value < 0.05,
      "Statistically significant",
      "Not statistically significant"
    )
  )

figure_10_forest <- ggplot(
  regression_plot_data,
  aes(
    x = estimate,
    y = reorder(predictor, estimate),
    colour = significance
  )
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = thesis_grey,
    linewidth = 0.8
  ) +
  geom_errorbarh(
    aes(
      xmin = conf.low,
      xmax = conf.high
    ),
    height = 0.18,
    linewidth = 0.9
  ) +
  geom_point(size = 4) +
  scale_colour_manual(
    values = c(
      "Statistically significant" = thesis_green,
      "Not statistically significant" = thesis_grey
    )
  ) +
  labs(
    title = "Adjusted Predictors of Informatics Competency",
    subtitle = "Multiple linear regression coefficients with 95% confidence intervals",
    x = "Adjusted regression coefficient",
    y = NULL,
    colour = "Result"
  ) +
  theme(
    legend.position = "bottom"
  )

figure_10_forest

ggsave(
  "figures/Figure10_adjusted_regression_predictors.png",
  plot = figure_10_forest,
  width = 11,
  height = 7,
  dpi = 300,
  bg = "white"
)

# ------------------------------------------------------------
# FIGURE 11: Observed Versus Predicted C-NICAS Scores
# ------------------------------------------------------------

regression_data$predicted_cnicas <- predict(
  model_1,
  newdata = regression_data
)

model_r_squared <- summary(model_1)$r.squared

figure_11_observed_predicted <- ggplot(
  regression_data,
  aes(
    x = predicted_cnicas,
    y = cnicas_mean
  )
) +
  geom_point(
    colour = thesis_sky,
    alpha = 0.45,
    size = 2
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    colour = thesis_red,
    fill = "#FDD0A2",
    linewidth = 1
  ) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    colour = thesis_dark,
    linewidth = 0.8
  ) +
  annotate(
    "label",
    x = 1.6,
    y = 4.8,
    label = paste0(
      "Model R² = ",
      round(model_r_squared, 3)
    ),
    fill = "white",
    colour = thesis_dark,
    fontface = "bold"
  ) +
  scale_x_continuous(
    limits = c(1, 5),
    breaks = seq(1, 5, 0.5)
  ) +
  scale_y_continuous(
    limits = c(1, 5),
    breaks = seq(1, 5, 0.5)
  ) +
  labs(
    title = "Observed and Predicted C-NICAS Scores",
    subtitle = "Dashed diagonal line represents perfect prediction",
    x = "Predicted mean C-NICAS score",
    y = "Observed mean C-NICAS score"
  )

figure_11_observed_predicted

ggsave(
  "figures/Figure11_observed_vs_predicted.png",
  plot = figure_11_observed_predicted,
  width = 9,
  height = 7,
  dpi = 300,
  bg = "white"
)

# ------------------------------------------------------------
# FIGURE 12: Ten-Fold Cross-Validation Performance
# ------------------------------------------------------------

# Works whether your column is called std_err or stderr
if ("std_err" %in% names(validation_results)) {
  validation_results <- validation_results %>%
    rename(stderr = std_err)
}

validation_plot_data <- validation_results %>%
  mutate(
    metric_label = case_when(
      .metric == "rsq" ~ "Cross-validated R-squared",
      .metric == "rmse" ~ "Root mean squared error",
      .metric == "mae" ~ "Mean absolute error",
      TRUE ~ .metric
    ),
    metric_label = factor(
      metric_label,
      levels = c(
        "Cross-validated R-squared",
        "Mean absolute error",
        "Root mean squared error"
      )
    ),
    colour = case_when(
      .metric == "rsq" ~ thesis_green,
      .metric == "mae" ~ thesis_orange,
      .metric == "rmse" ~ thesis_blue,
      TRUE ~ thesis_grey
    )
  )

figure_12_validation <- ggplot(
  validation_plot_data,
  aes(
    x = metric_label,
    y = mean,
    fill = colour
  )
) +
  geom_col(
    width = 0.62,
    colour = "white",
    linewidth = 0.5,
    show.legend = FALSE
  ) +
  geom_errorbar(
    aes(
      ymin = pmax(0, mean - stderr),
      ymax = mean + stderr
    ),
    width = 0.12,
    colour = thesis_dark
  ) +
  geom_text(
    aes(label = round(mean, 3)),
    vjust = -0.45,
    fontface = "bold",
    size = 4,
    colour = thesis_dark
  ) +
  scale_fill_identity() +
  scale_y_continuous(
    limits = c(
      0,
      max(validation_plot_data$mean + validation_plot_data$stderr) + 0.15
    )
  ) +
  labs(
    title = "Ten-Fold Cross-Validation Performance",
    subtitle = "Error bars represent standard errors across validation folds",
    x = NULL,
    y = "Performance value"
  ) +
  theme(
    axis.text.x = element_text(
      face = "bold",
      hjust = 0.5
    )
  )

figure_12_validation

ggsave(
  "figures/Figure12_cross_validation_performance.png",
  plot = figure_12_validation,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)
# ============================================================
# APPENDIX FIGURES
# C-NICAS CORRELATION HEATMAP AND REGRESSION DIAGNOSTICS
# ============================================================

# Confirm that the C-NICAS item list exists
if (!exists("cnicas_items")) {
  cnicas_items <- names(model_data)[
    grepl("^cnicas", names(model_data))
  ]
}

# Keep only numeric C-NICAS item columns
cnicas_numeric_data <- model_data %>%
  dplyr::select(dplyr::all_of(cnicas_items)) %>%
  dplyr::select(where(is.numeric))

# Check number of retained C-NICAS items
ncol(cnicas_numeric_data)

# ------------------------------------------------------------
# APPENDIX FIGURE 1: C-NICAS ITEM CORRELATION HEATMAP
# ------------------------------------------------------------

cnicas_correlation_matrix <- cor(
  cnicas_numeric_data,
  use = "pairwise.complete.obs",
  method = "spearman"
)

png(
  filename = "figures/AppendixFigure_CNICAS_correlation_heatmap.png",
  width = 2800,
  height = 2600,
  res = 300
)

corrplot::corrplot(
  cnicas_correlation_matrix,
  method = "color",
  type = "lower",
  order = "hclust",
  tl.col = thesis_dark,
  tl.cex = 0.55,
  tl.srt = 45,
  diag = FALSE,
  col = colorRampPalette(
    c(thesis_blue, "white", thesis_red)
  )(200),
  title = "Spearman Correlations Among C-NICAS Items",
  col.main = thesis_dark,
  font.main = 2,
  cex.main = 1.4
)

dev.off()

# ------------------------------------------------------------
# APPENDIX FIGURE 2: REGRESSION DIAGNOSTIC PLOTS
# ------------------------------------------------------------

png(
  filename = "figures/AppendixFigure_regression_diagnostics.png",
  width = 2800,
  height = 2200,
  res = 300
)

par(
  mfrow = c(2, 2),
  mar = c(4, 4, 3, 1),
  col.axis = thesis_dark,
  col.lab = thesis_dark,
  col.main = thesis_dark
)

plot(model_1)

par(mfrow = c(1, 1))

dev.off()

# Confirm all saved figure files
list.files(
  "figures",
  pattern = "\\.png$",
  full.names = TRUE
)
library(rsconnect)














# ============================================================
# COMPLETE FIGURE CODE FOR CHAPTER 4.1
# Run AFTER Chunks 1–25 and Final Figures
# ============================================================

# ---- Packages ----
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(patchwork)
library(scales)
library(corrplot)
library(psych)

# Ensure output folder exists
dir.create("figures", showWarnings = FALSE)

# ============================================================
# FIGURE 4.1: SAMPLE DISTRIBUTION BY SEX AND YEAR OF STUDY
# Two separate waffle plots (each square = 1% of N = 503)
# ============================================================

# ---- Counts ----
sex_total  <- sum(!is.na(model_data$sex))
sex_female <- sum(model_data$sex == "Female", na.rm = TRUE)
sex_male   <- sum(model_data$sex == "Male",   na.rm = TRUE)

year_total <- sum(!is.na(model_data$year_study))
year_1 <- sum(model_data$year_study == "1st year", na.rm = TRUE)
year_2 <- sum(model_data$year_study == "2nd year", na.rm = TRUE)
year_3 <- sum(model_data$year_study == "3rd year", na.rm = TRUE)

# ---- Waffle 1: Sex ----
sex_pct_f <- round(sex_female / sex_total * 100)   # e.g., 77
sex_pct_m <- 100 - sex_pct_f                       # e.g., 23

sex_grid <- expand.grid(x = 1:10, y = 1:10)
sex_grid$group <- c(
  rep("Female", sex_pct_f),
  rep("Male",   sex_pct_m)
)
sex_grid$group <- factor(sex_grid$group, levels = c("Female", "Male"))

p_sex <- ggplot(sex_grid, aes(x = x, y = y, fill = group)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  scale_fill_manual(values = c("Female" = "#17365D", "Male" = "#E69F00")) +
  coord_equal() +
  labs(
    title    = "Sample by Sex",
    subtitle = paste0("Each square = 1% of students (N = ", sex_total, ")"),
    fill     = NULL
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", colour = "#17365D",
                                 size = 14, hjust = 0),
    plot.subtitle = element_text(colour = "#4D4D4D", size = 10, hjust = 0),
    legend.position = "bottom",
    legend.text     = element_text(size = 10),
    legend.title    = element_blank(),
    plot.margin     = margin(10, 10, 10, 10)
  )

# ---- Waffle 2: Year of study (1st, 2nd, 3rd) ----
year_pct_1 <- round(year_1 / year_total * 100)
year_pct_2 <- round(year_2 / year_total * 100)
year_pct_3 <- 100 - year_pct_1 - year_pct_2

year_grid <- expand.grid(x = 1:10, y = 1:10)
year_grid$group <- c(
  rep("1st year", year_pct_1),
  rep("2nd year", year_pct_2),
  rep("3rd year", year_pct_3)
)
year_grid$group <- factor(
  year_grid$group,
  levels = c("1st year", "2nd year", "3rd year")
)

p_year <- ggplot(year_grid, aes(x = x, y = y, fill = group)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  scale_fill_manual(
    values = c(
      "1st year" = "#56B4E9",
      "2nd year" = "#17365D",
      "3rd year" = "#009E73"
    )
  ) +
  coord_equal() +
  labs(
    title    = "Sample by Year of Study",
    subtitle = paste0("Each square = 1% of students (N = ", year_total, ")"),
    fill     = NULL
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", colour = "#17365D",
                                 size = 14, hjust = 0),
    plot.subtitle = element_text(colour = "#4D4D4D", size = 10, hjust = 0),
    legend.position = "bottom",
    legend.text     = element_text(size = 10),
    legend.title    = element_blank(),
    plot.margin     = margin(10, 10, 10, 10)
  )

# ---- Combine side by side ----
figure_4_1 <- p_sex | p_year

figure_4_1

ggsave(
  "figures/Figure4.1_sample_distribution.png",
  plot = figure_4_1,
  width = 12, height = 6, dpi = 300, bg = "white"
)

# ============================================================
# FIGURE 4.2: DISTRIBUTION OF COMPOSITE C-NICAS SCORE
# (Copy from figure_1_histogram already in environment)
# ============================================================

ggsave(
  "figures/Figure4.2_CNICAS_distribution.png",
  plot = figure_1_histogram,
  width = 9, height = 6, dpi = 300, bg = "white"
)

# ============================================================
# FIGURE 4.3: MEAN SCORES ACROSS 26 C-NICAS ITEMS
# (Copy from figure_9_item_means already in environment)
# ============================================================

ggsave(
  "figures/Figure4.3_CNICAS_item_means.png",
  plot = figure_9_item_means,
  width = 12, height = 11, dpi = 300, bg = "white"
)

# ============================================================
# FIGURE 4.4: ITEM-TOTAL CORRELATIONS (TORNADO CHART)
# ============================================================

# Compute Cronbach's alpha (already done in Chunk 12, but re-run if needed)
cnicas_reliability <- psych::alpha(
  model_data[cnicas_items],
  check.keys = FALSE
)

item_total_df <- data.frame(
  item      = cnicas_items,
  r_drop    = cnicas_reliability$item.stats$r.drop,
  alpha_drop = cnicas_reliability$alpha.drop$raw_alpha
)

# Readable labels
item_labels <- c(
  cnicas_use_ict_devices                    = "Use ICT devices",
  cnicas_use_ict_tools                      = "Use ICT tools",
  cnicas_search_online_information          = "Search online information",
  cnicas_evaluate_online_information        = "Evaluate online information",
  cnicas_use_patient_data_decisions         = "Use patient data in decisions",
  cnicas_record_electronic_records          = "Record electronic records",
  cnicas_help_patients_find_information     = "Help patients find information",
  cnicas_understand_information_standards   = "Understand information standards",
  cnicas_understand_standard_nursing_terms  = "Understand standard nursing terms",
  cnicas_follow_information_ethics          = "Follow information ethics",
  cnicas_support_technology_healthcare      = "Support technology in healthcare",
  cnicas_advocate_health_technology         = "Advocate for health technology",
  cnicas_professional_judgment_technology   = "Prioritise professional judgment",
  cnicas_nurse_role_ict_systems             = "Nurse's role in ICT systems",
  cnicas_electronic_patient_documentation   = "Electronic patient documentation",
  cnicas_identify_ehr_errors                = "Identify EHR errors",
  cnicas_recognise_data_quality_gaps        = "Recognise data-quality gaps",
  cnicas_identify_system_device_problems    = "Identify system/device problems",
  cnicas_report_ict_problems                = "Report ICT problems",
  cnicas_patient_safety_system_downtime     = "Patient safety during downtime",
  cnicas_use_ict_patient_care               = "Use ICT in patient care",
  cnicas_use_ict_clinical_decisions         = "Use ICT in clinical decisions",
  cnicas_ict_nurse_patient_relationship     = "ICT and nurse-patient relationship",
  cnicas_identify_clinical_system_components = "Identify clinical system components",
  cnicas_distinguish_health_record_systems   = "Distinguish health-record systems",
  cnicas_informatics_improves_quality        = "Informatics improves quality"
)

item_total_df$item_label <- item_labels[item_total_df$item]
mean_r <- mean(item_total_df$r_drop, na.rm = TRUE)

item_total_df <- item_total_df %>%
  mutate(
    deviation = r_drop - mean_r,
    direction = ifelse(deviation >= 0, "Above scale mean", "Below scale mean")
  )

figure_4_4 <- ggplot(
  item_total_df,
  aes(x = reorder(item_label, deviation), y = deviation, fill = direction)
) +
  geom_col(width = 0.7, colour = "white", linewidth = 0.3) +
  geom_hline(yintercept = 0, colour = "#17365D", linewidth = 0.6) +
  geom_text(
    aes(
      label = round(r_drop, 3),
      hjust = ifelse(deviation >= 0, -0.15, 1.15)
    ),
    size = 3.1, colour = "#17365D", fontface = "bold"
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Above scale mean" = "#009E73",
      "Below scale mean" = "#E69F00"
    )
  ) +
  scale_y_continuous(
    limits = c(min(item_total_df$deviation) - 0.03,
               max(item_total_df$deviation) + 0.03)
  ) +
  labs(
    title = "Item-Total Correlations Relative to the Scale Mean",
    subtitle = paste0(
      "All 26 items exceed the 0.30 minimum acceptability threshold (Field, 2018)\n",
      "Mean item-total correlation r = ", round(mean_r, 3)
    ),
    x = NULL,
    y = "Deviation from mean item-total correlation",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 15, colour = "#17365D"),
    plot.subtitle = element_text(size = 10, colour = "#4D4D4D"),
    axis.title.x  = element_text(face = "bold", colour = "#17365D"),
    axis.text.y   = element_text(size = 9.5, colour = "#333333"),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom"
  )

figure_4_4

ggsave(
  "figures/Figure4.4_item_total_correlations.png",
  plot = figure_4_4,
  width = 12, height = 10, dpi = 300, bg = "white"
)

# ============================================================
# FIGURE 4.5: COMPUTER CONFIDENCE DISTRIBUTION
# ============================================================

ggsave(
  "figures/Figure4.5_computer_confidence.png",
  plot = figure_2_confidence,
  width = 9, height = 6, dpi = 300, bg = "white"
)

# ============================================================
# FIGURE 4.6: ACADEMIC DEVICE USE FREQUENCY
# ============================================================

ggsave(
  "figures/Figure4.6_academic_device_use.png",
  plot = figure_3_device_use,
  width = 9, height = 6, dpi = 300, bg = "white"
)

# ============================================================
# FIGURE 4.7: DIGITAL ACCESS AND INFORMATICS EXPOSURE
# ============================================================

ggsave(
  "figures/Figure4.7_digital_access_exposure.png",
  plot = figure_4_technology,
  width = 11, height = 7, dpi = 300, bg = "white"
)

# ============================================================
# FIGURE 4.8: 100% STACKED BAR OF ALL 12 PREDICTORS
# ============================================================

predictor_labels_full <- c(
  smartphone_yes                     = "Smartphone ownership",
  computer_yes                       = "Laptop/desktop computer ownership",
  internet_access_yes                = "Regular internet access",
  formal_computer_training_yes       = "Formal computer training",
  ehr_exposure_yes                   = "EHR exposure",
  computer_lab_yes                   = "Computer laboratory available",
  institution_internet_yes           = "Institutional internet available",
  informatics_nurse_tutor_yes        = "Informatics taught by a nurse tutor",
  informatics_other_professional_yes = "Taught informatics by another professional",
  tutor_confidence_yes               = "Tutor confidence teaching informatics",
  online_learning_yes                = "Prior online-learning exposure",
  digital_communication_yes          = "Digital academic communication"
)

predictor_order <- c(
  "smartphone_yes",
  "digital_communication_yes",
  "internet_access_yes",
  "institution_internet_yes",
  "online_learning_yes",
  "informatics_other_professional_yes",
  "informatics_nurse_tutor_yes",
  "tutor_confidence_yes",
  "computer_lab_yes",
  "formal_computer_training_yes",
  "computer_yes",
  "ehr_exposure_yes"
)

stacked_data <- do.call(
  rbind,
  lapply(predictor_order, function(v) {
    tab   <- table(model_data[[v]], useNA = "no")
    total <- sum(tab)
    data.frame(
      predictor     = predictor_labels_full[v],
      predictor_key = v,
      response      = c("No", "Yes"),
      n             = as.numeric(tab[c("0", "1")]),
      percent       = as.numeric(tab[c("0", "1")]) / total * 100
    )
  })
)

stacked_data$predictor <- factor(
  stacked_data$predictor,
  levels = rev(predictor_labels_full[predictor_order])
)

figure_4_8 <- ggplot(
  stacked_data,
  aes(x = predictor, y = percent, fill = response)
) +
  geom_col(width = 0.72, colour = "white", linewidth = 0.4) +
  geom_text(
    aes(label = ifelse(percent >= 4, paste0(round(percent, 1), "%"), "")),
    position = position_stack(vjust = 0.5),
    colour = "white", fontface = "bold", size = 3.1
  ) +
  coord_flip() +
  scale_fill_manual(values = c("No" = "#B0B7C3", "Yes" = "#009E73")) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 25),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title    = "Learner, Educational and Institutional Predictor Variables",
    subtitle = "Proportion of students responding Yes / No, sorted by prevalence of Yes",
    x = NULL,
    y = "Percentage of students (N = 503)",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14, colour = "#17365D"),
    plot.subtitle = element_text(size = 10, colour = "#4D4D4D"),
    axis.text.y   = element_text(size = 10, colour = "#333333"),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom"
  )

figure_4_8

ggsave(
  "figures/Figure4.8_predictor_stacked_bar.png",
  plot = figure_4_8,
  width = 11, height = 8, dpi = 300, bg = "white"
)

# ============================================================
# FIGURE 4.9: C-NICAS BY COMPUTER CONFIDENCE
# ============================================================

ggsave(
  "figures/Figure4.9_CNICAS_by_computer_confidence.png",
  plot = figure_5_cnicas_confidence,
  width = 10, height = 6.5, dpi = 300, bg = "white"
)

# ============================================================
# FIGURE 4.10: C-NICAS BY ACADEMIC DEVICE USE
# ============================================================

ggsave(
  "figures/Figure4.10_CNICAS_by_academic_device_use.png",
  plot = figure_6_cnicas_device,
  width = 10, height = 6.5, dpi = 300, bg = "white"
)

# ============================================================
# FIGURE 4.11: C-NICAS BY FORMAL COMPUTER TRAINING
# ============================================================

ggsave(
  "figures/Figure4.11_CNICAS_by_formal_training.png",
  plot = figure_7_cnicas_training,
  width = 8, height = 6, dpi = 300, bg = "white"
)

# ============================================================
# FIGURE 4.12: C-NICAS BY EHR EXPOSURE
# ============================================================

ggsave(
  "figures/Figure4.12_CNICAS_by_EHR_exposure.png",
  plot = figure_8_cnicas_ehr,
  width = 8, height = 6, dpi = 300, bg = "white"
)

# ============================================================
# FIGURE 4.13: C-NICAS INTER-ITEM CORRELATION HEATMAP
# ============================================================

cnicas_correlation_matrix <- cor(
  model_data[cnicas_items],
  use = "pairwise.complete.obs",
  method = "spearman"
)

png(
  filename = "figures/Figure4.13_CNICAS_correlation_heatmap.png",
  width = 2800, height = 2600, res = 300
)

corrplot::corrplot(
  cnicas_correlation_matrix,
  method  = "color",
  type    = "lower",
  order   = "hclust",
  tl.col  = "#17365D",
  tl.cex  = 0.55,
  tl.srt  = 45,
  diag    = FALSE,
  col     = colorRampPalette(c("#0072B2", "white", "#D55E00"))(200),
  title   = "Spearman Correlations Among C-NICAS Items",
  col.main = "#17365D",
  font.main = 2,
  cex.main  = 1.4
)

dev.off()

# ============================================================
# FIGURE 4.15: OBSERVED VS PREDICTED C-NICAS SCORES
# ============================================================

ggsave(
  "figures/Figure4.15_observed_vs_predicted.png",
  plot = figure_11_observed_predicted,
  width = 9, height = 7, dpi = 300, bg = "white"
)

# ============================================================
# FIGURE 4.16: TEN-FOLD CROSS-VALIDATION PERFORMANCE
# ============================================================

ggsave(
  "figures/Figure4.16_cross_validation_performance.png",
  plot = figure_12_validation,
  width = 10, height = 6, dpi = 300, bg = "white"
)

# ============================================================
# FIGURE 4.17: REGRESSION DIAGNOSTIC PLOTS
# ============================================================

png(
  filename = "figures/Figure4.17_regression_diagnostics.png",
  width = 2800, height = 2200, res = 300
)

par(
  mfrow   = c(2, 2),
  mar     = c(4, 4, 3, 1),
  col.axis = "#17365D",
  col.lab  = "#17365D",
  col.main = "#17365D"
)

plot(model_1)

par(mfrow = c(1, 1))

dev.off()

# ============================================================
# CONFIRM ALL FIGURES SAVED
# ============================================================

list.files("figures", pattern = "\\.png$", full.names = TRUE)
car::vif(model_1)
# ============================================================
# VIF TABLE WITH REPORTING LABELS
# ============================================================

vif_values <- car::vif(model_1)
tolerance_values <- 1 / vif_values

vif_labels <- c(
  academic_device_use_score = "Academic device-use frequency",
  computer_confidence_score = "Computer confidence",
  internet_access_yes = "Regular internet access",
  formal_computer_training_yes = "Formal computer training",
  ehr_exposure_level_score = "EHR exposure level",
  informatics_nurse_tutor_yes = "Informatics taught by a nurse tutor",
  online_learning_yes = "Prior online-learning exposure"
)

vif_table <- data.frame(
  Predictor = unname(vif_labels[names(vif_values)]),
  VIF = round(as.numeric(vif_values), 3),
  Tolerance = round(as.numeric(1 / vif_values), 3),
  Interpretation = dplyr::case_when(
    vif_values < 5 ~ "No serious multicollinearity",
    vif_values < 10 ~ "Potential multicollinearity: investigate",
    TRUE ~ "Serious multicollinearity"
  )
)

vif_table

write.csv(
  vif_table,
  "output/Table_VIF_Tolerance_Regression_Model.csv",
  row.names = FALSE
)
lab <- gsub("_", " ", sub("^cnicas_", "", colnames(cnicas_correlation_matrix)))
lab <- paste0(toupper(substring(lab, 1, 1)), substring(lab, 2))
colnames(cnicas_correlation_matrix) <- rownames(cnicas_correlation_matrix) <- lab

png("figures/Figure4_12_CNICAS_correlations.png", width = 3000, height = 2800, res = 300)
corrplot::corrplot(
  cnicas_correlation_matrix, method = "color", type = "lower", order = "hclust",
  tl.col = thesis_dark, tl.cex = 0.7, tl.srt = 45, diag = FALSE,
  col = colorRampPalette(c(thesis_blue, "white", thesis_red))(200),
  mar = c(0, 0, 1, 0)
)
dev.off()

