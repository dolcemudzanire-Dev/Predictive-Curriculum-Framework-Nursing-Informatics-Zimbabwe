# ============================================================
# GNIC-PT
# GENERAL NURSING STUDENT INFORMATICS COMPETENCE
# PREDICTION TOOL
#
# THIS VERSION:
# 1. Loads mydata.csv automatically
# 2. Cleans the data
# 3. Removes variables with >40% missingness
# 4. Creates the 26 C-NICAS items
# 5. Calculates C-NICAS mean score
# 6. Creates the regression predictors
# 7. Builds model_1 automatically
# 8. Starts the GNIC-PT Shiny application
#
# app.R is inside the "app" folder.
# mydata.csv is inside the main project's "data" folder.
# ============================================================


# ============================================================
# 1. INSTALL / LOAD REQUIRED PACKAGES
# ============================================================

required_packages <- c(
  "shiny",
  "ggplot2",
  "dplyr",
  "stringr",
  "scales",
  "DT"
)

for (package_name in required_packages) {
  
  if (!requireNamespace(package_name, quietly = TRUE)) {
    
    install.packages(package_name)
  }
}

library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(scales)
library(DT)


# ============================================================
# 2. CREATE OUTPUT FOLDERS
# ============================================================

dir.create(
  "../output",
  showWarnings = FALSE
)

dir.create(
  "../figures",
  showWarnings = FALSE
)


# ============================================================
# 3. FIND THE DATA FILE
# ============================================================

data_file <- "DATA/mydata.csv"


if (!file.exists(data_file)) {
  
  stop(
    paste0(
      "\n\nDATA FILE NOT FOUND.\n\n",
      "R is looking for:\n",
      normalizePath(
        data_file,
        winslash = "/",
        mustWork = FALSE
      ),
      "\n\n",
      "Please make sure mydata.csv is inside the DATA folder.\n"
    )
  )
}


# ============================================================
# 4. IMPORT DATA
# ============================================================

mydata <- read.csv(
  data_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

rawdata <- mydata

analysisdata <- mydata


# ============================================================
# 5. CLEAN TEXT VALUES
# ============================================================

analysisdata <- analysisdata %>%
  
  mutate(
    across(
      where(is.character),
      ~ str_squish(.x)
    )
  ) %>%
  
  mutate(
    across(
      where(is.character),
      ~ na_if(.x, "")
    )
  )


# ============================================================
# 6. ASSESS MISSINGNESS
# ============================================================

missingness_table <- data.frame(
  
  original_variable =
    names(analysisdata),
  
  missing_n =
    colSums(is.na(analysisdata)),
  
  missing_percent =
    round(
      colMeans(is.na(analysisdata)) * 100,
      1
    )
  
) %>%
  
  arrange(
    desc(missing_percent)
  )


write.csv(
  missingness_table,
  "../output/missingness_before_column_removal.csv",
  row.names = FALSE
)


# ============================================================
# 7. REMOVE VARIABLES WITH >40% MISSING DATA
# ============================================================

missing_threshold <- 40


columns_to_remove <- missingness_table %>%
  
  filter(
    missing_percent > missing_threshold
  ) %>%
  
  pull(
    original_variable
  )


analysisdata <- analysisdata %>%
  
  select(
    -all_of(columns_to_remove)
  )


# Save removed-variable report

removed_columns_report <- missingness_table %>%
  
  filter(
    original_variable %in% columns_to_remove
  )


write.csv(
  removed_columns_report,
  "../output/columns_removed_extensive_missingness.csv",
  row.names = FALSE
)


# ============================================================
# 8. REMOVE TIMESTAMP
# ============================================================

if (
  "Timestamp" %in%
  names(analysisdata)
) {
  
  analysisdata <- analysisdata %>%
    
    select(
      -Timestamp
    )
}


# ============================================================
# 9. CHECK THAT 49 VARIABLES REMAIN
# ============================================================

if (
  ncol(analysisdata) != 49
) {
  
  stop(
    paste0(
      "\n\nVARIABLE NUMBER ERROR.\n\n",
      "After cleaning and removing variables with >40% missingness, ",
      ncol(analysisdata),
      " variables remain.\n\n",
      "Your original analysis expects 49 variables.\n\n",
      "Please check mydata.csv and the missing-data step."
    )
  )
}


# ============================================================
# 10. RENAME THE 49 VARIABLES
# ============================================================

names(analysisdata) <- c(
  
  # ----------------------------------------------------------
  # 1-2 DEMOGRAPHICS
  # ----------------------------------------------------------
  
  "sex",
  
  "year_study",
  
  
  # ----------------------------------------------------------
  # 3-11 DIGITAL ACCESS / PREVIOUS EXPOSURE
  # ----------------------------------------------------------
  
  "smartphone",
  
  "computer",
  
  "internet_access",
  
  "internet_location",
  
  "academic_device_use",
  
  "formal_computer_training",
  
  "computer_training_type",
  
  "ehr_exposure",
  
  "ehr_exposure_level",
  
  
  # ----------------------------------------------------------
  # 12-22 EDUCATIONAL / INSTITUTIONAL FACTORS
  # ----------------------------------------------------------
  
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
  
  
  # ----------------------------------------------------------
  # 23-48 C-NICAS ITEMS
  # ----------------------------------------------------------
  
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
  
  
  # ----------------------------------------------------------
  # 49 EXPERIENCE SOURCE
  # ----------------------------------------------------------
  
  "informatics_experience_source"
)


# ============================================================
# 11. IDENTIFY THE 26 C-NICAS ITEMS
# ============================================================

cnicas_items <- names(
  analysisdata
)[
  grepl(
    "^cnicas",
    names(analysisdata)
  )
]


if (
  length(cnicas_items) != 26
) {
  
  stop(
    paste0(
      "\n\nC-NICAS ERROR.\n\n",
      "The app found ",
      length(cnicas_items),
      " C-NICAS variables.\n",
      "The analysis requires 26 C-NICAS items."
    )
  )
}


# ============================================================
# 12. CORRECT "MOTHLY" TO "MONTHLY"
# ============================================================

analysisdata <- analysisdata %>%
  
  mutate(
    
    academic_device_use =
      dplyr::recode(
        
        as.character(
          academic_device_use
        ),
        
        "Mothly" =
          "Monthly",
        
        .default =
          as.character(
            academic_device_use
          )
      )
  )


# ============================================================
# 13. CONVERT C-NICAS RESPONSES TO NUMERIC
# ============================================================

analysisdata <- analysisdata %>%
  
  mutate(
    
    across(
      
      all_of(
        cnicas_items
      ),
      
      ~ as.numeric(
        str_extract(
          as.character(.x),
          "[1-5]"
        )
      )
    )
  )


# ============================================================
# 14. C-NICAS MISSING ITEMS
# ============================================================

analysisdata$cnicas_missing_items <-
  
  rowSums(
    is.na(
      analysisdata[
        cnicas_items
      ]
    )
  )


analysisdata$cnicas_answered_items <-
  
  rowSums(
    !is.na(
      analysisdata[
        cnicas_items
      ]
    )
  )


# ============================================================
# 15. CALCULATE C-NICAS SCORE
#
# 80% of 26 items must be answered.
# ============================================================

minimum_cnicas_items <-
  
  ceiling(
    length(cnicas_items) *
      0.80
  )


analysisdata <- analysisdata %>%
  
  mutate(
    
    cnicas_total = ifelse(
      
      cnicas_answered_items >=
        minimum_cnicas_items,
      
      rowSums(
        across(
          all_of(cnicas_items)
        ),
        na.rm = TRUE
      ),
      
      NA_real_
    ),
    
    
    cnicas_mean = ifelse(
      
      cnicas_answered_items >=
        minimum_cnicas_items,
      
      rowMeans(
        across(
          all_of(cnicas_items)
        ),
        na.rm = TRUE
      ),
      
      NA_real_
    )
  )


# ============================================================
# 16. CREATE MODEL DATA
# ============================================================

model_data <- analysisdata


# ============================================================
# 17. CREATE BINARY PREDICTORS
# ============================================================

model_data <- model_data %>%
  
  mutate(
    
    smartphone_yes =
      ifelse(
        smartphone == "Yes",
        1,
        0
      ),
    
    computer_yes =
      ifelse(
        computer == "Yes",
        1,
        0
      ),
    
    internet_access_yes =
      ifelse(
        internet_access == "Yes",
        1,
        0
      ),
    
    formal_computer_training_yes =
      ifelse(
        formal_computer_training == "Yes",
        1,
        0
      ),
    
    ehr_exposure_yes =
      ifelse(
        ehr_exposure == "Yes",
        1,
        0
      ),
    
    computer_lab_yes =
      ifelse(
        computer_lab == "Yes",
        1,
        0
      ),
    
    institution_internet_yes =
      ifelse(
        institution_internet == "Yes",
        1,
        0
      ),
    
    informatics_nurse_tutor_yes =
      ifelse(
        informatics_taught_nurse_tutor == "Yes",
        1,
        0
      ),
    
    informatics_other_professional_yes =
      ifelse(
        informatics_taught_other_professional ==
          "Yes",
        1,
        0
      ),
    
    tutor_confidence_yes =
      ifelse(
        tutor_confidence == "Yes",
        1,
        0
      ),
    
    online_learning_yes =
      ifelse(
        online_learning_exposure == "Yes",
        1,
        0
      ),
    
    digital_communication_yes =
      ifelse(
        digital_academic_communication == "Yes",
        1,
        0
      )
  )


# ============================================================
# 18. CREATE ORDERED PREDICTORS
# ============================================================

model_data <- model_data %>%
  
  mutate(
    
    academic_device_use_score =
      
      dplyr::recode(
        
        as.character(
          academic_device_use
        ),
        
        "Never" =
          0,
        
        "Rarely" =
          1,
        
        "Monthly" =
          2,
        
        "Weekly" =
          3,
        
        "Daily" =
          4,
        
        .default =
          NA_real_
      ),
    
    
    computer_confidence_score =
      
      dplyr::recode(
        
        as.character(
          computer_confidence
        ),
        
        "Very low" =
          1,
        
        "Low" =
          2,
        
        "Moderate" =
          3,
        
        "High" =
          4,
        
        "Very High" =
          5,
        
        "Very high" =
          5,
        
        .default =
          NA_real_
      ),
    
    
    ict_resources_score =
      
      dplyr::recode(
        
        as.character(
          ict_resources_adequate
        ),
        
        "No" =
          0,
        
        "Partly" =
          1,
        
        "Yes" =
          2,
        
        .default =
          NA_real_
      ),
    
    
    ehr_exposure_level_score =
      
      dplyr::recode(
        
        as.character(
          ehr_exposure_level
        ),
        
        "Not applicable" =
          0,
        
        "Observed Only" =
          1,
        
        "Assisted use" =
          2,
        
        "Independent" =
          3,
        
        "Formal Training" =
          4,
        
        .default =
          NA_real_
      )
  )


# ============================================================
# 19. PREPARE REGRESSION DATA
# ============================================================

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


regression_data <- model_data %>%
  
  dplyr::select(
    dplyr::all_of(
      regression_variables
    )
  )


# Complete cases only

regression_data <- regression_data[
  stats::complete.cases(
    regression_data
  ),
]


# ============================================================
# 20. CHECK REGRESSION DATA
# ============================================================

if (
  nrow(regression_data) < 10
) {
  
  stop(
    paste0(
      "\n\nREGRESSION ERROR.\n\n",
      "Only ",
      nrow(regression_data),
      " complete observations are available.\n",
      "Please check the data."
    )
  )
}


# ============================================================
# 21. BUILD THE ORIGINAL MULTIPLE LINEAR REGRESSION MODEL
# ============================================================

model_1 <- stats::lm(
  
  cnicas_mean ~
    
    academic_device_use_score +
    
    computer_confidence_score +
    
    internet_access_yes +
    
    formal_computer_training_yes +
    
    ehr_exposure_level_score +
    
    informatics_nurse_tutor_yes +
    
    online_learning_yes,
  
  data =
    regression_data
)


# The Shiny prediction model

prediction_model <- model_1


# ============================================================
# 22. MODEL INFORMATION
# ============================================================

model_summary <-
  summary(
    model_1
  )


model_r2 <-
  model_summary$r.squared


model_adjusted_r2 <-
  model_summary$adj.r.squared


model_rmse <-
  model_summary$sigma


model_n <-
  nrow(
    regression_data
  )


# Save model information

model_information <- data.frame(
  
  Sample_Size =
    model_n,
  
  R_Squared =
    model_r2,
  
  Adjusted_R_Squared =
    model_adjusted_r2,
  
  Residual_Standard_Error =
    model_rmse
)


write.csv(
  
  model_information,
  
  "../output/GNIC_PT_model_information.csv",
  
  row.names = FALSE
)


# ============================================================
# 23. DEVELOPER EMAIL
# ============================================================

developer_email <-
  "dmudzanire@gmail.com"


# ============================================================
# USER INTERFACE
# ============================================================

ui <- fluidPage(
  
  title =
    "GNIC-PT",
  
  
  # ==========================================================
  # CSS
  # ==========================================================
  
  tags$head(
    
    tags$style(
      
      HTML("

      body {

        background-color:
          #F5F8FC;

        font-family:
          Arial,
          Helvetica,
          sans-serif;
      }


      h1, h2, h3, h4, h5, h6,
      label,
      p,
      .control-label,
      .menu-button,
      .student-info-button,
      .predictor-button,
      .app-title,
      .app-subtitle {

        font-weight:
          bold !important;
      }


      h1, h2, h3, h4, h5, h6,
      label,
      .control-label,
      .app-title,
      .app-subtitle,
      .menu-button,
      .student-info-button,
      .predictor-button {

        white-space:
          nowrap;

        overflow:
          hidden;

        text-overflow:
          ellipsis;

        max-width:
          100%;
      }


      h3 {

        font-size:
          18px !important;
      }


      h4 {

        font-size:
          15px !important;
      }


      label,
      .control-label {

        font-size:
          13px !important;
      }


      .title-box {

        background:
          linear-gradient(
            135deg,
            #12355B,
            #0072B2
          );

        color:
          white;

        padding:
          22px;

        border-radius:
          12px;

        margin-top:
          15px;

        margin-bottom:
          10px;

        text-align:
          center;

        overflow:
          hidden;
      }


      .app-title {

        font-size:
          40px;

        margin-top:
          0;

        margin-bottom:
          8px;

        letter-spacing:
          1px;
      }


      .app-subtitle {

        font-size:
          14px;

        margin-top:
          0;

        margin-bottom:
          0;

        letter-spacing:
          0.2px;
      }


      .menu-bar {

        background-color:
          white;

        border-radius:
          10px;

        padding:
          10px;

        margin-bottom:
          20px;

        text-align:
          center;

        box-shadow:
          0 2px 9px
          rgba(
            0,
            0,
            0,
            0.12
          );

        white-space:
          nowrap;

        overflow-x:
          auto;
      }


      .navy-button {

        background:
          linear-gradient(
            145deg,
            #12355B,
            #061F3B
          ) !important;

        color:
          white !important;

        border:
          1px solid #031426 !important;

        border-radius:
          4px !important;

        font-weight:
          bold !important;

        font-size:
          12px !important;

        min-width:
          88px;

        min-height:
          40px;

        margin:
          4px;

        box-shadow:
          0 4px 0 #031426,
          0 6px 9px
          rgba(
            0,
            0,
            0,
            0.20
          );
      }


      .red-round-button {

        background:
          linear-gradient(
            145deg,
            #E74C3C,
            #A51C16
          ) !important;

        color:
          white !important;

        border:
          2px solid #7C100B !important;

        border-radius:
          50% !important;

        font-weight:
          bold !important;

        font-size:
          11px !important;

        width:
          70px;

        height:
          70px;

        margin:
          4px 12px;

        box-shadow:
          0 5px 0 #7C100B,
          0 7px 10px
          rgba(
            0,
            0,
            0,
            0.24
          );
      }


      .card {

        background-color:
          white;

        border-radius:
          12px;

        padding:
          18px;

        margin-bottom:
          20px;

        box-shadow:
          0 2px 9px
          rgba(
            0,
            0,
            0,
            0.10
          );

        overflow:
          hidden;
      }


      .prediction-card {

        background:
          linear-gradient(
            145deg,
            #FFFFFF,
            #E9F4F1
          );

        border-radius:
          14px;

        padding:
          20px;

        margin-bottom:
          20px;

        border:
          1px solid #D0E5DD;

        box-shadow:
          8px 8px 16px
          rgba(
            0,
            0,
            0,
            0.16
          );

        overflow:
          hidden;
      }


      .student-info-button,
      .predictor-button {

        background:
          linear-gradient(
            145deg,
            #0072B2,
            #12355B
          ) !important;

        color:
          white !important;

        border:
          1px solid #061F3B !important;

        border-radius:
          6px !important;

        font-size:
          16px !important;

        min-height:
          62px;

        width:
          100%;

        margin-top:
          8px;

        margin-bottom:
          15px;

        box-shadow:
          0 6px 0 #061F3B,
          0 8px 12px
          rgba(
            0,
            0,
            0,
            0.23
          );
      }


      .student-info-panel,
      .predictor-panel {

        background-color:
          #F8FCFF;

        border:
          1px solid #CEE1EF;

        border-radius:
          8px;

        padding:
          15px;

        margin-bottom:
          15px;

        overflow:
          hidden;
      }


      .result-box {

        background-color:
          #E8F5E9;

        border-left:
          7px solid #009E73;

        border-radius:
          8px;

        padding:
          18px;

        text-align:
          center;

        overflow:
          hidden;
      }


      .student-name {

        font-weight:
          bold !important;

        color:
          #12355B;

        font-size:
          17px;

        white-space:
          nowrap;

        overflow:
          hidden;

        text-overflow:
          ellipsis;
      }


      .result-score {

        font-size:
          44px;

        font-weight:
          bold;

        color:
          #12355B;

        line-height:
          1.2;
      }


      .competency-level {

        font-size:
          16px;

        font-weight:
          bold;

        color:
          #0072B2;

        margin-top:
          10px;

        margin-bottom:
          6px;
      }


      .small-result-text {

        font-size:
          11px;

        color:
          #4B4B4B;

        line-height:
          1.35;

        margin-top:
          10px;

        margin-bottom:
          2px;
      }


      .instruction-note {

        background-color:
          #EAF3FA;

        border-left:
          5px solid #0072B2;

        padding:
          12px;

        border-radius:
          5px;

        margin-bottom:
          15px;

        font-size:
          12px;

        font-weight:
          bold;
      }


      .hidden-panel {

        margin-top:
          15px;

        padding:
          16px;

        background-color:
          #FFFFFF;

        border-radius:
          10px;

        border:
          1px solid #D7E1EA;

        overflow:
          hidden;
      }


      .help-question {

        display:
          block;

        width:
          100%;

        text-align:
          left;

        background-color:
          #F2F7FB;

        color:
          #12355B;

        border:
          1px solid #C8DAE8;

        border-radius:
          5px;

        padding:
          11px;

        margin-top:
          8px;

        font-weight:
          bold;

        font-size:
          13px;

        white-space:
          nowrap;

        overflow:
          hidden;

        text-overflow:
          ellipsis;
      }


      .help-answer {

        background-color:
          #EAF3FA;

        border-left:
          5px solid #0072B2;

        padding:
          12px;

        border-radius:
          5px;

        margin-top:
          12px;

        font-weight:
          bold;

        font-size:
          12px;
      }


      ")
    )
  ),
  
  
  # ==========================================================
  # TITLE
  # ==========================================================
  
  div(
    
    class =
      "title-box",
    
    div(
      
      class =
        "app-title",
      
      "GNIC-PT"
    ),
    
    div(
      
      class =
        "app-subtitle",
      
      "GENERAL NURSING STUDENT INFORMATICS COMPETENCE PREDICTION TOOL"
    )
  ),
  
  
  # ==========================================================
  # MENU
  # ==========================================================
  
  div(
    
    class =
      "menu-bar",
    
    actionButton(
      "profile_button",
      "PROFILE",
      class =
        "red-round-button menu-button"
    ),
    
    actionButton(
      "roster_button",
      "ROSTER",
      class =
        "navy-button menu-button"
    ),
    
    actionButton(
      "search_button",
      "SEARCH",
      class =
        "navy-button menu-button"
    ),
    
    actionButton(
      "overview_button",
      "OVERVIEW",
      class =
        "navy-button menu-button"
    ),
    
    actionButton(
      "help_button",
      "HELP",
      class =
        "navy-button menu-button"
    ),
    
    actionButton(
      "info_button",
      "INFO",
      class =
        "navy-button menu-button"
    )
  ),
  
  
  # ==========================================================
  # MAIN BODY
  # ==========================================================
  
  fluidRow(
    
    # --------------------------------------------------------
    # LEFT
    # --------------------------------------------------------
    
    column(
      
      width =
        5,
      
      div(
        
        class =
          "card",
        
        actionButton(
          
          "student_info_button",
          
          "Student Information",
          
          class =
            "student-info-button"
        ),
        
        uiOutput(
          "student_info_panel"
        ),
        
        
        actionButton(
          
          "predictor_button",
          
          "Student Predictor Information",
          
          class =
            "predictor-button"
        ),
        
        
        actionButton(
          
          "reset_button",
          
          "RESET",
          
          class =
            "predictor-button"
        ),
        
        
        downloadButton(
          
          "export_button",
          
          "EXPORT",
          
          class =
            "predictor-button"
        ),
        
        
        uiOutput(
          "predictor_panel"
        )
      )
    ),
    
    
    # --------------------------------------------------------
    # RIGHT
    # --------------------------------------------------------
    
    column(
      
      width =
        7,
      
      div(
        
        class =
          "prediction-card",
        
        h3(
          "Predicted Nursing Informatics Competence"
        ),
        
        uiOutput(
          "prediction_display"
        )
      ),
      
      
      div(
        
        class =
          "card",
        
        h3(
          "Prediction on the C-NICAS Scale"
        ),
        
        plotOutput(
          
          "prediction_plot",
          
          height =
            "300px"
        )
      ),
      
      
      uiOutput(
        "selected_panel"
      )
    )
  )
)


# ============================================================
# SERVER
# ============================================================

server <- function(
    input,
    output,
    session
) {
  
  
  # ==========================================================
  # PANEL STATES
  # ==========================================================
  
  student_info_open <-
    reactiveVal(
      FALSE
    )
  
  
  predictor_open <-
    reactiveVal(
      FALSE
    )
  
  
  active_panel <-
    reactiveVal(
      "none"
    )
  
  
  # ==========================================================
  # STUDENT INFORMATION BUTTON
  # ==========================================================
  
  observeEvent(
    
    input$student_info_button,
    
    {
      
      student_info_open(
        !student_info_open()
      )
    }
  )
  
  
  # ==========================================================
  # PREDICTOR BUTTON
  # ==========================================================
  
  observeEvent(
    
    input$predictor_button,
    
    {
      
      predictor_open(
        !predictor_open()
      )
    }
  )
  
  
  # ==========================================================
  # RESET BUTTON
  # ==========================================================
  
  observeEvent(
    
    input$reset_button,
    
    {
      
      student_info_open(
        TRUE
      )
      
      predictor_open(
        TRUE
      )
      
      
      updateTextInput(
        
        session,
        
        "student_name",
        
        value =
          ""
      )
      
      
      updateTextInput(
        
        session,
        
        "school_name",
        
        value =
          ""
      )
      
      
      updateTextInput(
        
        session,
        
        "student_group",
        
        value =
          ""
      )
      
      
      updateSelectInput(
        
        session,
        
        "academic_device_use_score",
        
        selected =
          "2"
      )
      
      
      updateSelectInput(
        
        session,
        
        "computer_confidence_score",
        
        selected =
          "3"
      )
      
      
      updateRadioButtons(
        
        session,
        
        "internet_access_yes",
        
        selected =
          "1"
      )
      
      
      updateRadioButtons(
        
        session,
        
        "formal_computer_training_yes",
        
        selected =
          "0"
      )
      
      
      updateSelectInput(
        
        session,
        
        "ehr_exposure_level_score",
        
        selected =
          "0"
      )
      
      
      updateRadioButtons(
        
        session,
        
        "informatics_nurse_tutor_yes",
        
        selected =
          "0"
      )
      
      
      updateRadioButtons(
        
        session,
        
        "online_learning_yes",
        
        selected =
          "0"
      )
    }
  )
  
  
  # ==========================================================
  # TOP MENU
  # ==========================================================
  
  observeEvent(
    input$profile_button,
    {
      active_panel("profile")
    }
  )
  
  
  observeEvent(
    input$roster_button,
    {
      active_panel("roster")
    }
  )
  
  
  observeEvent(
    input$search_button,
    {
      active_panel("search")
    }
  )
  
  
  observeEvent(
    input$overview_button,
    {
      active_panel("overview")
    }
  )
  
  
  observeEvent(
    input$help_button,
    {
      active_panel("help")
    }
  )
  
  
  observeEvent(
    input$info_button,
    {
      active_panel("info")
    }
  )
  
  
  # ==========================================================
  # STUDENT INFORMATION PANEL
  # ==========================================================
  
  output$student_info_panel <-
    renderUI({
      
      if (
        !student_info_open()
      ) {
        
        return(NULL)
      }
      
      
      div(
        
        class =
          "student-info-panel",
        
        textInput(
          
          "student_name",
          
          "Student Full Name",
          
          placeholder =
            "Enter student full name"
        ),
        
        
        textInput(
          
          "school_name",
          
          "School Name",
          
          placeholder =
            "Enter school name"
        ),
        
        
        textInput(
          
          "student_group",
          
          "Group",
          
          placeholder =
            "Enter group"
        )
      )
    })
  
  
  # ==========================================================
  # PREDICTOR PANEL
  # ==========================================================
  
  output$predictor_panel <-
    renderUI({
      
      if (
        !predictor_open()
      ) {
        
        return(NULL)
      }
      
      
      div(
        
        class =
          "predictor-panel",
        
        div(
          
          class =
            "instruction-note",
          
          "The predicted competence score updates automatically whenever you change an answer."
        ),
        
        
        selectInput(
          
          "academic_device_use_score",
          
          "Academic Device-Use Frequency",
          
          choices = c(
            
            "Never" =
              0,
            
            "Rarely" =
              1,
            
            "Monthly" =
              2,
            
            "Weekly" =
              3,
            
            "Daily" =
              4
          ),
          
          selected =
            2
        ),
        
        
        selectInput(
          
          "computer_confidence_score",
          
          "Computer Confidence",
          
          choices = c(
            
            "Very Low" =
              1,
            
            "Low" =
              2,
            
            "Moderate" =
              3,
            
            "High" =
              4,
            
            "Very High" =
              5
          ),
          
          selected =
            3
        ),
        
        
        radioButtons(
          
          "internet_access_yes",
          
          "Regular Internet Access for Academic Work",
          
          choices = c(
            
            "No" =
              0,
            
            "Yes" =
              1
          ),
          
          selected =
            1,
          
          inline =
            TRUE
        ),
        
        
        radioButtons(
          
          "formal_computer_training_yes",
          
          "Formal Computer Training Before Nursing School",
          
          choices = c(
            
            "No" =
              0,
            
            "Yes" =
              1
          ),
          
          selected =
            0,
          
          inline =
            TRUE
        ),
        
        
        selectInput(
          
          "ehr_exposure_level_score",
          
          "Electronic Health Record Exposure Level",
          
          choices = c(
            
            "No EHR Exposure" =
              0,
            
            "Observed EHR Use Only" =
              1,
            
            "Assisted With EHR Use" =
              2,
            
            "Used EHR Independently" =
              3,
            
            "Received Formal EHR Training" =
              4
          ),
          
          selected =
            0
        ),
        
        
        radioButtons(
          
          "informatics_nurse_tutor_yes",
          
          "Informatics Taught by a Nurse Tutor",
          
          choices = c(
            
            "No" =
              0,
            
            "Yes" =
              1
          ),
          
          selected =
            0,
          
          inline =
            TRUE
        ),
        
        
        radioButtons(
          
          "online_learning_yes",
          
          "Previous Online-Learning Exposure",
          
          choices = c(
            
            "No" =
              0,
            
            "Yes" =
              1
          ),
          
          selected =
            0,
          
          inline =
            TRUE
        )
      )
    })
  
  
  # ==========================================================
  # CURRENT STUDENT INPUT
  # ==========================================================
  
  student_input <-
    reactive({
      
      data.frame(
        
        academic_device_use_score =
          as.numeric(
            ifelse(
              
              is.null(
                input$academic_device_use_score
              ),
              
              2,
              
              input$academic_device_use_score
            )
          ),
        
        
        computer_confidence_score =
          as.numeric(
            ifelse(
              
              is.null(
                input$computer_confidence_score
              ),
              
              3,
              
              input$computer_confidence_score
            )
          ),
        
        
        internet_access_yes =
          as.numeric(
            ifelse(
              
              is.null(
                input$internet_access_yes
              ),
              
              1,
              
              input$internet_access_yes
            )
          ),
        
        
        formal_computer_training_yes =
          as.numeric(
            ifelse(
              
              is.null(
                input$formal_computer_training_yes
              ),
              
              0,
              
              input$formal_computer_training_yes
            )
          ),
        
        
        ehr_exposure_level_score =
          as.numeric(
            ifelse(
              
              is.null(
                input$ehr_exposure_level_score
              ),
              
              0,
              
              input$ehr_exposure_level_score
            )
          ),
        
        
        informatics_nurse_tutor_yes =
          as.numeric(
            ifelse(
              
              is.null(
                input$informatics_nurse_tutor_yes
              ),
              
              0,
              
              input$informatics_nurse_tutor_yes
            )
          ),
        
        
        online_learning_yes =
          as.numeric(
            ifelse(
              
              is.null(
                input$online_learning_yes
              ),
              
              0,
              
              input$online_learning_yes
            )
          )
      )
    })
  
  
  # ==========================================================
  # LIVE PREDICTION
  # ==========================================================
  
  prediction_result <-
    reactive({
      
      prediction <-
        predict(
          
          prediction_model,
          
          newdata =
            student_input(),
          
          interval =
            "prediction",
          
          level =
            0.95
        )
      
      
      data.frame(
        
        predicted_score =
          as.numeric(
            prediction[
              1,
              "fit"
            ]
          ),
        
        lower_prediction =
          as.numeric(
            prediction[
              1,
              "lwr"
            ]
          ),
        
        upper_prediction =
          as.numeric(
            prediction[
              1,
              "upr"
            ]
          )
      )
    })
  
  
  # ==========================================================
  # PREDICTOR PROFILE
  # ==========================================================
  
  predictor_evaluation <-
    reactive({
      
      inputs <-
        student_input()
      
      
      data.frame(
        
        predictor = c(
          
          "Academic Device Use",
          
          "Computer Confidence",
          
          "Internet Access",
          
          "Formal Computer Training",
          
          "EHR Exposure",
          
          "Nurse-Tutor Informatics Teaching",
          
          "Online-Learning Exposure"
        ),
        
        
        raw_value = c(
          
          inputs$academic_device_use_score,
          
          inputs$computer_confidence_score,
          
          inputs$internet_access_yes,
          
          inputs$formal_computer_training_yes,
          
          inputs$ehr_exposure_level_score,
          
          inputs$informatics_nurse_tutor_yes,
          
          inputs$online_learning_yes
        ),
        
        
        maximum_value = c(
          
          4,
          
          5,
          
          1,
          
          1,
          
          4,
          
          1,
          
          1
        )
      ) %>%
        
        mutate(
          
          percentage =
            raw_value /
            maximum_value,
          
          
          status =
            case_when(
              
              percentage >=
                0.75 ~
                
                "STRENGTH",
              
              percentage >=
                0.40 ~
                
                "MODERATE",
              
              TRUE ~
                
                "NEEDS SUPPORT"
            ),
          
          
          colour =
            case_when(
              
              status ==
                "STRENGTH" ~
                
                "#009E73",
              
              status ==
                "MODERATE" ~
                
                "#E69F00",
              
              TRUE ~
                
                "#D55E00"
            )
        )
    })
  
  
  # ==========================================================
  # PREDICTION DISPLAY
  # ==========================================================
  
  output$prediction_display <-
    renderUI({
      
      result <-
        prediction_result()
      
      
      predicted_score <-
        
        max(
          
          1,
          
          min(
            5,
            result$predicted_score
          )
        )
      
      
      lower_score <-
        
        max(
          
          1,
          
          result$lower_prediction
        )
      
      
      upper_score <-
        
        min(
          
          5,
          
          result$upper_prediction
        )
      
      
      entered_name <-
        input$student_name
      
      
      student_display_name <-
        
        ifelse(
          
          is.null(
            entered_name
          ) ||
            
            entered_name == "",
          
          "THE STUDENT",
          
          toupper(
            entered_name
          )
        )
      
      
      competency_category <-
        
        case_when(
          
          predicted_score < 2.50 ~
            
            "LOWER PREDICTED COMPETENCY",
          
          predicted_score < 3.50 ~
            
            "MODERATE PREDICTED COMPETENCY",
          
          predicted_score < 4.25 ~
            
            "GOOD PREDICTED COMPETENCY",
          
          TRUE ~
            
            "HIGHER PREDICTED COMPETENCY"
        )
      
      
      recommended_action <-
        
        case_when(
          
          predicted_score < 2.50 ~
            
            "The student may benefit from foundational computer-skills training, structured nursing-informatics lessons, guided digital-health practice, and regular tutor support.",
          
          
          predicted_score < 3.50 ~
            
            "The student may benefit from additional supervised work with online-learning platforms, digital academic resources, and simulated electronic health record activities.",
          
          
          predicted_score < 4.25 ~
            
            "The student has a good predicted competence level. Continue practical nursing-informatics learning, clinical application, and digital-health skills development.",
          
          
          TRUE ~
            
            "The student has a higher predicted competence level. Consider advanced informatics activities, clinical digital-health exposure, and peer-learning or peer-support responsibilities."
        )
      
      
      div(
        
        class =
          "result-box",
        
        
        p(
          
          style =
            "font-size:13px; margin-bottom:4px;",
          
          "PREDICTED C-NICAS COMPETENCE SCORE FOR"
        ),
        
        
        div(
          
          class =
            "student-name",
          
          student_display_name
        ),
        
        
        div(
          
          class =
            "result-score",
          
          paste0(
            
            round(
              predicted_score,
              2
            ),
            
            " / 5"
          )
        ),
        
        
        div(
          
          class =
            "competency-level",
          
          competency_category
        ),
        
        
        p(
          
          class =
            "small-result-text",
          
          paste0(
            
            "Approximate individual prediction range: ",
            
            round(
              lower_score,
              2
            ),
            
            " to ",
            
            round(
              upper_score,
              2
            )
          )
        ),
        
        
        p(
          
          class =
            "small-result-text",
          
          recommended_action
        )
      )
    })
  
  
  # ==========================================================
  # MENU PANELS
  # ==========================================================
  
  output$selected_panel <-
    renderUI({
      
      panel <-
        active_panel()
      
      
      # ========================================================
      # PROFILE
      # ========================================================
      
      if (
        panel ==
        "profile"
      ) {
        
        return(
          
          div(
            
            class =
              "card",
            
            h3(
              "Student Strengths and Areas Needing Support"
            ),
            
            p(
              
              style =
                "font-size:12px; font-weight:bold;",
              
              "GREEN = STRENGTH | ORANGE = MODERATE | RED = NEEDS SUPPORT"
            ),
            
            div(
              
              class =
                "hidden-panel",
              
              plotOutput(
                
                "strength_weakness_plot",
                
                height =
                  "420px"
              )
            )
          )
        )
      }
      
      
      # ========================================================
      # ROSTER
      # ========================================================
      
      if (
        panel ==
        "roster"
      ) {
        
        return(
          
          div(
            
            class =
              "card",
            
            h3(
              "Student Competence Roster"
            ),
            
            p(
              
              style =
                "font-size:12px; font-weight:bold;",
              
              "Student numbers correspond to the order of records in the dataset."
            ),
            
            div(
              
              class =
                "hidden-panel",
              
              DTOutput(
                "roster_table"
              )
            )
          )
        )
      }
      
      
      # ========================================================
      # SEARCH
      # ========================================================
      
      if (
        panel ==
        "search"
      ) {
        
        return(
          
          div(
            
            class =
              "card",
            
            h3(
              "Student Search"
            ),
            
            div(
              
              class =
                "hidden-panel",
              
              numericInput(
                
                "search_student_number",
                
                "Student Number",
                
                value =
                  NULL,
                
                min =
                  1,
                
                step =
                  1
              ),
              
              uiOutput(
                "student_search_result"
              )
            )
          )
        )
      }
      
      
      # ========================================================
      # OVERVIEW
      # ========================================================
      
      if (
        panel ==
        "overview"
      ) {
        
        return(
          
          div(
            
            class =
              "card",
            
            h3(
              "Group Competence Overview"
            ),
            
            div(
              
              class =
                "hidden-panel",
              
              plotOutput(
                
                "group_competence_plot",
                
                height =
                  "420px"
              ),
              
              uiOutput(
                "group_competence_summary"
              )
            )
          )
        )
      }
      
      
      # ========================================================
      # HELP
      # ========================================================
      
      if (
        panel ==
        "help"
      ) {
        
        return(
          
          div(
            
            class =
              "card",
            
            h3(
              "Help and Support"
            ),
            
            div(
              
              class =
                "hidden-panel",
              
              h4(
                "Click a Question for an Answer"
              ),
              
              
              actionButton(
                "faq_use_app",
                "How Do I Use the App?",
                class =
                  "help-question"
              ),
              
              
              actionButton(
                "faq_profile",
                "What Does PROFILE Do?",
                class =
                  "help-question"
              ),
              
              
              actionButton(
                "faq_roster",
                "What Does ROSTER Do?",
                class =
                  "help-question"
              ),
              
              
              actionButton(
                "faq_search",
                "What Does SEARCH Do?",
                class =
                  "help-question"
              ),
              
              
              actionButton(
                "faq_overview",
                "What Does OVERVIEW Do?",
                class =
                  "help-question"
              ),
              
              
              actionButton(
                "faq_info",
                "What Does INFO Do?",
                class =
                  "help-question"
              ),
              
              
              actionButton(
                "faq_score",
                "Why Does the Predicted Score Change?",
                class =
                  "help-question"
              ),
              
              
              actionButton(
                "faq_reset",
                "What Does RESET Do?",
                class =
                  "help-question"
              ),
              
              
              actionButton(
                "faq_export",
                "What Does EXPORT Do?",
                class =
                  "help-question"
              ),
              
              
              uiOutput(
                "faq_answer"
              ),
              
              
              tags$hr(),
              
              
              h4(
                "Further Assistance"
              ),
              
              
              textAreaInput(
                
                "support_message",
                
                "Describe the Problem",
                
                placeholder =
                  "Type your message here",
                
                rows =
                  4,
                
                width =
                  "100%"
              ),
              
              
              actionButton(
                
                "send_support_button",
                
                "SEND SUPPORT REQUEST",
                
                class =
                  "navy-button"
              ),
              
              
              uiOutput(
                "support_status"
              )
            )
          )
        )
      }
      
      
      # ========================================================
      # INFO
      # ========================================================
      
      if (
        panel ==
        "info"
      ) {
        
        return(
          
          div(
            
            class =
              "card",
            
            h3(
              "GNIC-PT Information"
            ),
            
            div(
              
              class =
                "hidden-panel",
              
              p(
                
                paste0(
                  
                  "Model R-squared: ",
                  
                  percent(
                    model_r2,
                    accuracy =
                      0.1
                  ),
                  
                  "."
                )
              ),
              
              
              p(
                
                paste0(
                  
                  "Adjusted R-squared: ",
                  
                  percent(
                    model_adjusted_r2,
                    accuracy =
                      0.1
                  ),
                  
                  "."
                )
              ),
              
              
              p(
                
                paste0(
                  
                  "Regression sample size: ",
                  
                  model_n,
                  
                  " students."
                )
              ),
              
              
              p(
                
                paste0(
                  
                  "Estimated residual standard error: ",
                  
                  round(
                    model_rmse,
                    2
                  ),
                  
                  " C-NICAS points."
                )
              ),
              
              
              p(
                
                "GNIC-PT estimates a self-reported C-NICAS mean score on a 1 to 5 scale. It is a research prototype for educational support and should not replace tutor assessment, student feedback, or professional judgement."
              ),
              
              
              p(
                
                "The competence labels used by the application are practical interpretation bands and are not validated pass/fail thresholds."
              )
            )
          )
        )
      }
      
      
      # ========================================================
      # DEFAULT
      # ========================================================
      
      div(
        
        class =
          "card",
        
        p(
          
          style =
            "text-align:center; font-size:13px;",
          
          "Select a menu button to continue."
        )
      )
    })
  
  
  # ==========================================================
  # FAQ
  # ==========================================================
  
  faq_selected <-
    reactiveVal(
      NULL
    )
  
  
  observeEvent(
    input$faq_use_app,
    {
      faq_selected(
        "use_app"
      )
    }
  )
  
  
  observeEvent(
    input$faq_profile,
    {
      faq_selected(
        "profile"
      )
    }
  )
  
  
  observeEvent(
    input$faq_roster,
    {
      faq_selected(
        "roster"
      )
    }
  )
  
  
  observeEvent(
    input$faq_search,
    {
      faq_selected(
        "search"
      )
    }
  )
  
  
  observeEvent(
    input$faq_overview,
    {
      faq_selected(
        "overview"
      )
    }
  )
  
  
  observeEvent(
    input$faq_info,
    {
      faq_selected(
        "info"
      )
    }
  )
  
  
  observeEvent(
    input$faq_score,
    {
      faq_selected(
        "score"
      )
    }
  )
  
  
  observeEvent(
    input$faq_reset,
    {
      faq_selected(
        "reset"
      )
    }
  )
  
  
  observeEvent(
    input$faq_export,
    {
      faq_selected(
        "export"
      )
    }
  )
  
  
  output$faq_answer <-
    renderUI({
      
      selected <-
        faq_selected()
      
      
      if (
        is.null(selected)
      ) {
        
        return(NULL)
      }
      
      
      answer <-
        
        switch(
          
          selected,
          
          
          "use_app" =
            
            "Select Student Information and enter the student details. Then select Student Predictor Information and complete the predictor responses. The predicted C-NICAS competence score updates automatically.",
          
          
          "profile" =
            
            "PROFILE displays the current student's colour-coded predictor profile. Green indicates a strength, orange indicates a moderate area, and red identifies an area needing support.",
          
          
          "roster" =
            
            "ROSTER displays the student records available in the analysis dataset together with their C-NICAS scores and competence categories.",
          
          
          "search" =
            
            "SEARCH allows you to enter a Student Number and view that student's C-NICAS score and competence category.",
          
          
          "overview" =
            
            "OVERVIEW displays the distribution of competence categories and the group mean and standard deviation.",
          
          
          "info" =
            
            "INFO displays model performance information and important limitations of GNIC-PT.",
          
          
          "score" =
            
            "The predicted score changes because GNIC-PT recalculates the predicted C-NICAS score whenever a predictor response changes.",
          
          
          "reset" =
            
            "RESET clears the current student information and restores the default predictor responses.",
          
          
          "export" =
            
            "EXPORT downloads a CSV report containing the student details, predicted C-NICAS score, prediction range, competence category, and predictor responses."
        )
      
      
      div(
        
        class =
          "help-answer",
        
        paste0(
          "GNIC-PT ANSWER: ",
          answer
        )
      )
    })
  
  
  # ==========================================================
  # PROFILE GRAPH
  # ==========================================================
  
  output$strength_weakness_plot <-
    renderPlot({
      
      evaluation <-
        predictor_evaluation()
      
      
      ggplot(
        
        evaluation,
        
        aes(
          
          x =
            reorder(
              predictor,
              percentage
            ),
          
          y =
            percentage,
          
          fill =
            colour
        )
        
      ) +
        
        geom_col(
          
          width =
            0.65,
          
          colour =
            "white",
          
          linewidth =
            0.4,
          
          show.legend =
            FALSE
        ) +
        
        geom_text(
          
          aes(
            label =
              status
          ),
          
          hjust =
            -0.10,
          
          fontface =
            "bold",
          
          colour =
            "#12355B",
          
          size =
            4
        ) +
        
        coord_flip() +
        
        scale_fill_identity() +
        
        scale_y_continuous(
          
          limits =
            c(
              0,
              1.35
            ),
          
          labels =
            percent_format(
              accuracy =
                1
            )
        ) +
        
        labs(
          
          x =
            NULL,
          
          y =
            "Student Response Level (% of Maximum Value)"
        ) +
        
        theme_minimal(
          base_size =
            13
        ) +
        
        theme(
          
          panel.grid.minor =
            element_blank(),
          
          axis.text.y =
            element_text(
              face =
                "bold"
            )
        )
    })
  
  
  # ==========================================================
  # C-NICAS SCALE
  # ==========================================================
  
  output$prediction_plot <-
    renderPlot({
      
      result <-
        prediction_result()
      
      
      predicted_score <-
        
        max(
          
          1,
          
          min(
            5,
            result$predicted_score
          )
        )
      
      
      lower_score <-
        
        max(
          1,
          result$lower_prediction
        )
      
      
      upper_score <-
        
        min(
          5,
          result$upper_prediction
        )
      
      
      plot_data <-
        data.frame(
          
          predicted_score =
            predicted_score,
          
          lower_score =
            lower_score,
          
          upper_score =
            upper_score
        )
      
      
      ggplot(
        
        plot_data,
        
        aes(
          x =
            predicted_score,
          
          y =
            1
        )
        
      ) +
        
        annotate(
          
          "rect",
          
          xmin =
            1,
          
          xmax =
            2.5,
          
          ymin =
            0.6,
          
          ymax =
            1.4,
          
          fill =
            "#D55E00"
        ) +
        
        annotate(
          
          "rect",
          
          xmin =
            2.5,
          
          xmax =
            3.5,
          
          ymin =
            0.6,
          
          ymax =
            1.4,
          
          fill =
            "#E69F00"
        ) +
        
        annotate(
          
          "rect",
          
          xmin =
            3.5,
          
          xmax =
            4.25,
          
          ymin =
            0.6,
          
          ymax =
            1.4,
          
          fill =
            "#0072B2"
        ) +
        
        annotate(
          
          "rect",
          
          xmin =
            4.25,
          
          xmax =
            5,
          
          ymin =
            0.6,
          
          ymax =
            1.4,
          
          fill =
            "#009E73"
        ) +
        
        geom_vline(
          
          xintercept =
            c(
              2.5,
              3.5,
              4.25
            ),
          
          colour =
            "white",
          
          linewidth =
            1
        ) +
        
        geom_segment(
          
          aes(
            
            x =
              lower_score,
            
            xend =
              upper_score,
            
            y =
              1,
            
            yend =
              1
          ),
          
          colour =
            "white",
          
          linewidth =
            6,
          
          lineend =
            "round"
        ) +
        
        geom_point(
          
          colour =
            "black",
          
          size =
            6
        ) +
        
        annotate(
          
          "text",
          
          x =
            1.75,
          
          y =
            1.26,
          
          label =
            "LOWER",
          
          colour =
            "white",
          
          fontface =
            "bold",
          
          size =
            4
        ) +
        
        annotate(
          
          "text",
          
          x =
            3.00,
          
          y =
            1.26,
          
          label =
            "MODERATE",
          
          colour =
            "white",
          
          fontface =
            "bold",
          
          size =
            4
        ) +
        
        annotate(
          
          "text",
          
          x =
            3.88,
          
          y =
            1.26,
          
          label =
            "GOOD",
          
          colour =
            "white",
          
          fontface =
            "bold",
          
          size =
            4
        ) +
        
        annotate(
          
          "text",
          
          x =
            4.63,
          
          y =
            1.26,
          
          label =
            "HIGHER",
          
          colour =
            "white",
          
          fontface =
            "bold",
          
          size =
            4
        ) +
        
        geom_text(
          
          aes(
            
            x =
              predicted_score,
            
            y =
              0.76,
            
            label =
              paste0(
                "SCORE: ",
                round(
                  predicted_score,
                  2
                )
              )
          ),
          
          colour =
            "white",
          
          fontface =
            "bold",
          
          size =
            4
        ) +
        
        scale_x_continuous(
          
          limits =
            c(
              1,
              5
            ),
          
          breaks =
            seq(
              1,
              5,
              0.5
            )
        ) +
        
        scale_y_continuous(
          
          limits =
            c(
              0.6,
              1.4
            )
        ) +
        
        labs(
          
          x =
            "PREDICTED MEAN C-NICAS SCORE",
          
          y =
            NULL
        ) +
        
        theme_minimal(
          base_size =
            13
        ) +
        
        theme(
          
          panel.grid =
            element_blank(),
          
          axis.text.y =
            element_blank(),
          
          axis.ticks.y =
            element_blank(),
          
          axis.title.x =
            element_text(
              face =
                "bold"
            )
        )
    })
  
  
  # ==========================================================
  # ROSTER
  # ==========================================================
  
  output$roster_table <-
    renderDT({
      
      roster_data <-
        
        model_data %>%
        
        filter(
          !is.na(
            cnicas_mean
          )
        ) %>%
        
        mutate(
          
          Student_Number =
            row_number(),
          
          Competence_Level =
            case_when(
              
              cnicas_mean < 2.50 ~
                
                "Lower",
              
              cnicas_mean < 3.50 ~
                
                "Moderate",
              
              cnicas_mean < 4.25 ~
                
                "Good",
              
              TRUE ~
                
                "Higher"
            )
        ) %>%
        
        transmute(
          
          Student_Number,
          
          C_NICAS_Score =
            round(
              cnicas_mean,
              2
            ),
          
          Competence_Level
        )
      
      
      DT::datatable(
        
        roster_data,
        
        rownames =
          FALSE,
        
        filter =
          "top",
        
        options =
          list(
            
            pageLength =
              10,
            
            lengthMenu =
              c(
                5,
                10,
                25,
                50
              ),
            
            autoWidth =
              TRUE
          ),
        
        colnames =
          c(
            
            "Student Number",
            
            "C-NICAS Score",
            
            "Competence Level"
          )
      )
    })
  
  
  # ==========================================================
  # SEARCH
  # ==========================================================
  
  output$student_search_result <-
    renderUI({
      
      req(
        input$search_student_number
      )
      
      
      student_number <-
        as.integer(
          input$search_student_number
        )
      
      
      if (
        
        is.na(
          student_number
        ) ||
        
        student_number < 1 ||
        
        student_number >
        nrow(model_data)
        
      ) {
        
        return(
          
          div(
            
            style =
              "color:#D55E00; font-weight:bold; margin-top:10px;",
            
            "No student exists with that student number."
          )
        )
      }
      
      
      div(
        
        class =
          "hidden-panel",
        
        h4(
          paste(
            "Student",
            student_number
          )
        ),
        
        DTOutput(
          "search_table"
        )
      )
    })
  
  
  output$search_table <-
    renderDT({
      
      req(
        input$search_student_number
      )
      
      
      student_number <-
        as.integer(
          input$search_student_number
        )
      
      
      if (
        
        is.na(
          student_number
        ) ||
        
        student_number < 1 ||
        
        student_number >
        nrow(model_data)
        
      ) {
        
        return(NULL)
      }
      
      
      student_record <-
        
        model_data %>%
        
        slice(
          student_number
        ) %>%
        
        mutate(
          
          Student_Number =
            student_number,
          
          Competence_Level =
            case_when(
              
              cnicas_mean < 2.50 ~
                
                "Lower",
              
              cnicas_mean < 3.50 ~
                
                "Moderate",
              
              cnicas_mean < 4.25 ~
                
                "Good",
              
              TRUE ~
                
                "Higher"
            )
        ) %>%
        
        transmute(
          
          Student_Number,
          
          C_NICAS_Score =
            round(
              cnicas_mean,
              2
            ),
          
          Competence_Level
        )
      
      
      DT::datatable(
        
        student_record,
        
        rownames =
          FALSE,
        
        options =
          list(
            
            dom =
              "t",
            
            paging =
              FALSE,
            
            searching =
              FALSE
          ),
        
        colnames =
          c(
            
            "Student Number",
            
            "C-NICAS Score",
            
            "Competence Level"
          )
      )
    })
  
  
  # ==========================================================
  # GROUP OVERVIEW
  # ==========================================================
  
  output$group_competence_plot <-
    renderPlot({
      
      group_data <-
        
        model_data %>%
        
        filter(
          !is.na(
            cnicas_mean
          )
        ) %>%
        
        mutate(
          
          Competence_Level =
            case_when(
              
              cnicas_mean < 2.50 ~
                
                "Lower",
              
              cnicas_mean < 3.50 ~
                
                "Moderate",
              
              cnicas_mean < 4.25 ~
                
                "Good",
              
              TRUE ~
                
                "Higher"
            )
        ) %>%
        
        count(
          Competence_Level
        ) %>%
        
        mutate(
          
          Competence_Level =
            factor(
              
              Competence_Level,
              
              levels =
                c(
                  "Lower",
                  "Moderate",
                  "Good",
                  "Higher"
                )
            )
        )
      
      
      colour_values <-
        c(
          
          "Lower" =
            "#D55E00",
          
          "Moderate" =
            "#E69F00",
          
          "Good" =
            "#0072B2",
          
          "Higher" =
            "#009E73"
        )
      
      
      ggplot(
        
        group_data,
        
        aes(
          
          x =
            Competence_Level,
          
          y =
            n,
          
          fill =
            Competence_Level
        )
        
      ) +
        
        geom_col(
          
          width =
            0.70,
          
          colour =
            "white"
        ) +
        
        geom_text(
          
          aes(
            label =
              n
          ),
          
          vjust =
            -0.35,
          
          fontface =
            "bold",
          
          size =
            5
        ) +
        
        scale_fill_manual(
          
          values =
            colour_values
        ) +
        
        labs(
          
          x =
            "Competence Category",
          
          y =
            "Number of Students",
          
          title =
            "Distribution of Student C-NICAS Competence Scores"
        ) +
        
        theme_minimal(
          base_size =
            13
        ) +
        
        theme(
          
          legend.position =
            "none",
          
          plot.title =
            element_text(
              face =
                "bold",
              hjust =
                0.5
            ),
          
          panel.grid.minor =
            element_blank()
        )
    })
  
  
  # ==========================================================
  # GROUP SUMMARY
  # ==========================================================
  
  output$group_competence_summary <-
    renderUI({
      
      group_mean <-
        
        mean(
          
          regression_data$cnicas_mean,
          
          na.rm =
            TRUE
        )
      
      
      group_sd <-
        
        sd(
          
          regression_data$cnicas_mean,
          
          na.rm =
            TRUE
        )
      
      
      total_students <-
        
        sum(
          
          !is.na(
            regression_data$cnicas_mean
          )
        )
      
      
      div(
        
        style =
          "margin-top:15px; font-size:13px; font-weight:bold;",
        
        paste0(
          
          "GROUP SUMMARY: Number of Students = ",
          
          total_students,
          
          "; Mean C-NICAS Score = ",
          
          round(
            group_mean,
            2
          ),
          
          " / 5; Standard Deviation = ",
          
          round(
            group_sd,
            2
          ),
          
          "."
        )
      )
    })
  
  
  # ==========================================================
  # EXPORT CURRENT STUDENT REPORT
  # ==========================================================
  
  output$export_button <-
    
    downloadHandler(
      
      filename =
        function() {
          
          student_file_name <-
            
            ifelse(
              
              is.null(
                input$student_name
              ) ||
                
                input$student_name == "",
              
              "GNIC_PT_Student_Report",
              
              gsub(
                
                " ",
                
                "_",
                
                input$student_name
              )
            )
          
          
          paste0(
            
            student_file_name,
            
            "_GNIC_PT_Report.csv"
          )
        },
      
      
      content =
        function(file) {
          
          result <-
            prediction_result()
          
          
          predicted_score <-
            
            max(
              
              1,
              
              min(
                5,
                result$predicted_score
              )
            )
          
          
          competency_category <-
            
            case_when(
              
              predicted_score < 2.50 ~
                
                "Lower Predicted Competency",
              
              predicted_score < 3.50 ~
                
                "Moderate Predicted Competency",
              
              predicted_score < 4.25 ~
                
                "Good Predicted Competency",
              
              TRUE ~
                
                "Higher Predicted Competency"
            )
          
          
          student_report <-
            
            data.frame(
              
              Student_Full_Name =
                
                ifelse(
                  
                  is.null(
                    input$student_name
                  ) ||
                    
                    input$student_name == "",
                  
                  "Not Provided",
                  
                  input$student_name
                ),
              
              
              School_Name =
                
                ifelse(
                  
                  is.null(
                    input$school_name
                  ) ||
                    
                    input$school_name == "",
                  
                  "Not Provided",
                  
                  input$school_name
                ),
              
              
              Group =
                
                ifelse(
                  
                  is.null(
                    input$student_group
                  ) ||
                    
                    input$student_group == "",
                  
                  "Not Provided",
                  
                  input$student_group
                ),
              
              
              Predicted_C_NICAS_Score =
                
                round(
                  predicted_score,
                  2
                ),
              
              
              Prediction_Lower_Range =
                
                round(
                  
                  max(
                    1,
                    result$lower_prediction
                  ),
                  
                  2
                ),
              
              
              Prediction_Upper_Range =
                
                round(
                  
                  min(
                    5,
                    result$upper_prediction
                  ),
                  
                  2
                ),
              
              
              Competence_Level =
                
                competency_category,
              
              
              Academic_Device_Use =
                
                input$academic_device_use_score,
              
              
              Computer_Confidence =
                
                input$computer_confidence_score,
              
              
              Internet_Access =
                
                input$internet_access_yes,
              
              
              Formal_Computer_Training =
                
                input$formal_computer_training_yes,
              
              
              EHR_Exposure =
                
                input$ehr_exposure_level_score,
              
              
              Nurse_Tutor_Informatics =
                
                input$informatics_nurse_tutor_yes,
              
              
              Online_Learning_Exposure =
                
                input$online_learning_yes,
              
              
              stringsAsFactors =
                FALSE
            )
          
          
          write.csv(
            
            student_report,
            
            file,
            
            row.names =
              FALSE
          )
        }
    )
  
  
  # ==========================================================
  # SUPPORT REQUEST
  # ==========================================================
  
  observeEvent(
    
    input$send_support_button,
    
    {
      
      if (
        
        is.null(
          input$support_message
        ) ||
        
        trimws(
          input$support_message
        ) == ""
        
      ) {
        
        output$support_status <-
          renderUI({
            
            div(
              
              style =
                "color:#D55E00; font-weight:bold; margin-top:10px;",
              
              "Please type a support message before sending."
            )
          })
        
        return()
      }
      
      
      student_reference <-
        
        ifelse(
          
          is.null(
            input$student_name
          ) ||
            
            input$student_name == "",
          
          "Not provided",
          
          input$student_name
        )
      
      
      email_subject <-
        
        utils::URLencode(
          
          "GNIC-PT Support Request"
        )
      
      
      email_body <-
        
        utils::URLencode(
          
          paste0(
            
            "Student reference: ",
            
            student_reference,
            
            "\n\nSupport request:\n",
            
            input$support_message
          ),
          
          reserved =
            TRUE
        )
      
      
      support_link <-
        
        paste0(
          
          "mailto:",
          
          developer_email,
          
          "?subject=",
          
          email_subject,
          
          "&body=",
          
          email_body
        )
      
      
      output$support_status <-
        renderUI({
          
          tagList(
            
            div(
              
              style =
                "background-color:#E8F5E9; border-left:5px solid #009E73; padding:12px; border-radius:5px; margin-top:12px; font-weight:bold;",
              
              "Your support request is ready."
            ),
            
            
            tags$a(
              
              href =
                support_link,
              
              class =
                "btn navy-button",
              
              "SEND NOW"
            )
          )
        })
    }
  )
}


# ============================================================
# START THE GNIC-PT APPLICATION
# ============================================================

shinyApp(
  
  ui =
    ui,
  
  server =
    server
)

