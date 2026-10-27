# An Evidence-Based and Predictive Curriculum Enhancement Framework for Informatics Education among Pre-Service Nursing Students in Zimbabwe

R code for the quantitative analysis in my MSc Health Informatics and Medical Analytics dissertation. The study measured nursing informatics competency with the **C-NICAS** scale (26 items), looked at which digital access and exposure factors are linked to it, built and internally validated a predictive model, and deployed the model as **GNIC-PT**, an interactive Shiny app.

## Study objectives covered by this code

1. **Objective 1:** describe students' level of informatics competency (C-NICAS scores and reliability)
2. **Objective 2:** test relationships between candidate predictors and competency (Spearman correlations, t-tests, ANOVA with Tukey post-hoc tests)
3. **Objective 3:** develop and internally validate a predictive model (multiple linear regression, diagnostics, VIF, ten-fold cross-validation)
4. **Objective 4:** deploy the model as the GNIC-PT Shiny app

## Repository structure

| Path | Contents |
|---|---|
| `FINAL_ANALYSIS_CNICAS.R` | Full analysis pipeline in 25 chunks, from data import to final figures |
| `output/` | Summary tables exported by the script (descriptives, reliability, correlations, t-tests, regression, cross-validation, VIF) |
| `figures/` | Figures used in Chapter 4 and the appendices |
| `app/app.R` | GNIC-PT Shiny app |
| `FINAL DATA ANALYSIS 1 OF 3.Rproj` | RStudio project file |

## Analysis workflow (`FINAL_ANALYSIS_CNICAS.R`)

| Chunks | Step |
|---|---|
| 1–2 | Load packages, create folders, import and inspect data |
| 3–6 | Clean text values, assess missingness, drop variables with extensive missingness, rename variables |
| 7–11 | Identify the 26 C-NICAS items, convert responses to numbers, compute total/mean scores, reliability (Cronbach's alpha) |
| 12–14 | Code binary and ordered predictors; descriptive statistics and frequencies |
| 15–17 | Spearman correlations, independent-samples t-tests, ANOVA and Tukey post-hoc tests |
| 18–21 | Multiple linear regression, diagnostics, exported results, ten-fold cross-validation |
| 22–25 | Figures for Chapter 4 and the appendices, VIF table |

## Data availability

The raw survey data (`data/mydata.csv`) and any participant-level files are **not included**, in line with the study's ethical approval and participant confidentiality. Only aggregated outputs are shared.

## How to run

1. Open `FINAL DATA ANALYSIS 1 OF 3.Rproj` in RStudio.
2. Put the survey file at `data/mydata.csv`.
3. Edit the `setwd()` line at the top of the script to your own folder (or remove it when using the .Rproj).
4. Run `FINAL_ANALYSIS_CNICAS.R` from top to bottom.

The app needs the model file the script produces, plus the data copied into `app/DATA/`. Run it with `shiny::runApp("app")`.

## Author

Dolce Mudzanire, MSc Health Informatics and Medical Analytics (supervisor: Dr Turugari), 2026
