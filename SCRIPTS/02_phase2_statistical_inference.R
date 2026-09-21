# IT3081 Hospital Readmission Consultancy
# Phase 2: Statistical inference (Task 4)
# Run after 01_phase1_descriptive_analysis.R.

required <- c("dplyr", "readr", "tidyr")
missing_packages <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}
still_missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(still_missing) > 0) {
  stop("Package installation failed: ", paste(still_missing, collapse = ", "))
}
library(dplyr)
library(readr)
library(tidyr)

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
data_dir <- file.path(root, "DATA")
report_dir <- file.path(root, "REPORTS", "INDIVIDUAL_SECTIONS")
if (!file.exists(file.path(data_dir, "readmission_cleaned.csv"))) {
  stop("Run 01_phase1_descriptive_analysis.R first.")
}
df <- read_csv(file.path(data_dir, "readmission_cleaned.csv"), show_col_types = FALSE) %>%
  mutate(
    readmitted = factor(readmitted, levels = c("No", "Yes")),
    age_numeric = as.numeric(sub("[^0-9]*([0-9]+).*", "\\1", age)),
    discharge_disposition_id = factor(discharge_disposition_id)
  )

fmt_sig <- function(p) ifelse(p < 0.05, "Yes (p < 0.05)", "No")
cohens_d <- function(x, y) (mean(x, na.rm = TRUE) - mean(y, na.rm = TRUE)) /
  sqrt((var(x, na.rm = TRUE) + var(y, na.rm = TRUE)) / 2)
cramers_v <- function(tab) {
  chi <- suppressWarnings(chisq.test(tab, correct = FALSE))
  sqrt(as.numeric(chi$statistic) / (sum(tab) * min(nrow(tab) - 1, ncol(tab) - 1)))
}

# Test 1: age and readmission
age_data <- df %>% filter(!is.na(age_numeric), !is.na(readmitted))
age_yes <- age_data$age_numeric[age_data$readmitted == "Yes"]
age_no <- age_data$age_numeric[age_data$readmitted == "No"]
age_test <- t.test(age_yes, age_no)
age_d <- cohens_d(age_yes, age_no)
result_age <- tibble(
  Test = "Age vs Readmission", Test_Type = "Welch independent t-test",
  Statistic = unname(age_test$statistic), p_value = age_test$p.value,
  Significant = fmt_sig(age_test$p.value), Effect_Size = age_d,
  Mean_Difference = mean(age_yes) - mean(age_no),
  Clinical_Implication = "Use age-stratified discharge planning; association is not causal"
)

# Test 2: gender and readmission
 gender_data <- df %>% filter(!is.na(gender), !is.na(readmitted))
gender_table <- table(gender_data$gender, gender_data$readmitted)
gender_test <- chisq.test(gender_table)
result_gender <- tibble(
  Test = "Gender vs Readmission", Test_Type = "Chi-square test of independence",
  Statistic = unname(gender_test$statistic), p_value = gender_test$p.value,
  Significant = fmt_sig(gender_test$p.value), Effect_Size = cramers_v(gender_table),
  Mean_Difference = NA_real_, Clinical_Implication = "Monitor subgroup performance and investigate access or care explanations"
)

# Test 3: length of stay and readmission
los_data <- df %>% filter(!is.na(time_in_hospital), !is.na(readmitted))
los_yes <- los_data$time_in_hospital[los_data$readmitted == "Yes"]
los_no <- los_data$time_in_hospital[los_data$readmitted == "No"]
los_test <- t.test(los_yes, los_no)
los_d <- cohens_d(los_yes, los_no)
result_los <- tibble(
  Test = "Length of Stay vs Readmission", Test_Type = "Welch independent t-test",
  Statistic = unname(los_test$statistic), p_value = los_test$p.value,
  Significant = fmt_sig(los_test$p.value), Effect_Size = los_d,
  Mean_Difference = mean(los_yes) - mean(los_no),
  Clinical_Implication = "Use LOS as a complexity marker alongside clinical context"
)

# Test 4: medication count and readmission
med_data <- df %>% filter(!is.na(num_medications), !is.na(readmitted))
med_yes <- med_data$num_medications[med_data$readmitted == "Yes"]
med_no <- med_data$num_medications[med_data$readmitted == "No"]
med_test <- t.test(med_yes, med_no)
med_d <- cohens_d(med_yes, med_no)
result_med <- tibble(
  Test = "Medications vs Readmission", Test_Type = "Welch independent t-test",
  Statistic = unname(med_test$statistic), p_value = med_test$p.value,
  Significant = fmt_sig(med_test$p.value), Effect_Size = med_d,
  Mean_Difference = mean(med_yes) - mean(med_no),
  Clinical_Implication = "Prioritise medication reconciliation for polypharmacy patients"
)

# Test 5: discharge disposition and length of stay
anova_data <- df %>% filter(!is.na(discharge_disposition_id), !is.na(time_in_hospital))
anova_model <- aov(time_in_hospital ~ discharge_disposition_id, data = anova_data)
anova_table <- summary(anova_model)[[1]]
anova_f <- anova_table$`F value`[1]
anova_p <- anova_table$`Pr(>F)`[1]
ss_between <- anova_table$`Sum Sq`[1]
ss_total <- sum(anova_table$`Sum Sq`)
result_anova <- tibble(
  Test = "Discharge Disposition vs Length of Stay", Test_Type = "One-way ANOVA",
  Statistic = anova_f, p_value = anova_p, Significant = fmt_sig(anova_p),
  Effect_Size = ss_between / ss_total, Mean_Difference = NA_real_,
  Clinical_Implication = "Review disposition-specific discharge pathways and post-acute support"
)

# Test 6: diabetic medication indicator and readmission
# The raw dataset represents this as diabetesMed (Yes/No), not diabetic_medications.
diab_data <- df %>% filter(!is.na(diabetesMed), !is.na(readmitted))
diab_table <- table(diab_data$diabetesMed, diab_data$readmitted)
diab_test <- chisq.test(diab_table)
result_diab <- tibble(
  Test = "Diabetes Medication Indicator vs Readmission", Test_Type = "Chi-square test of independence",
  Statistic = unname(diab_test$statistic), p_value = diab_test$p.value,
  Significant = fmt_sig(diab_test$p.value), Effect_Size = cramers_v(diab_table),
  Mean_Difference = NA_real_, Clinical_Implication = "Interpret medication status as a clinical complexity marker, not a treatment effect"
)

results <- bind_rows(result_age, result_gender, result_los, result_med, result_anova, result_diab) %>%
  mutate(across(c(Statistic, p_value, Effect_Size, Mean_Difference), ~ round(.x, 4)))
write.csv(results, file.path(report_dir, "statistical_test_results.csv"), row.names = FALSE)

cat("Phase 2 complete. Results written to:\n", file.path(report_dir, "statistical_test_results.csv"), "\n")
print(results)
