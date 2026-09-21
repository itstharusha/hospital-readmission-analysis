# IT3081 Hospital Readmission Consultancy
# Phase 1: Data preparation and descriptive analysis (Task 3)
# Run this script from the project root in RStudio.

required <- c("tidyverse", "psych", "corrplot", "scales")
missing_packages <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}
still_missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(still_missing) > 0) {
  stop("Package installation failed: ", paste(still_missing, collapse = ", "))
}
library(tidyverse)
library(psych)
library(corrplot)
library(scales)

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
data_dir <- file.path(root, "DATA")
visual_dir <- file.path(root, "VISUALIZATIONS", "DESCRIPTIVE")
report_dir <- file.path(root, "REPORTS", "INDIVIDUAL_SECTIONS")
dir.create(visual_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

raw_path <- file.path(data_dir, "readmission_raw.csv")
if (!file.exists(raw_path)) stop("Raw data not found: ", raw_path)

df <- read.csv(raw_path, na.strings = "?", stringsAsFactors = FALSE, check.names = FALSE)
df$readmitted_original <- df$readmitted
df$readmitted <- ifelse(df$readmitted == "<30", "Yes", "No")
df$readmitted <- factor(df$readmitted, levels = c("No", "Yes"))

# Missingness is reported using the source values; the complete cohort is retained
# because weight, payer, and specialty are sparsely recorded in this dataset.
missing_analysis <- tibble(
  variable = names(df),
  missing_count = vapply(df, function(x) sum(is.na(x)), integer(1))
) %>%
  mutate(missing_percent = round(100 * missing_count / nrow(df), 2)) %>%
  filter(missing_count > 0) %>%
  arrange(desc(missing_percent))
write.csv(missing_analysis, file.path(data_dir, "missing_data_analysis.csv"), row.names = FALSE)
write.csv(df, file.path(data_dir, "readmission_cleaned.csv"), row.names = FALSE)

plot_theme <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 15),
        plot.subtitle = element_text(size = 11),
        legend.position = "bottom")
status_colors <- c("No" = "#2ca02c", "Yes" = "#d62728")

# 1. Outcome distribution
outcome_dist <- df %>% count(readmitted) %>% mutate(percentage = 100 * n / sum(n))
p <- ggplot(outcome_dist, aes(x = readmitted, y = percentage, fill = readmitted)) +
  geom_col(color = "black", alpha = .85) +
  geom_text(aes(label = sprintf("%.1f%%\n(n=%s)", percentage, scales::comma(n))),
            vjust = -.25, fontface = "bold") +
  scale_x_discrete(labels = c("No" = "Not readmitted", "Yes" = "Readmitted <30 days")) +
  scale_fill_manual(values = status_colors) +
  labs(title = "Hospital Readmission Distribution", subtitle = "Outcome distribution in 101,766 discharge records",
       x = "Readmission status", y = "Percentage", fill = "Readmitted") +
  ylim(0, max(outcome_dist$percentage) * 1.18) + plot_theme
ggsave(file.path(visual_dir, "01_readmission_distribution.png"), p, width = 9, height = 6, dpi = 150)

# 2. Age distribution
age_levels <- unique(na.omit(df$age))
age_levels <- age_levels[order(as.numeric(gsub("[^0-9].*", "", age_levels)))]
p <- ggplot(df, aes(x = factor(age, levels = age_levels), fill = readmitted)) +
  geom_bar(position = "dodge", color = "black", alpha = .8) +
  scale_fill_manual(values = status_colors) +
  labs(title = "Age Distribution by Readmission Status", subtitle = "Age bands across the two outcome groups",
       x = "Age group", y = "Frequency", fill = "Readmitted") +
  plot_theme + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(visual_dir, "02_age_by_readmission.png"), p, width = 9, height = 6, dpi = 150)

# 3. Gender distribution
p <- df %>% filter(!is.na(gender)) %>%
  ggplot(aes(x = gender, fill = readmitted)) + geom_bar(position = "fill", color = "black") +
  scale_fill_manual(values = status_colors) + scale_y_continuous(labels = percent) +
  labs(title = "Readmission Rate by Gender", x = "Gender", y = "Proportion", fill = "Readmitted") + plot_theme
ggsave(file.path(visual_dir, "03_gender_by_readmission.png"), p, width = 8, height = 6, dpi = 150)

# 4. Equity analysis by race
p <- df %>% filter(!is.na(race), race != "?") %>%
  ggplot(aes(x = race, fill = readmitted)) + geom_bar(position = "fill", color = "black") +
  scale_fill_manual(values = status_colors) + scale_y_continuous(labels = percent) +
  labs(title = "Readmission Rate by Race/Ethnicity", subtitle = "Descriptive equity review; differences are not causal evidence",
       x = "Race/ethnicity", y = "Proportion", fill = "Readmitted") + plot_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(visual_dir, "04_race_by_readmission_EQUITY.png"), p, width = 10, height = 6, dpi = 150)

# 5. Length of stay
p <- ggplot(df, aes(x = time_in_hospital, fill = readmitted)) +
  geom_histogram(position = "dodge", bins = 14, color = "black", alpha = .8) +
  scale_fill_manual(values = status_colors) +
  labs(title = "Length of Hospital Stay by Readmission Status", x = "Days in hospital", y = "Frequency", fill = "Readmitted") + plot_theme
ggsave(file.path(visual_dir, "05_los_by_readmission.png"), p, width = 9, height = 6, dpi = 150)

# 6. Medication count
p <- ggplot(df, aes(x = num_medications, fill = readmitted)) +
  geom_histogram(position = "dodge", bins = 20, color = "black", alpha = .8) +
  scale_fill_manual(values = status_colors) +
  labs(title = "Number of Medications by Readmission Status", x = "Number of medications", y = "Frequency", fill = "Readmitted") + plot_theme
ggsave(file.path(visual_dir, "06_medications_by_readmission.png"), p, width = 9, height = 6, dpi = 150)

# 7. Prior utilisation: source columns are number_inpatient/outpatient/emergency
util <- df %>% select(readmitted, number_inpatient, number_outpatient, number_emergency) %>%
  pivot_longer(-readmitted, names_to = "visit_type", values_to = "visits") %>%
  mutate(visit_type = recode(visit_type, number_inpatient = "Inpatient", number_outpatient = "Outpatient", number_emergency = "Emergency"))
p <- ggplot(util, aes(x = visits, fill = readmitted)) +
  geom_histogram(position = "dodge", bins = 15, color = "black", alpha = .8) +
  facet_wrap(~visit_type, scales = "free") + scale_fill_manual(values = status_colors) +
  labs(title = "Prior Year Healthcare Utilisation by Readmission Status", x = "Number of visits", y = "Frequency", fill = "Readmitted") + plot_theme
 ggsave(file.path(visual_dir, "07_prior_utilization.png"), p, width = 10, height = 6, dpi = 150)

# 8. Numeric outliers
numeric_cols <- c("time_in_hospital", "num_lab_procedures", "num_procedures", "num_medications",
                  "number_outpatient", "number_emergency", "number_inpatient", "number_diagnoses")
long_numeric <- df %>% select(readmitted, all_of(numeric_cols)) %>% pivot_longer(-readmitted)
p <- ggplot(long_numeric, aes(x = readmitted, y = value, fill = readmitted)) +
  geom_boxplot(alpha = .8, color = "black") + facet_wrap(~name, scales = "free", ncol = 4) +
  scale_fill_manual(values = status_colors) +
  labs(title = "Outlier Detection: Numeric Variables by Readmission Status", x = "Readmitted", y = "Value", fill = "Readmitted") + plot_theme
 ggsave(file.path(visual_dir, "08_outlier_detection_boxplots.png"), p, width = 12, height = 8, dpi = 150)

# 9. Correlation matrix
corr_matrix <- cor(df[numeric_cols], use = "pairwise.complete.obs")
png(file.path(visual_dir, "09_correlation_matrix.png"), width = 900, height = 900, res = 150)
corrplot(corr_matrix, method = "circle", type = "upper", addCoef.col = "black", number.cex = .65,
         tl.cex = .8, title = "Correlation Matrix: Numeric Variables", mar = c(0, 0, 2, 0))
dev.off()

# 10. Missing-data pattern
missing_plot <- missing_analysis %>% slice_head(n = 15) %>% arrange(missing_percent)
p <- ggplot(missing_plot, aes(x = missing_percent, y = reorder(variable, missing_percent))) +
  geom_col(fill = "#4c78a8", color = "black") +
  labs(title = "Missing Data by Variable", x = "Missing values (%)", y = "Variable") + plot_theme
ggsave(file.path(visual_dir, "10_missing_data_pattern.png"), p, width = 10, height = 6, dpi = 150)

# Summary statistics for numeric clinical/utilisation variables
desc_stats <- psych::describe(df[numeric_cols]) %>% as.data.frame() %>%
  rownames_to_column("variable") %>%
  select(variable, n, mean, sd, median, min, max, skew, kurtosis) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))
write.csv(desc_stats, file.path(report_dir, "data_summary_statistics.csv"), row.names = FALSE)

insights <- tribble(
  ~Insight_Number, ~Insight, ~Organization_Implication, ~Actionable_Recommendation,
  1, sprintf("Class imbalance: %.1f%% readmitted within 30 days", mean(df$readmitted == "Yes") * 100), "Rare-event prediction requires stratification and appropriate metrics", "Use stratified validation and prioritise sensitivity and calibration",
  2, "Age groups show different outcome patterns", "Risk may vary across age strata", "Assess age-stratified discharge support and subgroup calibration",
  3, "Gender-specific rates can be compared descriptively", "Differences require clinical and access-related review", "Monitor subgroup performance without causal interpretation",
  4, "Race/ethnicity patterns require equity review", "Differences may reflect access or care disparities", "Audit model performance and intervention access across groups",
  5, "Medication burden marks clinical complexity", "Polypharmacy increases reconciliation and adherence challenges", "Prioritise medication reconciliation for high medication counts",
  6, "Prior utilisation identifies frequent users", "Recent utilisation supports risk stratification", "Target high-utilisation patients for coordinated follow-up",
  7, "Discharge disposition reflects different care pathways", "Post-acute support needs vary by destination", "Include disposition in risk review and coordinate handoffs",
  8, "Length of stay spans admissions of varying complexity", "LOS alone is not a causal explanation", "Use LOS alongside clinical and social context"
)
write.csv(insights, file.path(report_dir, "business_insights_task3.csv"), row.names = FALSE)

cat("Phase 1 complete. Outputs written to:\n", visual_dir, "\n", report_dir, "\n")
 This town is full I think it's a I don't know. I don't know. I don't know. I don't know. I don't know if it's a good idea, but I wanted to know what was going on here. I just wanted to know what was going on here. This town is full I think it's a There is a lot of tension in the country, and so if you don't have a place to live, you can be in a situation where it becomes more difficult. This town is full I think it's a The correct answer is no. This town is full I think it's a The This town is full I think it's a To the north of the capital, there are a few people who are not as well represented in the media as the guys who are in the front row. This town is full I think it's a It is the first time in the history of the US that we have a national park. This town is full I think it's a The first one is the one that was used in the first one. This town is full I think it's a I don't know. one is the one that was used in the first one, and that was the one that was used in The The first one is the one that was used in the movie.