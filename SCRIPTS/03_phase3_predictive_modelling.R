# IT3081 Hospital Readmission Consultancy
# Phase 3: Predictive statistical modelling (Task 5)
# Run after 01_phase1_descriptive_analysis.R and 02_phase2_statistical_inference.R.

required <- c("dplyr", "readr", "tidyr", "ggplot2", "glmnet", "pROC", "forcats")
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
library(ggplot2)
library(glmnet)
library(pROC)

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
data_dir <- file.path(root, "DATA")
visual_dir <- file.path(root, "VISUALIZATIONS", "MODELING")
report_dir <- file.path(root, "REPORTS", "INDIVIDUAL_SECTIONS")
dir.create(visual_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

input_path <- file.path(data_dir, "readmission_cleaned.csv")
if (!file.exists(input_path)) stop("Run 01_phase1_descriptive_analysis.R first.")
df <- read_csv(input_path, show_col_types = FALSE) %>%
  mutate(
    readmitted = factor(readmitted, levels = c("No", "Yes")),
    readmitted_binary = as.integer(readmitted == "Yes"),
    age_numeric = as.numeric(sub("[^0-9]*([0-9]+).*", "\\1", age)),
    gender = factor(gender),
    race = factor(race),
    discharge_disposition_id = factor(discharge_disposition_id),
    diabetesMed = factor(diabetesMed),
    metformin = factor(metformin),
    insulin = factor(insulin)
  )

# Use source column names and preserve categorical predictors as factors.
features <- c(
  "age_numeric", "gender", "race", "time_in_hospital", "num_medications",
  "num_lab_procedures", "num_procedures", "number_inpatient", "number_outpatient",
  "number_emergency", "diabetesMed", "metformin", "insulin", "discharge_disposition_id"
)
model_df <- df %>% select(all_of(c(features, "readmitted_binary"))) %>% drop_na()

set.seed(42)
train_index <- unlist(lapply(split(seq_len(nrow(model_df)), model_df$readmitted_binary), function(index) {
  sample(index, size = floor(.70 * length(index)))
}))
train <- model_df[train_index, ]
test <- model_df[-train_index, ]

# model.matrix creates a consistent numeric design matrix for glmnet.
formula_text <- paste("readmitted_binary ~", paste(features, collapse = " + "))
model_formula <- as.formula(formula_text)
train_matrix <- model.matrix(model_formula, data = train)[, -1, drop = FALSE]
test_matrix <- model.matrix(model_formula, data = test)[, -1, drop = FALSE]
missing_columns <- setdiff(colnames(train_matrix), colnames(test_matrix))
if (length(missing_columns) > 0) {
  for (column in missing_columns) test_matrix <- cbind(test_matrix, setNames(data.frame(0), column))
}
test_matrix <- test_matrix[, colnames(train_matrix), drop = FALSE]
y_train <- train$readmitted_binary
y_test <- test$readmitted_binary

# Standardise using training-set means and standard deviations only. This is
# important for penalised regression because otherwise large-scale variables
# receive disproportionate shrinkage.
train_center <- colMeans(train_matrix)
train_scale <- apply(train_matrix, 2, sd)
train_scale[is.na(train_scale) | train_scale == 0] <- 1
train_matrix_scaled <- scale(train_matrix, center = train_center, scale = train_scale)
test_matrix_scaled <- scale(test_matrix, center = train_center, scale = train_scale)

threshold_from_training_roc <- function(actual, probabilities) {
  roc_obj <- pROC::roc(actual, probabilities, quiet = TRUE)
  as.numeric(pROC::coords(roc_obj, "best", best.method = "youden", ret = "threshold"))
}

safe_metrics <- function(actual, predicted, probabilities) {
  cm <- table(factor(actual, levels = c(0, 1)), factor(predicted, levels = c(0, 1)))
  specificity <- cm[1, 1] / sum(cm[1, ])
  sensitivity <- cm[2, 2] / sum(cm[2, ])
  list(
    accuracy = mean(actual == predicted), sensitivity = sensitivity,
    specificity = specificity, auc = as.numeric(pROC::auc(actual, probabilities)), confusion = cm
  )
}

# Logistic regression
logit_model <- glm(model_formula, data = train, family = binomial())
logit_prob <- predict(logit_model, newdata = test, type = "response")
logit_train_prob <- predict(logit_model, newdata = train, type = "response")
logit_threshold <- threshold_from_training_roc(y_train, logit_train_prob)
logit_class <- as.integer(logit_prob >= logit_threshold)
logit_metrics <- safe_metrics(y_test, logit_class, logit_prob)

# Ridge and LASSO with five-fold cross-validation on the training set
ridge_cv <- cv.glmnet(train_matrix_scaled, y_train, family = "binomial", alpha = 0,
                      nfolds = 5, type.measure = "auc", standardize = FALSE,
                      control = list(maxit = 1000000))
lasso_cv <- cv.glmnet(train_matrix_scaled, y_train, family = "binomial", alpha = 1,
                      nfolds = 5, type.measure = "auc", standardize = FALSE,
                      control = list(maxit = 1000000))
ridge_model <- glmnet(train_matrix_scaled, y_train, family = "binomial", alpha = 0,
                      lambda = ridge_cv$lambda.1se, standardize = FALSE,
                      control = list(maxit = 1000000))
lasso_model <- glmnet(train_matrix_scaled, y_train, family = "binomial", alpha = 1,
                      lambda = lasso_cv$lambda.1se, standardize = FALSE,
                      control = list(maxit = 1000000))

ridge_prob <- as.numeric(predict(ridge_model, newx = test_matrix_scaled, type = "response"))
lasso_prob <- as.numeric(predict(lasso_model, newx = test_matrix_scaled, type = "response"))
ridge_train_prob <- as.numeric(predict(ridge_model, newx = train_matrix_scaled, type = "response"))
lasso_train_prob <- as.numeric(predict(lasso_model, newx = train_matrix_scaled, type = "response"))
ridge_threshold <- threshold_from_training_roc(y_train, ridge_train_prob)
lasso_threshold <- threshold_from_training_roc(y_train, lasso_train_prob)
ridge_class <- as.integer(ridge_prob >= ridge_threshold)
lasso_class <- as.integer(lasso_prob >= lasso_threshold)
ridge_metrics <- safe_metrics(y_test, ridge_class, ridge_prob)
lasso_metrics <- safe_metrics(y_test, lasso_class, lasso_prob)

# Separate ROC plots
roc_specs <- list(
  list(name = "01_logit_roc_curve.png", title = "Logistic Regression - ROC Curve", prob = logit_prob, color = "#d62728"),
  list(name = "02_ridge_roc_curve.png", title = "Ridge Regression - ROC Curve", prob = ridge_prob, color = "#2ca02c"),
  list(name = "03_lasso_roc_curve.png", title = "LASSO Regression - ROC Curve", prob = lasso_prob, color = "#1f77b4")
)
for (spec in roc_specs) {
  roc_obj <- roc(y_test, spec$prob, quiet = TRUE)
  png(file.path(visual_dir, spec$name), width = 800, height = 600, res = 120)
  plot(roc_obj, main = spec$title, col = spec$color, lwd = 3,
       xlab = "False positive rate", ylab = "True positive rate")
  legend("bottomright", legend = sprintf("AUC = %.4f", as.numeric(auc(roc_obj))), bty = "n")
  grid()
  dev.off()
}

comparison <- tibble(
  Model = c("Logistic Regression", "Ridge Regression", "LASSO Regression"),
  Accuracy = c(logit_metrics$accuracy, ridge_metrics$accuracy, lasso_metrics$accuracy),
  Sensitivity = c(logit_metrics$sensitivity, ridge_metrics$sensitivity, lasso_metrics$sensitivity),
  Specificity = c(logit_metrics$specificity, ridge_metrics$specificity, lasso_metrics$specificity),
  AUC = c(logit_metrics$auc, ridge_metrics$auc, lasso_metrics$auc),
  Threshold = c(logit_threshold, ridge_threshold, lasso_threshold),
  Strengths = c("Interpretable coefficients", "Handles correlated predictors", "Selects a sparse feature set"),
  Limitations = c("Linear log-odds assumption", "Retains all predictors", "Selection can be unstable with correlated predictors")
) %>% mutate(across(c(Accuracy, Sensitivity, Specificity, AUC, Threshold), ~ round(.x, 4)))
write.csv(comparison, file.path(report_dir, "model_comparison.csv"), row.names = FALSE)

p <- comparison %>% select(Model, Accuracy, Sensitivity, Specificity, AUC) %>%
  pivot_longer(-Model, names_to = "Metric", values_to = "Score") %>%
  ggplot(aes(Metric, Score, fill = Model)) + geom_col(position = "dodge", color = "black") +
  geom_text(aes(label = sprintf("%.3f", Score)), position = position_dodge(.9), vjust = -.25, size = 3.5) +
  scale_fill_manual(values = c("Logistic Regression" = "#d62728", "Ridge Regression" = "#2ca02c", "LASSO Regression" = "#1f77b4")) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "Predictive Model Performance Comparison", x = "Metric", y = "Score", fill = "Model") +
  theme_minimal(base_size = 12) + theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
ggsave(file.path(visual_dir, "04_model_performance_comparison.png"), p, width = 10, height = 6, dpi = 150)

# Logistic coefficients and odds ratios for interpretation.
coefs <- coef(logit_model)
feature_importance <- tibble(
  Feature = names(coefs)[-1],
  Coefficient = as.numeric(coefs[-1]),
  Odds_Ratio = exp(as.numeric(coefs[-1]))
) %>% mutate(Direction = ifelse(Coefficient >= 0, "Increases readmission risk", "Decreases readmission risk")) %>%
  arrange(desc(abs(Coefficient)))
write.csv(feature_importance, file.path(report_dir, "feature_importance.csv"), row.names = FALSE)

p <- feature_importance %>% slice_head(n = 15) %>%
  mutate(Feature = forcats::fct_reorder(Feature, abs(Coefficient))) %>%
  ggplot(aes(abs(Coefficient), Feature, fill = Direction)) + geom_col(color = "black") +
  scale_fill_manual(values = c("Increases readmission risk" = "#d62728", "Decreases readmission risk" = "#2ca02c")) +
  labs(title = "Feature Importance in Logistic Regression", x = "Absolute coefficient", y = "Feature", fill = "Direction") +
  theme_minimal(base_size = 12) + theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
ggsave(file.path(visual_dir, "05_feature_importance.png"), p, width = 9, height = 6, dpi = 150)

cat("Phase 3 complete. Outputs written to:\n", visual_dir, "\n", report_dir, "\n")
print(comparison)
