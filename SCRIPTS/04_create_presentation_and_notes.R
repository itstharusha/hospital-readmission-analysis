# IT3081 Hospital Readmission Consultancy
# Create the 15-slide presentation, speaker notes, and viva preparation documents.

required <- c("officer", "rvg")
missing_packages <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}
still_missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(still_missing) > 0) stop("Package installation failed: ", paste(still_missing, collapse = ", "))
library(officer)

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
presentation_dir <- file.path(root, "PRESENTATIONS")
visual_dir <- file.path(root, "VISUALIZATIONS")
dir.create(presentation_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(presentation_dir, "VIVA_PREP"), recursive = TRUE, showWarnings = FALSE)

add_title_content <- function(doc, title, content) {
  doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
  doc <- ph_with(doc, title, location = ph_location_type(type = "title"))
  doc <- ph_with(doc, paste(content, collapse = "\n"), location = ph_location_type(type = "body"))
  doc
}

add_image_slide <- function(doc, title, image_path) {
  doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
  doc <- ph_with(doc, title, location = ph_location_type(type = "title"))
  if (file.exists(image_path)) {
    doc <- ph_with(doc, external_img(image_path, width = 8.5, height = 5.2),
                   location = ph_location(left = 0.7, top = 1.3, width = 8.5, height = 5.2))
  } else {
    doc <- ph_with(doc, "Figure not generated yet", location = ph_location_type(type = "body"))
  }
  doc
}

slides <- read_pptx()
slides <- add_slide(slides, layout = "Title Slide", master = "Office Theme")
slides <- ph_with(slides, "Hospital Readmission Analysis", location = ph_location_type(type = "ctrTitle"))
slides <- ph_with(slides, "IT3081 Statistical Modelling Consultancy\nTeam Members | 2026", location = ph_location_type(type = "subTitle"))
slides <- add_title_content(slides, "The consultancy question", c(
  "How can hospitals identify patients who need coordinated discharge support?",
  "Goal: reduce preventable readmissions while protecting clinical judgement, privacy, and equity.",
  "Evidence base: 101,766 encounter records plus statistical modelling and implementation design."
))
slides <- add_title_content(slides, "Why readmission matters", c(
  "Readmission affects patient safety, continuity of care, staffing, and cost.",
  "A discharge intervention must be clinically useful and operationally resourced.",
  "Prediction alone is not a readmission-prevention programme."
))
slides <- add_title_content(slides, "Dataset and outcome", c(
  "101,766 encounter records and 50 source variables.",
  "Source outcome mapping: <30 = readmitted within 30 days; >30 and NO = not readmitted for binary modelling.",
  "30-day readmission prevalence: approximately 11.2%.",
  "Missingness is concentrated in selected administrative and clinical fields."
))
slides <- add_image_slide(slides, "Descriptive analysis: outcome and age", file.path(visual_dir, "DESCRIPTIVE", "01_readmission_distribution.png"))
slides <- add_image_slide(slides, "Descriptive analysis: utilisation and equity", file.path(visual_dir, "DESCRIPTIVE", "04_race_by_readmission_EQUITY.png"))
slides <- add_title_content(slides, "Statistical inference", c(
  "Associations were tested for age, gender, length of stay, medication count, discharge disposition, and diabetes medication status.",
  "Age, length of stay, medication count, disposition, and diabetes medication status were significant in this large sample.",
  "Gender was not significant. Effect sizes should be considered alongside p-values.",
  "Observational associations do not prove causal effects."
))
slides <- add_title_content(slides, "Predictive modelling", c(
  "Models: logistic regression, ridge regression, and LASSO.",
  "Stratified split, training-only standardization, five-fold cross-validation, and ROC-derived thresholds.",
  "Test AUC was approximately 0.66 across models.",
  "The models support a monitored pilot, not autonomous decisions."
))
slides <- add_image_slide(slides, "Model performance comparison", file.path(visual_dir, "MODELING", "04_model_performance_comparison.png"))
slides <- add_image_slide(slides, "Model discrimination", file.path(visual_dir, "MODELING", "01_logit_roc_curve.png"))
slides <- add_title_content(slides, "Innovation: HREWS", c(
  "An advisory Hospital Readmission Early Warning System.",
  "Risk probability + key drivers + provisional tier + matched discharge checklist.",
  "Clinical staff retain authority and can document overrides.",
  "Local validation, calibration, equity auditing, and prospective evaluation come first."
))
slides <- add_title_content(slides, "Risk-matched support", c(
  "Low: standard education and routine follow-up.",
  "Moderate: medication reconciliation, structured checklist, and 48-72 hour call.",
  "High: multidisciplinary planning, social-needs screening, and early nurse or telehealth contact.",
  "The intervention must match available staffing and follow-up capacity."
))
slides <- add_title_content(slides, "Critical evaluation and validation", c(
  "RCBD is preferred for testing interventions within validated risk strata.",
  "PCA is not recommended as the primary method because interpretability matters.",
  "Bayesian modelling is promising for uncertainty and cost-sensitive decisions.",
  "The current encounter table is not a patient-level time series.",
  "Expert validation must be completed with a genuine healthcare professional."
))
slides <- add_title_content(slides, "Implementation roadmap", c(
  "Months 1-2: governance, local validation, privacy review, and pilot selection.",
  "Months 3-5: dashboard, protocols, training, and monitoring plan.",
  "Months 6-8: advisory pilot with safety, calibration, equity, and workload review.",
  "Months 9-12: controlled expansion only if pilot criteria are met."
))
slides <- add_title_content(slides, "Final recommendation", c(
  "Proceed with a governed, locally validated advisory HREWS pilot.",
  "Measure readmission, calibration, follow-up completion, equity, workload, and cost.",
  "Use a prospective controlled evaluation to test whether interventions cause improvement.",
  "Next step: form the clinical and technical steering group."
))

print(slides, target = file.path(presentation_dir, "Consultancy_Presentation_15min.pptx"))

notes <- read_docx()
notes <- body_add_par(notes, "Hospital Readmission Consultancy - Presentation Speaker Notes", style = "heading 1")
notes <- body_add_par(notes, "Use approximately one minute per slide. Replace team placeholders and add genuine expert details before submission.")
for (i in seq_len(15)) {
  notes <- body_add_par(notes, paste("Slide", i), style = "heading 2")
  notes <- body_add_par(notes, paste(c(
    "State the slide's main claim clearly.",
    "Connect the evidence to the hospital decision or implementation implication.",
    "Avoid claiming causation from the observational dataset.",
    "Hand off to the next speaker with one sentence."
  ), collapse = "\n"), style = "Normal")
}
print(notes, target = file.path(presentation_dir, "Presentation_Speaker_Notes.docx"))

viva <- read_docx()
viva <- body_add_par(viva, "Anticipated Viva Questions and Answers", style = "heading 1")
qa <- list(
  c("Why logistic, ridge, and LASSO?", "They provide an interpretable baseline, a multicollinearity-robust model, and a sparse feature-selection model."),
  c("How was class imbalance handled?", "The split was stratified, AUC and sensitivity were reported, and thresholds were selected from training ROC curves rather than using accuracy alone."),
  c("Can the model prove an intervention works?", "No. The data are observational. A prospective randomized or controlled evaluation is required for causal claims."),
  c("Why is the AUC only about 0.66?", "The model uses limited encounter-level predictors and omits important social and longitudinal information. The result is useful for a pilot but not a standalone decision."),
  c("Why not PCA?", "PCA reduces variance, not necessarily prediction-relevant information, and makes clinical explanation harder."),
  c("How would you deploy HREWS safely?", "Start in advisory mode, validate locally, monitor calibration and equity, allow overrides, and expand only after a controlled pilot.")
)
for (item in qa) {
  viva <- body_add_par(viva, item[1], style = "heading 2")
  viva <- body_add_par(viva, item[2], style = "Normal")
}
print(viva, target = file.path(presentation_dir, "VIVA_PREP", "Anticipated_Questions_and_Answers.docx"))
cat("Presentation, notes, and viva documents created in ", presentation_dir, "\n")
