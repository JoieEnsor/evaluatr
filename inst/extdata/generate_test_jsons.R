## generate_test_jsons.R
## Run this script (after devtools::load_all()) to regenerate the obfuscated
## JSON files for the three test/example models.
## Output: generated_test_models/ directory in the package root.

library(evaluatr)

out_base <- file.path(
  system.file(package = "evaluatr"),   # package root when installed
  "..", "..", ".."                      # back to package source root
)
# Fallback: use current working directory (when running from package root)
if (!dir.exists(file.path(out_base, "R"))) {
  out_base <- "."
}
out_dir <- file.path(out_base, "generated_test_models")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

message("Generating obfuscated test model JSON files to: ", out_dir)

# ============================================================
# Model 1: Test logistic regression
# (Intercept) = -1.25, age = 0.02, biomarker_score = 0.8, treatment_group = -0.6
# ============================================================

message("\n--- Model 1: Test logistic (sample_model_001_v2) ---")
generate_model_json(
  coefficients = c("(Intercept)" = -1.25,
                   age             = 0.02,
                   biomarker_score = 0.8,
                   treatment_group = -0.6),
  model_type   = "logistic",
  model_id     = "sample_model_001_v2",
  model_name   = "Test Logistic Regression Model v2",
  outcome_type = "binary",
  variables    = c("age", "biomarker_score", "treatment_group"),
  description  = "Test logistic regression model with synthetic predictors. Obfuscated v1 format.",
  version      = "2.0",
  output_dir   = file.path(out_dir, "sample_model_001_v2")
)


# ============================================================
# Model 2: MIMIC AKI logistic regression
# ============================================================

message("\n--- Model 2: MIMIC AKI (sample_model_002_v2) ---")
generate_model_json(
  coefficients = c("(Intercept)"     =  3.388602,
                   gender            =  0.1837806,
                   bicarbonate_mean  = -0.0422585,
                   creatinine_mean   =  1.047176,
                   hemoglobin_mean   = -0.1877922,
                   bun_mean          = -0.0036745,
                   potassium_mean    =  0.5659359,
                   sysbp_mean        = -0.0081565,
                   spo2_mean         = -0.0432393),
  model_type   = "logistic",
  model_id     = "sample_model_002_v2",
  model_name   = "MIMIC-III AKI Prediction Model v2",
  outcome_type = "binary",
  variables    = c("gender", "bicarbonate_mean", "creatinine_mean",
                   "hemoglobin_mean", "bun_mean", "potassium_mean",
                   "sysbp_mean", "spo2_mean"),
  description  = paste0(
    "Logistic regression model for predicting acute kidney injury (AKI) ",
    "in ICU patients. Derived from MIMIC-III (de-identified). ",
    "Obfuscated v1 format."
  ),
  version      = "2.0",
  output_dir   = file.path(out_dir, "sample_model_002_v2")
)


# ============================================================
# Model 3: ADNEX with CA-125
# 4 non-reference categories: borderline (z1), stage_I (z2),
#   stage_II_IV (z3), metastatic (z4). Reference = benign.
# Preprocessing: compute log2 and squared transforms.
# ============================================================

message("\n--- Model 3: ADNEX with CA-125 (sample_model_003_v2) ---")

adnex_with_coeffs <- list(
  borderline = c(
    "(Intercept)"              = -7.577663,
    age                        =  0.004506,
    log2_ca125                 =  0.111642,
    log2_max_diam_lesion       =  0.372046,
    prop_solid                 =  6.967853,
    prop_solid_sq              = -5.65588,
    locules_gt_10              =  1.375079,
    papillary_count            =  0.604238,
    acoustic_shadows           = -2.04157,
    ascites                    =  0.971061,
    oncology_center            =  0.953043
  ),
  stage_I = c(
    "(Intercept)"              = -12.276041,
    age                        =   0.01726,
    log2_ca125                 =   0.197249,
    log2_max_diam_lesion       =   0.87353,
    prop_solid                 =   9.583053,
    prop_solid_sq              =  -5.83319,
    locules_gt_10              =   0.791873,
    papillary_count            =   0.400369,
    acoustic_shadows           =  -1.87763,
    ascites                    =   0.452731,
    oncology_center            =   0.452484
  ),
  stage_II_IV = c(
    "(Intercept)"              = -14.91583,
    age                        =   0.051239,
    log2_ca125                 =   0.765456,
    log2_max_diam_lesion       =   0.430477,
    prop_solid                 =  10.37696,
    prop_solid_sq              =  -5.70975,
    locules_gt_10              =   0.273692,
    papillary_count            =   0.389874,
    acoustic_shadows           =  -2.35516,
    ascites                    =   1.348408,
    oncology_center            =   0.459021
  ),
  metastatic = c(
    "(Intercept)"              = -11.909267,
    age                        =   0.033601,
    log2_ca125                 =   0.276166,
    log2_max_diam_lesion       =   0.449025,
    prop_solid                 =   6.644939,
    prop_solid_sq              =  -2.3033,
    locules_gt_10              =   0.89998,
    papillary_count            =   0.215645,
    acoustic_shadows           =  -2.49845,
    ascites                    =   1.636407,
    oncology_center            =   0.808887
  )
)

adnex_with_preprocessing <- paste0(
  "validation_data$log2_ca125           <- log2(validation_data$ca125)\n",
  "validation_data$log2_max_diam_lesion  <- log2(validation_data$max_diam_lesion)\n",
  "validation_data$prop_solid_sq         <- validation_data$prop_solid^2"
)

generate_model_json(
  coefficients       = adnex_with_coeffs,
  model_type         = "multinomial",
  model_id           = "sample_model_003_v2",
  model_name         = "ADNEX with CA-125",
  outcome_type       = "multinomial",
  variables          = c("age", "log2_ca125", "log2_max_diam_lesion",
                         "prop_solid", "prop_solid_sq", "locules_gt_10",
                         "papillary_count", "acoustic_shadows",
                         "ascites", "oncology_center"),
  description        = paste0(
    "ADNEX multinomial logistic regression model for classifying ovarian masses. ",
    "Reference category: benign. Non-reference categories: borderline, stage_I, ",
    "stage_II_IV, metastatic. Uses CA-125. ",
    "Van Calster et al. BMJ 2014. Obfuscated v1 format."
  ),
  version            = "2.0",
  preprocessing      = adnex_with_preprocessing,
  reference_category = "benign",
  output_dir         = file.path(out_dir, "sample_model_003_v2")
)


# ============================================================
# Model 4: ADNEX without CA-125
# Same structure but one fewer predictor (no ca125)
# ============================================================

message("\n--- Model 4: ADNEX without CA-125 (sample_model_004_v2) ---")

adnex_without_coeffs <- list(
  borderline = c(
    "(Intercept)"              = -7.412534,
    age                        =  0.003489,
    log2_max_diam_lesion       =  0.430701,
    prop_solid                 =  7.117925,
    prop_solid_sq              = -5.74135,
    locules_gt_10              =  1.343699,
    papillary_count            =  0.607211,
    acoustic_shadows           = -2.11885,
    ascites                    =  1.167767,
    oncology_center            =  0.983227
  ),
  stage_I = c(
    "(Intercept)"              = -12.201607,
    age                        =   0.017607,
    log2_max_diam_lesion       =   0.98728,
    prop_solid                 =  10.07145,
    prop_solid_sq              =  -6.17742,
    locules_gt_10              =   0.763081,
    papillary_count            =   0.410449,
    acoustic_shadows           =  -1.98073,
    ascites                    =   0.77054,
    oncology_center            =   0.543677
  ),
  stage_II_IV = c(
    "(Intercept)"              = -12.826207,
    age                        =   0.045172,
    log2_max_diam_lesion       =   0.759002,
    prop_solid                 =  11.83296,
    prop_solid_sq              =  -6.64336,
    locules_gt_10              =   0.316444,
    papillary_count            =   0.390959,
    acoustic_shadows           =  -2.94082,
    ascites                    =   2.691276,
    oncology_center            =   0.929483
  ),
  metastatic = c(
    "(Intercept)"              = -11.424379,
    age                        =   0.033407,
    log2_max_diam_lesion       =   0.560396,
    prop_solid                 =   7.264105,
    prop_solid_sq              =  -2.77392,
    locules_gt_10              =   0.983394,
    papillary_count            =   0.199164,
    acoustic_shadows           =  -2.63702,
    ascites                    =   2.185574,
    oncology_center            =   0.906249
  )
)

adnex_without_preprocessing <- paste0(
  "validation_data$log2_max_diam_lesion  <- log2(validation_data$max_diam_lesion)\n",
  "validation_data$prop_solid_sq         <- validation_data$prop_solid^2"
)

generate_model_json(
  coefficients       = adnex_without_coeffs,
  model_type         = "multinomial",
  model_id           = "sample_model_004_v2",
  model_name         = "ADNEX without CA-125",
  outcome_type       = "multinomial",
  variables          = c("age", "log2_max_diam_lesion",
                         "prop_solid", "prop_solid_sq", "locules_gt_10",
                         "papillary_count", "acoustic_shadows",
                         "ascites", "oncology_center"),
  description        = paste0(
    "ADNEX multinomial logistic regression model for classifying ovarian masses. ",
    "Reference category: benign. Non-reference categories: borderline, stage_I, ",
    "stage_II_IV, metastatic. Does not use CA-125. ",
    "Van Calster et al. BMJ 2014. Obfuscated v1 format."
  ),
  version            = "2.0",
  preprocessing      = adnex_without_preprocessing,
  reference_category = "benign",
  output_dir         = file.path(out_dir, "sample_model_004_v2")
)

message("\nDone. JSON files written to: ", out_dir)
message("Upload each subfolder to your GitHub model repository.")
