# evaluatr: Performance Metrics for Clinical Prediction Model Evaluation
# v0.1.0

# Suppress R CMD check notes for ggplot2 aes() column references
utils::globalVariables(c("thresholds", "NB", "sNB", "label", "y_plot"))

# Compute decision curve net benefit at each threshold (binary outcome).
# Returns a data frame with columns: thresholds, NB, label.
.compute_dca <- function(outcomes, predictions,
                         thresholds = seq(0, 1, by = 0.01)) {
  n    <- length(outcomes)
  prev <- mean(outcomes)
  rows <- lapply(thresholds, function(pt) {
    if (pt >= 1) return(NULL)
    tp     <- sum(outcomes == 1 & predictions >= pt)
    fp     <- sum(outcomes == 0 & predictions >= pt)
    nb     <- tp / n - (fp / n) * (pt / (1 - pt))
    nb_all <- prev - (1 - prev) * (pt / (1 - pt))
    data.frame(
      thresholds = pt,
      NB         = c(nb, nb_all, 0),
      label      = c("Model", "Treat all", "Treat none"),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Calculate Performance Metrics from a Validation Result
#'
#' Computes a comprehensive set of discrimination, calibration, and utility
#' performance metrics from the shuffled prediction-outcome matrix returned
#' by [secure_model_validation()]. Optionally generates diagnostic plots and
#' bootstrap confidence intervals.
#'
#' @param validation_result List. Output from [secure_model_validation()].
#' @param generate_plots Logical. Whether to produce calibration, decision
#'   curve, and probability distribution plots. Default `TRUE`.
#' @param confidence_intervals Logical. Whether to compute 95% bootstrap
#'   confidence intervals. Default `TRUE`.
#' @param n_boot Integer. Number of bootstrap resamples. Default `200`.
#'   Increase to 1000 for publication-quality confidence intervals.
#' @param plot_title Character or `NULL`. Optional title suffix added to
#'   plot headings.
#' @param decision_threshold Numeric. Probability threshold used for net
#'   benefit calculations in the decision curve. Default `0.1`. Set to the
#'   clinically relevant threshold for your outcome.
#' @param by Vector or `NULL`. Optional subgroup vector of the same length
#'   as the predictions (pass `result$shuffled_by` from the output of
#'   [secure_model_validation()] with the `by` argument). When provided,
#'   metrics are computed separately for each level of this variable.
#' @param cal_args Named list of additional arguments controlling the
#'   calibration plot. Supported options:
#'   \describe{
#'     \item{`g`}{Integer. Number of groups. Default `10`.}
#'     \item{`smooth`}{Character. Flexible calibration curve method:
#'       `"loess"` (default) or `"rcs"`.}
#'     \item{`CL.smooth`}{Whether to show the confidence band around the
#'       flexible curve: `"fill"` (default shaded band), `"line"`, or
#'       `FALSE` to suppress.}
#'     \item{`xlim`}{Numeric vector of length 2. X-axis limits.
#'       Default `c(0, 1)`.}
#'     \item{`ylim`}{Numeric vector of length 2. Y-axis limits.
#'       Default `c(0, 1)`.}
#'   }
#' @param dca_args Named list of additional arguments controlling the
#'   decision curve analysis. Supported options:
#'   \describe{
#'     \item{`thresholds`}{Numeric vector. Threshold probabilities at which
#'       to evaluate net benefit. Default `seq(0, 1, by = 0.01)`.}
#'     \item{`standardize`}{Logical. If `TRUE`, plot standardised net benefit
#'       instead of net benefit. Default `FALSE`.}
#'     \item{`policy`}{Character. `"opt-in"` (default, treat high-risk
#'       patients) or `"opt-out"` (withhold treatment from low-risk patients).}
#'   }
#'
#' @return A named list:
#' \describe{
#'   \item{`metrics`}{Data frame of performance metrics. When `by` is
#'     supplied a `Subgroup` column is prepended.}
#'   \item{`plots`}{List of plot objects: `calibration`, `decision_curve`,
#'     and `distribution`. When `by` is supplied, plots are organised by
#'     subgroup level: `plots[["level_name"]]$calibration`, etc.}
#'   \item{`model_info`}{Metadata from `validation_result`.}
#'   \item{`decision_threshold`}{The threshold value used.}
#'   \item{`subgroup_variable`}{Name of the `by` variable (subgroup
#'     analysis only).}
#'   \item{`subgroup_categories`}{Character vector of category names
#'     (subgroup analysis only).}
#' }
#'
#' @details
#' Metrics computed include: AUROC/c-statistic, calibration intercept,
#' calibration slope, ECI, ICI, ECE, O:E ratio, Brier score, Nagelkerke
#' R-squared, Cox-Snell R-squared, McFadden R-squared, net benefit, and
#' standardised net benefit.
#'
#' This function currently supports **binary outcomes** (0/1) only.
#' Multinomial support is planned for a future release.
#'
#' @examples
#' \dontrun{
#' # ---- Whole-dataset evaluation ----------------------------------------------
#' dvt_path <- system.file("extdata", "dvt_example.csv", package = "evaluatr")
#' dvt_val  <- read.csv(dvt_path)
#' dvt_val  <- dvt_val[dvt_val$study %in% c(6, 7), ]
#'
#' result <- secure_model_validation(
#'   repo_owner      = "JoieEnsor",
#'   repo_name       = "evaluatr-demo-models",
#'   model_id        = "dvt_logistic_v1",
#'   github_token    = "evaluatr-demo",
#'   validation_data = dvt_val,
#'   outcome         = "dvt"
#' )
#'
#' perf <- eval_performance(
#'   result,
#'   n_boot             = 200,
#'   decision_threshold = 0.15
#' )
#' perf$metrics
#'
#' # Access individual diagnostic plots
#' perf$plots$calibration    # calibration plot
#' perf$plots$decision_curve # decision curve analysis
#' perf$plots$distribution   # predicted probability distribution
#'
#' # ---- Subgroup evaluation by study -----------------------------------------
#' result_by_study <- secure_model_validation(
#'   repo_owner      = "JoieEnsor",
#'   repo_name       = "evaluatr-demo-models",
#'   model_id        = "dvt_logistic_v1",
#'   github_token    = "evaluatr-demo",
#'   validation_data = dvt_val,
#'   outcome         = "dvt",
#'   by              = "study"
#' )
#'
#' perf_by_study <- eval_performance(
#'   result_by_study,
#'   by                 = result_by_study$shuffled_by,
#'   n_boot             = 200,
#'   decision_threshold = 0.15
#' )
#'
#' # Metrics table has a Subgroup column
#' perf_by_study$metrics
#'
#' # Access study-specific plots
#' perf_by_study$plots[["6"]]$calibration
#' perf_by_study$plots[["7"]]$calibration
#'
#' # ---- Customising the calibration plot -------------------------------------
#' perf_custom <- eval_performance(
#'   result,
#'   n_boot             = 200,
#'   decision_threshold = 0.15,
#'   cal_args = list(smooth = "rcs", g = 5),
#'   dca_args = list(standardize = TRUE)
#' )
#' }
#'
#' @seealso [secure_model_validation()], [list_registered_models()]
#' @export
eval_performance <- function(validation_result,
                                       generate_plots = TRUE,
                                       confidence_intervals = TRUE,
                                       n_boot = 200,
                                       plot_title = NULL,
                                       decision_threshold = 0.1,
                                       by = NULL,
                                       cal_args = list(),
                                       dca_args = list()) {

  # ---- Input validation -------------------------------------------------------
  if (!inherits(validation_result, "evaluatr_result")) {
    stop("'validation_result' must be the output of secure_model_validation().")
  }

  outcomes    <- validation_result$shuffled_outcomes
  predictions <- validation_result$shuffled_predictions

  if (length(outcomes) != length(predictions)) {
    stop("Outcomes and predictions must have the same length.")
  }
  if (!all(outcomes %in% c(0, 1))) {
    stop("Outcomes must be binary (0/1). Multinomial support is planned for a future release.")
  }
  if (any(predictions < 0) || any(predictions > 1)) {
    stop("Predictions must be probabilities between 0 and 1.")
  }

  # ---- Subgroup analysis ------------------------------------------------------
  if (!is.null(by)) {
    if (length(by) != length(outcomes)) {
      stop("'by' must be a vector of the same length as the predictions.")
    }

    by_categories <- unique(by[!is.na(by)])
    if (length(by_categories) < 2) {
      stop("'by' must have at least 2 non-missing categories.")
    }

    message("Subgroup analysis: ", length(by_categories), " groups (",
            paste(by_categories, collapse = ", "), ")")

    all_results <- list()
    all_plots   <- list()

    for (cat in by_categories) {
      cat_name    <- as.character(cat)
      cat_idx     <- which(by == cat & !is.na(by))
      cat_out     <- outcomes[cat_idx]
      cat_pred    <- predictions[cat_idx]

      if (length(cat_out) < 10) {
        stop("Subgroup '", cat_name, "' has fewer than 10 observations.")
      }
      if (length(unique(cat_out)) < 2) {
        warning("Subgroup '", cat_name, "' has no variation in outcomes -- skipping.")
        next
      }

      message("  Computing metrics for subgroup: ", cat_name)
      cat_result <- .calculate_metrics_single(
        outcomes             = cat_out,
        predictions          = cat_pred,
        confidence_intervals = confidence_intervals,
        n_boot               = n_boot,
        decision_threshold   = decision_threshold,
        generate_plots       = generate_plots,
        plot_title           = paste0(if (!is.null(plot_title)) paste0(plot_title, " -- "),
                                     "Subgroup: ", cat_name),
        cal_args             = cal_args,
        dca_args             = dca_args
      )
      all_results[[cat_name]] <- cat_result$metrics
      all_plots[[cat_name]]   <- cat_result$plots
    }

    # Combine results
    combined_metrics <- do.call(rbind, lapply(names(all_results), function(nm) {
      df <- all_results[[nm]]
      df$Subgroup <- nm
      df
    }))
    col_order <- c("Subgroup", "Metric", "Value",
                   setdiff(names(combined_metrics), c("Subgroup", "Metric", "Value")))
    combined_metrics <- combined_metrics[, col_order]

    # Console summary
    .print_metrics_header(validation_result, decision_threshold,
                          subgroup = TRUE, n_groups = length(all_results))
    .print_metrics_table(combined_metrics, confidence_intervals, subgroup = TRUE)

    return(invisible(list(
      metrics             = combined_metrics,
      plots               = all_plots,
      model_info          = validation_result$model_info,
      decision_threshold  = decision_threshold,
      subgroup_variable   = deparse(substitute(by)),
      subgroup_categories = names(all_results)
    )))
  }

  # ---- Overall analysis -------------------------------------------------------
  result <- .calculate_metrics_single(
    outcomes             = outcomes,
    predictions          = predictions,
    confidence_intervals = confidence_intervals,
    n_boot               = n_boot,
    decision_threshold   = decision_threshold,
    generate_plots       = generate_plots,
    plot_title           = plot_title,
    cal_args             = cal_args,
    dca_args             = dca_args
  )

  .print_metrics_header(validation_result, decision_threshold)
  .print_metrics_table(result$metrics, confidence_intervals)

  invisible(list(
    metrics            = result$metrics,
    plots              = result$plots,
    model_info         = validation_result$model_info,
    decision_threshold = decision_threshold
  ))
}


# ---- Internal helpers --------------------------------------------------------

# Print the summary header to console
.print_metrics_header <- function(validation_result, decision_threshold,
                                  subgroup = FALSE, n_groups = NULL) {
  cat("\n=== evaluatr performance metrics",
      if (subgroup) "-- subgroup analysis" else "", "===\n")
  cat("Model ID:          ", validation_result$model_info$model_id, "\n")
  cat("N predictions:     ", validation_result$model_info$n_predictions, "\n")
  cat("Decision threshold:", decision_threshold, "\n")
  if (subgroup) cat("Number of subgroups:", n_groups, "\n")
  cat("Validated at:      ", format(validation_result$model_info$validation_timestamp), "\n\n")
}

# Print formatted metrics table to console
.print_metrics_table <- function(metrics_df, confidence_intervals,
                                 subgroup = FALSE) {
  if (subgroup) {
    if (confidence_intervals) {
      cat(sprintf("%-15s  %-25s  %6s   %s\n", "Subgroup", "Metric", "Value", "95% CI"))
      cat(strrep("-", 68), "\n")
      for (i in seq_len(nrow(metrics_df))) {
        cat(sprintf("%-15s  %-25s  %6.3f   %s\n",
                    metrics_df$Subgroup[i], metrics_df$Metric[i],
                    metrics_df$Value[i],   metrics_df$CI_String[i]))
      }
    } else {
      cat(sprintf("%-15s  %-25s  %6s\n", "Subgroup", "Metric", "Value"))
      cat(strrep("-", 50), "\n")
      for (i in seq_len(nrow(metrics_df))) {
        cat(sprintf("%-15s  %-25s  %6.3f\n",
                    metrics_df$Subgroup[i], metrics_df$Metric[i],
                    metrics_df$Value[i]))
      }
    }
  } else {
    if (confidence_intervals) {
      cat(sprintf("%-25s  %6s   %s\n", "Metric", "Value", "95% CI"))
      cat(strrep("-", 50), "\n")
      for (i in seq_len(nrow(metrics_df))) {
        cat(sprintf("%-25s  %6.3f   %s\n",
                    metrics_df$Metric[i], metrics_df$Value[i],
                    metrics_df$CI_String[i]))
      }
    } else {
      cat(sprintf("%-25s  %6s\n", "Metric", "Value"))
      cat(strrep("-", 35), "\n")
      for (i in seq_len(nrow(metrics_df))) {
        cat(sprintf("%-25s  %6.3f\n", metrics_df$Metric[i], metrics_df$Value[i]))
      }
    }
  }
}

# Core metrics calculation for a single group
.calculate_metrics_single <- function(outcomes, predictions,
                                      confidence_intervals, n_boot,
                                      decision_threshold, generate_plots,
                                      plot_title,
                                      cal_args = list(),
                                      dca_args = list()) {

  message("Calculating performance metrics...")

  overall_metrics <- .OvPerfBin(y = outcomes,  p = predictions)
  disc_metrics    <- .DiscPerfBin(y = outcomes, p = predictions)
  cal_metrics     <- .CalPerfBin(y = outcomes,  p = predictions, flexcal = "loess")
  util_metrics    <- .UtilPerfBin(y = outcomes, p = predictions, cut = decision_threshold)

  all_metrics <- cbind(overall_metrics, disc_metrics, cal_metrics, util_metrics)

  selected_cols <- c(
    "AUROC/c statistic", "Cal. intercept", "Cal. slope",
    "ECI", "ICI", "ECE", "O:E ratio", "Brier",
    "Nagelkerke R2", "Cox-Snell R2", "McFadden R2",
    "Net benefit", "Standardized net benefit"
  )
  selected_metrics <- all_metrics[, selected_cols]

  results_df <- data.frame(
    Metric = names(selected_metrics),
    Value  = as.numeric(selected_metrics[1, ]),
    stringsAsFactors = FALSE
  )

  # Bootstrap CIs
  if (confidence_intervals) {
    message("Calculating bootstrap confidence intervals (n_boot = ", n_boot, ")...")
    boot_results <- matrix(NA, nrow = n_boot, ncol = length(selected_cols),
                           dimnames = list(NULL, selected_cols))
    set.seed(123)
    for (i in seq_len(n_boot)) {
      idx   <- sample(length(outcomes), replace = TRUE)
      b_out <- outcomes[idx]
      b_pred <- predictions[idx]
      tryCatch({
        b_all <- cbind(
          .OvPerfBin(y = b_out, p = b_pred),
          .DiscPerfBin(y = b_out, p = b_pred),
          .CalPerfBin(y = b_out, p = b_pred, flexcal = "loess"),
          .UtilPerfBin(y = b_out, p = b_pred, cut = decision_threshold)
        )
        boot_results[i, ] <- as.numeric(b_all[1, selected_cols])
      }, error = function(e) NULL)
    }
    ci_lower <- apply(boot_results, 2, stats::quantile, 0.025, na.rm = TRUE)
    ci_upper <- apply(boot_results, 2, stats::quantile, 0.975, na.rm = TRUE)
    results_df$CI_Lower <- ci_lower
    results_df$CI_Upper <- ci_upper
    results_df$CI_String <- paste0("(", round(ci_lower, 3), ", ", round(ci_upper, 3), ")")
  }

  results_df$Value <- round(results_df$Value, 3)

  # Plots
  plots <- list()
  if (generate_plots) {
    message("Generating diagnostic plots...")

    # 1. Calibration plot (ggplot via CalibrationCurves::valProbggplot)
    # valProbggplot returns a ggplotCalibrationCurve S3 object; the underlying
    # ggplot lives in the $ggPlot slot. cal_args overrides defaults.
    plots$calibration <- tryCatch({
      cal_defaults <- list(
        CL.smooth    = "fill",
        logistic.cal = FALSE,
        g            = 10,
        dostats      = FALSE,
        xlim         = c(0, 1),
        ylim         = c(0, 1)
      )
      cal_call <- c(
        list(p = predictions, y = outcomes),
        utils::modifyList(cal_defaults, cal_args[
          intersect(names(cal_args),
                    c("g", "smooth", "CL.smooth", "xlim", "ylim"))
        ])
      )
      cal_obj <- do.call(CalibrationCurves::valProbggplot, cal_call)
      cal_obj$ggPlot +
        ggplot2::labs(
          x     = "Predicted probability",
          y     = "Observed probability",
          title = paste0("Calibration Plot",
                         if (!is.null(plot_title)) paste0(" -- ", plot_title))
        ) +
        ggplot2::theme(
          axis.title = ggplot2::element_text(size = 12, face = "bold"),
          plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5)
        )
    }, error = function(e) {
      warning("Calibration plot failed: ", e$message); NULL
    })

    # 2. Decision curve
    # dca_args: thresholds, standardize
    plots$decision_curve <- tryCatch({
      standardize <- isTRUE(dca_args[["standardize"]])
      thresholds  <- dca_args[["thresholds"]] %||% seq(0, 1, by = 0.01)
      dd          <- .compute_dca(outcomes, predictions, thresholds)
      dd$sNB      <- dd$NB / mean(outcomes)
      y_lab   <- if (standardize) "Standardised net benefit" else "Net benefit"
      dd$y_plot <- if (standardize) dd$sNB else dd$NB
      ggplot2::ggplot(dd, ggplot2::aes(x = thresholds, y = y_plot,
                                       colour = label, linetype = label)) +
        ggplot2::geom_line(linewidth = 0.9, na.rm = TRUE) +
        ggplot2::scale_colour_manual(
          values = c("Model" = "black", "Treat all" = "gray50", "Treat none" = "gray80")
        ) +
        ggplot2::scale_linetype_manual(
          values = c("Model" = "solid", "Treat all" = "dashed", "Treat none" = "dotted")
        ) +
        ggplot2::coord_cartesian(ylim = c(
          max(-0.05, min(dd$y_plot, na.rm = TRUE) - 0.01),
          max(dd$y_plot, na.rm = TRUE) + 0.01
        )) +
        ggplot2::labs(
          x      = "Threshold probability",
          y      = y_lab,
          title  = paste0("Decision Curve",
                          if (!is.null(plot_title)) paste0(" -- ", plot_title)),
          colour = NULL, linetype = NULL
        ) +
        ggplot2::theme_classic() +
        ggplot2::theme(
          legend.position  = "bottom",
          axis.text        = ggplot2::element_text(size = 10),
          axis.title       = ggplot2::element_text(size = 12, face = "bold"),
          plot.title       = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5)
        )
    }, error = function(e) {
      warning("Decision curve failed: ", e$message); NULL
    })

    # 3. Probability distribution (ggplot2)
    plots$distribution <- tryCatch({
      plot_df <- data.frame(
        outcome    = factor(outcomes, levels = c(0, 1),
                            labels = c("Negative", "Positive")),
        prediction = predictions
      )
      p <- ggplot2::ggplot(plot_df,
               ggplot2::aes(x = outcome, y = prediction, fill = outcome)) +
        ggplot2::geom_violin(alpha = 0.7) +
        ggplot2::geom_jitter(width = 0.2, alpha = 0.5, size = 1) +
        ggplot2::labs(
          x = "Outcome", y = "Predicted probability",
          title = paste0("Predicted Probability Distribution",
                         if (!is.null(plot_title)) paste0(" -- ", plot_title))
        ) +
        ggplot2::theme_classic() +
        ggplot2::theme(
          legend.position = "none",
          axis.text.x  = ggplot2::element_text(size = 12, face = "bold"),
          axis.text.y  = ggplot2::element_text(size = 10),
          axis.title   = ggplot2::element_text(size = 12, face = "bold"),
          plot.title   = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5)
        ) +
        ggplot2::scale_fill_manual(values = c("lightblue", "lightcoral"))
      p
    }, error = function(e) {
      warning("Distribution plot failed: ", e$message); NULL
    })
  }

  list(metrics = results_df, plots = plots)
}


# ---- BVC Statistical Helper Functions (internal) ----------------------------
# Original implementations by Ben Van Calster

# Fast AUROC via Wilcoxon rank-sum statistic
.fastAUC <- function(p, y) {
  x1 <- p[y == 1]; n1 <- length(x1)
  x2 <- p[y == 0]; n2 <- length(x2)
  r  <- rank(c(x1, x2))
  (sum(r[seq_len(n1)]) - n1 * (n1 + 1) / 2) / n1 / n2
}

# Discrimination
.DiscPerfBin <- function(y, p) {
  df <- as.data.frame(t(.fastAUC(p = p, y = y)))
  colnames(df) <- "AUROC/c statistic"
  df
}

# Calibration
.CalPerfBin <- function(y, p, flexcal = "loess", ngr = 10) {
  oe  <- sum(y) / sum(p)
  int <- summary(stats::glm(y ~ 1, offset = stats::qlogis(p),
                             family = "binomial"))$coefficients[1, 1]
  sl  <- summary(stats::glm(y ~ stats::qlogis(p),
                             family = "binomial"))$coefficients[2, 1]

  flc <- if (flexcal == "loess") {
    stats::predict(stats::loess(y ~ p, degree = 2))
  } else if (flexcal == "rcs5") {
    stats::predict(stats::glm(y ~ rms::rcs(stats::qlogis(p), 5),
                              family = "binomial"), type = "response")
  } else if (flexcal == "rcs3") {
    stats::predict(stats::glm(y ~ rms::rcs(stats::qlogis(p), 3),
                              family = "binomial"), type = "response")
  }

  eci <- mean((flc - p)^2) / mean((mean(y) - p)^2)
  ici <- mean(abs(flc - p))
  hlt <- suppressWarnings(ResourceSelection::hoslem.test(y, p, g = ngr))
  ece <- sum(abs(hlt$expected[, 2] - hlt$observed[, 2]) / length(y))

  df <- as.data.frame(t(c(oe, int, sl, eci, ici, ece)))
  colnames(df) <- c("O:E ratio", "Cal. intercept", "Cal. slope",
                    "ECI", "ICI", "ECE")
  df
}

# Overall performance
.OvPerfBin <- function(y, p) {
  lli  <- sum(stats::dbinom(y, prob = p,         size = 1, log = TRUE))
  ll0  <- sum(stats::dbinom(y, prob = mean(y),   size = 1, log = TRUE))
  llo  <- -lli
  br   <- mean((y - p)^2)
  bss  <- 1 - br / mean((y - mean(y))^2)
  mfr2 <- 1 - lli / ll0
  csr2 <- 1 - exp(2 * (ll0 - lli) / length(y))
  nr2  <- csr2 / (1 - exp(2 * ll0 / length(y)))
  ds   <- mean(p[y == 1]) - mean(p[y == 0])
  mape <- mean(abs(y - p))

  df <- as.data.frame(t(c(lli, llo, br, bss, mfr2, csr2, nr2, ds, mape)))
  colnames(df) <- c("Loglikelihood", "Logloss", "Brier", "Scaled Brier",
                    "McFadden R2", "Cox-Snell R2", "Nagelkerke R2",
                    "Discrimination slope", "MAPE")
  df
}

# Utility
.UtilPerfBin <- function(y, p, cut) {
  NB  <- mean((p >= cut) * (y == 1)) -
         (cut / (1 - cut)) * mean((p >= cut) * (y == 0))
  SNB <- NB / mean(y)
  df  <- as.data.frame(t(c(NB, SNB)))
  colnames(df) <- c("Net benefit", "Standardized net benefit")
  df
}
