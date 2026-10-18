#' Cross-tabulate qualitative themes against a quantitative metric
#'
#' For each theme, compares a numeric metric between units where the theme is
#' present and units where it is absent, using Welch's two-sample t-test.
#' P-values are adjusted for multiple comparisons across themes.
#'
#' When the text was coded at segment level (several rows per participant) and
#' the metrics live in a separate participant-level table, pass that table as
#' `metrics`: codes are first aggregated per participant (a theme counts as
#' present if it appears in any of their segments) and then joined to the
#' metrics by `id_col`.
#'
#' @param coded_data A data frame returned by [llm_auto_code()].
#' @param metric_col The numeric metric column (unquoted or a string). It is
#'   looked up in `metrics` if supplied, otherwise in `coded_data`.
#' @param themes Character vector of theme columns. Defaults to the codebook
#'   stored by [llm_auto_code()].
#' @param metrics Optional participant-level data frame holding the metric.
#' @param id_col Name of the ID column (a string) used to aggregate and join
#'   when `metrics` is supplied. Defaults to the ID column used in
#'   [llm_auto_code()].
#' @param p_adjust Multiple-comparison correction passed to
#'   [stats::p.adjust()]. Defaults to `"BH"` (false discovery rate).
#' @param alpha Significance threshold applied to the adjusted p-value.
#'
#' @return A tibble with one row per theme: group sizes and means, the mean
#'   difference (present minus absent) with its 95% confidence interval,
#'   Cohen's d, raw and adjusted p-values, and a `significant` flag. Themes
#'   with fewer than two units in either group get `NA` statistics.
#' @export
#'
#' @examples
#' df <- mock_mixed_data(80, seed = 42)
#' coded <- llm_auto_code(df, user_id, transcript_feedback,
#'                        codebook = c("UI_Confusion", "Positive_Experience",
#'                                     "Feature_Request"),
#'                        coder = mock_coder())
#' cross_tab_themes(coded, session_duration_sec)
cross_tab_themes <- function(coded_data, metric_col, themes = NULL,
                             metrics = NULL, id_col = NULL,
                             p_adjust = "BH", alpha = 0.05) {
  if (!is.data.frame(coded_data)) {
    cli::cli_abort("{.arg coded_data} must be a data frame.")
  }
  if (is.null(themes)) {
    themes <- names(attr(coded_data, "saydo_codebook"))
    if (is.null(themes)) {
      cli::cli_abort("Supply {.arg themes}; {.arg coded_data} has no stored codebook.")
    }
  }
  missing <- setdiff(themes, names(coded_data))
  if (length(missing) > 0) {
    cli::cli_abort("Theme column{?s} {.val {missing}} not found in {.arg coded_data}.")
  }

  analysis <- coded_data
  if (!is.null(metrics)) {
    id_name <- id_col %||% attr(coded_data, "saydo_id")
    if (is.null(id_name)) {
      cli::cli_abort("Supply {.arg id_col} to join {.arg metrics} to the coded data.")
    }
    if (!id_name %in% names(coded_data)) {
      cli::cli_abort("ID column {.val {id_name}} not found in {.arg coded_data}.")
    }
    if (!id_name %in% names(metrics)) {
      cli::cli_abort("ID column {.val {id_name}} not found in {.arg metrics}.")
    }
    if (anyDuplicated(metrics[[id_name]])) {
      cli::cli_abort("{.arg metrics} must have one row per {.val {id_name}}.")
    }
    analysis <- aggregate_codes(coded_data, id_name, themes)
    analysis <- merge(metrics, analysis, by = id_name, all.x = FALSE)
  }

  metric_name <- column_name(rlang::enquo(metric_col), analysis, "metric_col")
  y <- analysis[[metric_name]]
  if (!is.numeric(y)) {
    cli::cli_abort("Metric {.val {metric_name}} must be numeric.")
  }

  rows <- lapply(themes, function(theme) compare_groups(y, analysis[[theme]], theme))
  out <- do.call(rbind, rows)
  out$p_adjusted <- stats::p.adjust(out$p_value, method = p_adjust)
  out$significant <- !is.na(out$p_adjusted) & out$p_adjusted < alpha

  out <- tibble::as_tibble(out)
  attr(out, "metric") <- metric_name
  out
}

compare_groups <- function(y, present, theme) {
  present <- as.logical(present)
  keep <- !is.na(y) & !is.na(present)
  y_in <- y[keep & present]
  y_out <- y[keep & !present]

  res <- data.frame(
    theme = theme,
    n_absent = length(y_out),
    n_present = length(y_in),
    mean_absent = if (length(y_out)) mean(y_out) else NA_real_,
    mean_present = if (length(y_in)) mean(y_in) else NA_real_,
    difference = NA_real_,
    conf_low = NA_real_,
    conf_high = NA_real_,
    cohens_d = NA_real_,
    p_value = NA_real_
  )
  if (length(y_in) < 2 || length(y_out) < 2) {
    return(res)
  }

  res$difference <- res$mean_present - res$mean_absent
  pooled_sd <- sqrt(((length(y_in) - 1) * stats::var(y_in) +
                       (length(y_out) - 1) * stats::var(y_out)) /
                      (length(y_in) + length(y_out) - 2))
  if (pooled_sd > 0) {
    res$cohens_d <- res$difference / pooled_sd
    tt <- stats::t.test(y_in, y_out)
    res$conf_low <- tt$conf.int[1]
    res$conf_high <- tt$conf.int[2]
    res$p_value <- tt$p.value
  }
  res
}

# A theme is present for a participant if any of their segments has it; NA
# only when all of their segments are NA.
aggregate_codes <- function(data, id_name, themes) {
  ids <- unique(data[[id_name]])
  idx <- match(data[[id_name]], ids)
  out <- data.frame(ids)
  names(out) <- id_name
  for (theme in themes) {
    out[[theme]] <- vapply(seq_along(ids), function(i) {
      x <- data[[theme]][idx == i]
      if (all(is.na(x))) NA else any(x, na.rm = TRUE)
    }, logical(1))
  }
  out
}

`%||%` <- function(x, y) if (is.null(x)) y else x
