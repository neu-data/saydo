#' Measure agreement between LLM codes and human codes
#'
#' Validates automated coding against a human-coded subsample. For each theme
#' it reports raw agreement, Cohen's kappa with a confidence interval, and,
#' treating the human codes as the reference, the LLM's precision, recall and
#' F1 score.
#'
#' Units are matched by `id_col`, which must be unique in both data frames
#' (use a segment ID when coding at segment level). Units missing from either
#' data frame, or with an `NA` code from either coder, are left out of that
#' theme's statistics.
#'
#' @param llm_coded A data frame returned by [llm_auto_code()].
#' @param human_coded A data frame with the same ID column and the human
#'   codes in columns named after the themes (logical, or 0/1).
#' @param id_col Name of the ID column (a string). Defaults to the ID column
#'   used in [llm_auto_code()].
#' @param themes Character vector of theme columns. Defaults to the codebook
#'   stored by [llm_auto_code()].
#' @param conf_level Confidence level for the kappa interval.
#'
#' @return A tibble with one row per theme: `n` units compared, prevalence of
#'   the theme for each coder, percent `agreement`, `kappa` with `kappa_low`
#'   and `kappa_high`, and `precision`, `recall` and `f1`. Kappa is `NA` when
#'   it is undefined (both coders give every unit the same code).
#'
#' @details The kappa interval uses the large-sample standard error
#'   \eqn{\sqrt{p_o(1 - p_o) / (n(1 - p_e)^2)}}, which is approximate for small
#'   samples. As a rough guide, kappa above 0.6 is commonly read as
#'   substantial agreement and above 0.8 as near-perfect.
#'
#' @export
#'
#' @examples
#' df <- mock_mixed_data(60, seed = 1)
#' coded <- llm_auto_code(df, user_id, transcript_feedback,
#'                        codebook = c("UI_Confusion", "Positive_Experience"),
#'                        coder = mock_coder())
#'
#' # Pretend a human coded a subsample, using the hidden profile:
#' human <- data.frame(
#'   user_id = df$user_id[1:30],
#'   UI_Confusion = df$profile[1:30] == "frustrated",
#'   Positive_Experience = df$profile[1:30] == "efficient"
#' )
#' code_agreement(coded, human)
code_agreement <- function(llm_coded, human_coded, id_col = NULL, themes = NULL,
                           conf_level = 0.95) {
  if (!is.data.frame(llm_coded) || !is.data.frame(human_coded)) {
    cli::cli_abort("{.arg llm_coded} and {.arg human_coded} must be data frames.")
  }
  id_name <- id_col %||% attr(llm_coded, "saydo_id")
  if (is.null(id_name)) {
    cli::cli_abort("Supply {.arg id_col}; {.arg llm_coded} has no stored ID column.")
  }
  themes <- themes %||% names(attr(llm_coded, "saydo_codebook"))
  if (is.null(themes)) {
    cli::cli_abort("Supply {.arg themes}; {.arg llm_coded} has no stored codebook.")
  }

  for (nm in c("llm_coded", "human_coded")) {
    df <- if (nm == "llm_coded") llm_coded else human_coded
    missing <- setdiff(c(id_name, themes), names(df))
    if (length(missing) > 0) {
      cli::cli_abort("Column{?s} {.val {missing}} not found in {.arg {nm}}.")
    }
    if (anyDuplicated(df[[id_name]])) {
      cli::cli_abort(c(
        "{.arg {nm}} must have one row per {.val {id_name}}.",
        i = "When coding segments, give each segment its own ID."
      ))
    }
  }

  idx <- match(human_coded[[id_name]], llm_coded[[id_name]])
  matched <- !is.na(idx)
  if (!any(matched)) {
    cli::cli_abort("No IDs in {.arg human_coded} match {.arg llm_coded}.")
  }
  if (any(!matched)) {
    cli::cli_warn("{sum(!matched)} human-coded unit{?s} not found in {.arg llm_coded} and skipped.")
  }

  rows <- lapply(themes, function(theme) {
    agreement_stats(
      llm = as_code(llm_coded[[theme]][idx[matched]], theme, "llm_coded"),
      human = as_code(human_coded[[theme]][matched], theme, "human_coded"),
      theme = theme,
      conf_level = conf_level
    )
  })
  tibble::as_tibble(do.call(rbind, rows))
}

agreement_stats <- function(llm, human, theme, conf_level) {
  keep <- !is.na(llm) & !is.na(human)
  llm <- llm[keep]
  human <- human[keep]
  n <- length(llm)

  tp <- sum(llm & human)
  fp <- sum(llm & !human)
  fn <- sum(!llm & human)

  po <- if (n) mean(llm == human) else NA_real_
  pe <- if (n) mean(llm) * mean(human) + mean(!llm) * mean(!human) else NA_real_
  kappa <- se <- NA_real_
  if (n && pe < 1) {
    kappa <- (po - pe) / (1 - pe)
    se <- sqrt(po * (1 - po) / (n * (1 - pe)^2))
  }
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  precision <- if (tp + fp > 0) tp / (tp + fp) else NA_real_
  recall <- if (tp + fn > 0) tp / (tp + fn) else NA_real_
  f1 <- if (!is.na(precision) && !is.na(recall) && precision + recall > 0) {
    2 * precision * recall / (precision + recall)
  } else {
    NA_real_
  }

  data.frame(
    theme = theme,
    n = n,
    prevalence_llm = if (n) mean(llm) else NA_real_,
    prevalence_human = if (n) mean(human) else NA_real_,
    agreement = po,
    kappa = kappa,
    kappa_low = max(-1, kappa - z * se),
    kappa_high = min(1, kappa + z * se),
    precision = precision,
    recall = recall,
    f1 = f1
  )
}

as_code <- function(x, theme, arg, call = rlang::caller_env()) {
  if (is.logical(x)) {
    return(x)
  }
  if (is.numeric(x) && all(x %in% c(0, 1, NA))) {
    return(x == 1)
  }
  cli::cli_abort("Theme {.val {theme}} in {.arg {arg}} must be logical or 0/1.", call = call)
}
