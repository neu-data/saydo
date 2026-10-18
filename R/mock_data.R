#' Generate synthetic mixed-methods behavioural data
#'
#' Simulates users from three latent profiles (frustrated, efficient, casual)
#' with session metrics and a short feedback text whose wording depends on the
#' profile. Frustrated users have longer sessions and more errors; feature
#' requests are added at random, independent of profile, so they should show
#' no real effect. Useful for trying the package without an API key.
#'
#' @param n_users Number of users to simulate.
#' @param seed Optional random seed. The global random state is restored
#'   afterwards.
#'
#' @return A tibble with `user_id`, `profile` (the hidden ground truth),
#'   `session_duration_sec`, `errors_encountered` and `transcript_feedback`.
#' @export
#'
#' @examples
#' mock_mixed_data(5, seed = 1)
mock_mixed_data <- function(n_users = 50, seed = NULL) {
  if (!is.numeric(n_users) || length(n_users) != 1 || n_users < 1 || n_users %% 1 != 0) {
    cli::cli_abort("{.arg n_users} must be a single positive whole number.")
  }
  if (!is.null(seed)) {
    withr::local_preserve_seed()
    set.seed(seed)
  }

  texts <- list(
    frustrated = c(
      "I got completely lost trying to save my profile. The button vanished.",
      "Why is the checkout process so confusing? It kept throwing error codes.",
      "This interface is incredibly frustrating. It took me three tries to upload.",
      "The layout changed and now I can't find where my dashboard went."
    ),
    efficient = c(
      "Super fast load times, absolutely love the new minimal interface!",
      "The feature works exactly as expected. Seamless export process.",
      "Really clean UI. Navigating from the home screen was effortless.",
      "Extremely efficient workflow. Saved me a ton of time today."
    ),
    casual = c(
      "The application loaded normally and I viewed my recent activity history.",
      "Logged in, checked the system status, and closed the window.",
      "Standard performance. Nothing spectacular but it gets the job done.",
      "I opened the settings page to review my default notifications."
    )
  )
  requests <- c(
    "I wish there was a dark mode.",
    "Please add an option to export to Excel.",
    "It would be great to have keyboard shortcuts."
  )

  profile <- sample(names(texts), n_users, replace = TRUE)
  mean_sec <- c(frustrated = 450, efficient = 140, casual = 260)[profile]
  sd_sec <- c(frustrated = 60, efficient = 25, casual = 40)[profile]
  lambda <- c(frustrated = 8, efficient = 0.3, casual = 2)[profile]

  feedback <- vapply(profile, function(p) sample(texts[[p]], 1), character(1))
  add_request <- stats::runif(n_users) < 0.3
  feedback[add_request] <- paste(feedback[add_request],
                                 sample(requests, sum(add_request), replace = TRUE))

  tibble::tibble(
    user_id = sprintf("USR-%04d", 1000 + seq_len(n_users)),
    profile = unname(profile),
    session_duration_sec = pmax(10, round(stats::rnorm(n_users, mean_sec, sd_sec))),
    errors_encountered = stats::rpois(n_users, lambda),
    transcript_feedback = unname(feedback)
  )
}

#' A keyword-based stand-in coder for demos and tests
#'
#' Returns a coder function for the `coder` argument of [llm_auto_code()]
#' that tags texts by regular-expression matching instead of calling an LLM.
#' It is meant for trying the workflow offline with [mock_mixed_data()], not
#' for real research coding.
#'
#' @param patterns Named character vector of case-insensitive regular
#'   expressions, one per code. Codes without a pattern are coded `FALSE`.
#'
#' @return A function `function(texts, codebook)`.
#' @export
#'
#' @examples
#' coder <- mock_coder()
#' coder(c("I got lost", "I love it"), c(UI_Confusion = "", Positive_Experience = ""))
mock_coder <- function(patterns = c(
  UI_Confusion = "lost|confus|can't find|frustrat|error",
  Positive_Experience = "love|seamless|clean|effortless|efficient|fast",
  Feature_Request = "wish|please add|would be great"
)) {
  function(texts, codebook) {
    out <- lapply(names(codebook), function(code) {
      if (code %in% names(patterns)) {
        grepl(patterns[[code]], texts, ignore.case = TRUE)
      } else {
        rep(FALSE, length(texts))
      }
    })
    names(out) <- names(codebook)
    as.data.frame(out)
  }
}
