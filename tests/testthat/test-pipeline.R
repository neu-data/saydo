codes <- c("UI_Confusion", "Positive_Experience", "Feature_Request")

test_that("mock_mixed_data is reproducible and leaves the global seed alone", {
  set.seed(99)
  before <- .Random.seed
  a <- mock_mixed_data(20, seed = 1)
  expect_identical(.Random.seed, before)
  expect_identical(a, mock_mixed_data(20, seed = 1))
  expect_named(a, c("user_id", "profile", "session_duration_sec",
                    "errors_encountered", "transcript_feedback"))
  expect_error(mock_mixed_data(0))
})

test_that("llm_auto_code adds one logical column per code", {
  df <- mock_mixed_data(30, seed = 2)
  coded <- llm_auto_code(df, user_id, transcript_feedback, codes, coder = mock_coder())
  expect_equal(nrow(coded), 30)
  for (code in codes) expect_type(coded[[code]], "logical")
  expect_equal(names(attr(coded, "saydo_codebook")), codes)
  expect_equal(attr(coded, "saydo_id"), "user_id")

  # string column names work too
  coded2 <- llm_auto_code(df, "user_id", "transcript_feedback", codes, coder = mock_coder())
  expect_equal(coded[codes], coded2[codes])
})

test_that("empty texts are NA and never sent to the coder", {
  df <- data.frame(id = 1:3, txt = c("I got lost", NA, "  "))
  seen <- NULL
  spy <- function(texts, codebook) {
    seen <<- texts
    mock_coder()(texts, codebook)
  }
  coded <- llm_auto_code(df, id, txt, "UI_Confusion", coder = spy)
  expect_equal(seen, "I got lost")
  expect_equal(coded$UI_Confusion, c(TRUE, NA, NA))
})

test_that("input validation gives clear errors", {
  df <- mock_mixed_data(5, seed = 3)
  expect_error(llm_auto_code(df, user_id, transcript_feedback, codes), "chat")
  expect_error(llm_auto_code(df, nope, transcript_feedback, codes, coder = mock_coder()), "not found")
  expect_error(llm_auto_code(df, user_id, transcript_feedback, "bad name", coder = mock_coder()), "syntactic")
  expect_error(llm_auto_code(df, user_id, transcript_feedback, "profile", coder = mock_coder()), "already exist")
  bad_coder <- function(texts, codebook) data.frame(x = 1)
  expect_error(llm_auto_code(df, user_id, transcript_feedback, codes, coder = bad_coder), "one row per text")
})

test_that("cross_tab_themes recovers the simulated effects", {
  df <- mock_mixed_data(200, seed = 42)
  coded <- llm_auto_code(df, user_id, transcript_feedback, codes, coder = mock_coder())
  res <- cross_tab_themes(coded, session_duration_sec)

  expect_equal(res$theme, codes)
  expect_equal(attr(res, "metric"), "session_duration_sec")
  confusion <- res[res$theme == "UI_Confusion", ]
  expect_true(confusion$significant)
  expect_gt(confusion$difference, 100)
  expect_lt(confusion$conf_low, confusion$difference)
  expect_false(res$significant[res$theme == "Feature_Request"])
})

test_that("cross_tab_themes matches a direct Welch t-test", {
  df <- mock_mixed_data(60, seed = 7)
  coded <- llm_auto_code(df, user_id, transcript_feedback, "UI_Confusion", coder = mock_coder())
  res <- cross_tab_themes(coded, session_duration_sec)
  tt <- t.test(coded$session_duration_sec[coded$UI_Confusion],
               coded$session_duration_sec[!coded$UI_Confusion])
  expect_equal(res$p_value, tt$p.value)
  expect_equal(c(res$conf_low, res$conf_high), as.numeric(tt$conf.int))
})

test_that("themes too rare to test get NA statistics", {
  df <- data.frame(id = 1:4, y = c(1, 2, 3, 4), A = c(TRUE, FALSE, FALSE, FALSE))
  res <- cross_tab_themes(df, y, themes = "A")
  expect_equal(res$n_present, 1)
  expect_true(is.na(res$p_value))
  expect_false(res$significant)
})

test_that("segment-level codes aggregate to participants before joining metrics", {
  segments <- data.frame(
    pid = c("a", "a", "b", "b", "c", "d", "e", "f"),
    text = c("fine", "I got lost", "fine", "ok", "so confusing", "great", "ok", "I got lost")
  )
  metrics <- data.frame(pid = c("a", "b", "c", "d", "e", "f"),
                        minutes = c(30, 10, 28, 12, 11, 33))
  coded <- llm_auto_code(segments, pid, text, "UI_Confusion", coder = mock_coder())
  res <- cross_tab_themes(coded, minutes, metrics = metrics)
  expect_equal(res$n_present, 3)
  expect_equal(res$n_absent, 3)
  expect_equal(res$mean_present, mean(c(30, 28, 33)))

  dup <- rbind(metrics, metrics[1, ])
  expect_error(cross_tab_themes(coded, minutes, metrics = dup), "one row per")
})

test_that("plot_theme_impact returns a ggplot", {
  df <- mock_mixed_data(80, seed = 42)
  coded <- llm_auto_code(df, user_id, transcript_feedback, codes, coder = mock_coder())
  p <- plot_theme_impact(cross_tab_themes(coded, session_duration_sec))
  expect_s3_class(p, "ggplot")
  expect_error(plot_theme_impact(data.frame(x = 1)), "cross_tab_themes")
})

test_that("a non-Chat object is rejected", {
  df <- mock_mixed_data(5, seed = 3)
  expect_error(llm_auto_code(df, user_id, transcript_feedback, codes, chat = "gpt"), "Chat")
})

test_that("a failing first LLM request stops the batch with a clear error", {
  calls <- 0
  fake <- structure(new.env(), class = "Chat")
  fake$clone <- function() fake
  fake$set_turns <- function(turns) invisible(fake)
  fake$get_system_prompt <- function() "sys"
  fake$chat_structured <- function(...) {
    calls <<- calls + 1
    stop("HTTP 429 Too Many Requests: no credits remaining")
  }
  df <- mock_mixed_data(5, seed = 3)
  expect_error(
    llm_auto_code(df, user_id, transcript_feedback, codes, chat = fake),
    "first LLM request failed"
  )
  expect_equal(calls, 1)
})

test_that("a single text is coded by the probe request alone", {
  fake <- structure(new.env(), class = "Chat")
  fake$clone <- function() fake
  fake$set_turns <- function(turns) invisible(fake)
  fake$get_system_prompt <- function() "sys"
  fake$chat_structured <- function(...) list(UI_Confusion = TRUE, Feature_Request = FALSE)
  df <- data.frame(id = 1, txt = "I got lost")
  coded <- llm_auto_code(df, id, txt, c("UI_Confusion", "Feature_Request"), chat = fake)
  expect_equal(coded$UI_Confusion, TRUE)
  expect_equal(coded$Feature_Request, FALSE)
})
