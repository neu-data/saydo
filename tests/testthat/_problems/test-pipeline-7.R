# Extracted from test-pipeline.R:7

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "saydo", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
codes <- c("UI_Confusion", "Positive_Experience", "Feature_Request")

# test -------------------------------------------------------------------------
set.seed(99)
before <- .Random.seed
a <- mock_mixed_data(20, seed = 1)
expect_identical(.Random.seed, before)
