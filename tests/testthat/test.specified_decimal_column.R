library(testthat)
library(val)
library(readr)
library(dplyr)

context("Text column format validation functions.")


dataset <- tibble(test_column=c(2.1,3.2,4.5,1.2),
                  unparsed__test_column=c("2.1","3.2","4.5","1.2"),
                  index__=seq(4));

state <- fresh_state(dataset, "ok");

test_that("Test basic specified decimal checks.",
          {
              expect_identical("ok",
                        column_is_specified_decimal("test_column","1.1")(state)$status);
          })



