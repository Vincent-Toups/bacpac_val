library(testthat)
library(val)
library(readr)
library(dplyr)

context("Test the EX validator")

## NB - we encode our test data as a string to avoid the complexity of
## persisting it as a table of some kind.
## Here we read it back in.
## (Turns out read_csv doesn't read from textConnections)
## We use block here to create a temporary context so we don't
## make a mess with our tempnames
test_ex <- block({
  tempname <- tempfile();
  write(test_data$EX, file=tempname);
  val_read_csv(tempname);
});

test_that("Test that the EX validator just runs.",
          {
            result <- validate_ex(fresh_state(test_ex));
            expect_identical(result, result);
          });
