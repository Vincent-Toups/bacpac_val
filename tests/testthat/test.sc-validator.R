library(testthat)
library(val)
library(readr)
library(dplyr)

context("Test the SC validator")

## NB - we encode our test data as a string to avoid the complexity of
## persisting it as a table of some kind.
## Here we read it back in.
## (Turns out read_csv doesn't read from textConnections)
## We use block here to create a temporary context so we don't
## make a mess with our tempnames
test_sc <- block({
  tempname <- tempfile();
  write(test_data$SC, file=tempname);
  val_read_csv(tempname);
});

test_that("Test that the SC validator just runs.",
          {
            result <- validate_sc(fresh_state(test_sc));
            expect_identical(result, result);
          });

test_that("Test that the SC validator returns zero errors on the test data set.",
          {
            result <- validate_sc(fresh_state(test_sc));
            expect_identical(0, sum(!result$messages$pass));
          });
