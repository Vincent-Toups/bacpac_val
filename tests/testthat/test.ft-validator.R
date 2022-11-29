library(testthat)
library(val)
library(readr)
library(dplyr)

context("Test the FT validator.")

## NB - we encode our test data as a string to avoid the complexity of
## persisting it as a table of some kind.
## Here we read it back in.
## (Turns out read_csv doesn't read from textConnections)
## We use block here to create a temporary context so we don't
## make a mess with our tempnames
test_ft <- block({
    tempname <- tempfile();
    write(test_data$FT, file=tempname);
    val_read_csv(tempname);
});

test_that("Test that the FT validator just runs.",
          {
              result <- validate_ft(fresh_state(test_ft));
              expect_identical(result, result);
          });

test_that("Test that the FT validator produces zero errors on a good data set.",
          {
              result <- validate_ft(fresh_state(test_ft));
              expect_identical(0, sum(result$messages$pass == FALSE));
          });



