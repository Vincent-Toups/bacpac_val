library(testthat)
library(val)
library(readr)
library(dplyr)

context("Test the DM validator")

## NB - we encode our test data as a string to avoid the complexity of
## persisting it as a table of some kind.
## Here we read it back in.
## (Turns out read_csv doesn't read from textConnections)
## We use block here to create a temporary context so we don't
## make a mess with our tempnames
test_dm <- block({
  tempname <- tempfile();
  write(test_data$DM, file=tempname);
  val_read_csv(tempname);
});

test_that("Test that the DM validator just runs.",
          {
            result <- validate_dm(fresh_state(test_dm));
            expect_identical(result, result);
          });

test_that("Test that the DM validator is continuable on DM test data.",
          {
            result <- validate_dm(fresh_state(test_dm));
            expect_identical(result$status, "ok");
          });

test_that("Test that the DM validator finds exactly one test that does not pass on DM test data.",
          {
            result <- validate_dm(fresh_state(test_dm));
            expect_identical(result$messages %>% filter(!pass) %>% nrow(), 0L);
          });

## Case Test 1

test_dm <- block({
  tempname <- tempfile();
  write(test_data$DM, file=tempname);
  val_read_csv(tempname);
});

test_dm$DOMAIN <- "SC"

test_that("Test that the DM validator is halted if DOMAIN isn't DM (Case Test 1).",
          {
            result <- validate_dm(fresh_state(test_dm));
            expect_identical(result$status, "halted");
          });

## Case Test 2
## Prior belief is that the start date will be fine

test_dm <- block({
  tempname <- tempfile();
  write(test_data$DM, file=tempname);
  val_read_csv(tempname);
});

test_dm$DOMAIN[2] <- "SC"
test_dm$unparsed__RFSTDTC[2] <- "3020-01-01"
test_dm$AGE[2] <- -2
test_dm$SEX[2] <- "F"
test_dm$RACE[2] <- "Caucasian"
test_dm$ETHNIC[2] <- "Hispanic"

test_that("Test that the DM validator finds exactly 5 tests that do not pass on DM test data (Case Test 2).",
          {
            result <- validate_dm(fresh_state(test_dm));
            expect_identical(result$messages %>% filter(!pass) %>% nrow(), 5L);
          });

## Case Test 3

test_dm <- block({
  tempname <- tempfile();
  write(test_data$DM, file=tempname);
  val_read_csv(tempname);
});

test_dm$unparsed__RFSTDTC[3] <- "2020-01"
test_dm$AGE[3] <- 333
test_dm$SEX[3] <- "M"
test_dm$RACE[3] <- "W"
test_dm$ETHNIC[3] <- "Latino"

test_that("Test that the DM validator finds exactly 5 tests that do not pass on DM test data (Case Test 3).",
          {
            result <- validate_dm(fresh_state(test_dm));
            expect_identical(result$messages %>% filter(!pass) %>% nrow(), 5L);
          });

## Case Test 4

test_dm <- block({
  tempname <- tempfile();
  write(test_data$DM, file=tempname);
  val_read_csv(tempname);
});

test_dm$unparsed__RFSTDTC[4] <- "2020-13-01"
test_dm$AGE[4] <- 40.4
test_dm$SEX[4] <- "INTERSEX"
test_dm$RACE[4] <- "BLACK"
test_dm$ETHNIC[4] <- "UNKNOWN"

test_that("Test that the DM validator finds exactly 5 tests that do not pass on DM test data (Case Test 4).",
          {
            result <- validate_dm(fresh_state(test_dm));
            expect_identical(result$messages %>% filter(!pass) %>% nrow(), 5L);
          });

## Case Test 5
## Handling of multiple failures per column
## Reports all correctly formatted, yet invalid calendar dates

test_dm <- block({
  tempname <- tempfile();
  write(test_data$DM, file=tempname);
  val_read_csv(tempname);
});

test_dm$unparsed__RFSTDTC[5] <- "2020-02-30"
test_dm$unparsed__RFSTDTC[6] <- "9999-99-99"

test_dm$AGE[5] <- 555
test_dm$AGE[6] <- -66

test_dm$SEX[5] <- "unknown"
test_dm$SEX[6] <- "female"

test_dm$RACE[5] <- "asian"
test_dm$RACE[6] <- "Alaska Native"

test_dm$ETHNIC[5] <- "not reported"
test_dm$ETHNIC[6] <- "Not Hispanic"

test_that("Test that the DM validator finds exactly 6 tests that do not pass on DM test data (Case Test 5).",
          {
            result <- validate_dm(fresh_state(test_dm));
            expect_identical(result$messages %>% filter(!pass) %>% nrow(), 6L);
          });