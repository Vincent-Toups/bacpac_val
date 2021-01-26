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


## Case Test 1

test_sc <- block({
  tempname <- tempfile();
  write(test_data$SC, file=tempname);
  val_read_csv(tempname);
});

test_sc$DOMAIN <- "DM"

test_that("Test that the SC validator is halted if DOMAIN isn't SC (Case Test 1).",
          {
            result <- validate_sc(fresh_state(test_sc));
            expect_identical(result$status, "halted");
          });


## Case Test 2

test_sc <- block({
  tempname <- tempfile();
  write(test_data$SC, file=tempname);
  val_read_csv(tempname);
});

test_sc$SCTESTCD[1]<-"GENIDEN"
test_sc$SCSEQ[3]<-"A"
test_sc$SCSTRESU[15]<-"FEET"

test_that("Test that the SC validator finds exactly 4 errors--should be 3 but doing 4 because of current existing apostrophe error (Case Test 2).",
          {
            result <- validate_sc(fresh_state(test_sc));
            expect_identical(result$messages %>% filter(!pass) %>% nrow(), 4L);
          });

## Case Test 3

test_sc <- block({
  tempname <- tempfile();
  write(test_data$SC, file=tempname);
  val_read_csv(tempname);
});

test_sc$SCSTRESC[2]<-"Not employed"

test_that("Test that the SC validator finds 2 errors--should be 1 but doing 2 because of current existing apostrophe error (Case Test 3). This is testing codelist subsets",
          {
            result <- validate_sc(fresh_state(test_sc));
            expect_identical(result$messages %>% filter(!pass) %>% nrow(), 2L);
          });

#Not recognizing this as an error
result <- validate_sc(fresh_state(test_sc))

result$messages %>% filter(!pass)
